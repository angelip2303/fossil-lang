//! Chunked executor for memory-efficient plan execution
//!
//! This is the ONLY place where RecordsPlan is materialized into actual data.
//! All execution goes through this module, ensuring constant memory usage
//! regardless of dataset size.
//!
//! # Lazy Evaluation Guarantee
//!
//! This module uses `SafeLazyFrame` internally to build transformation pipelines.
//! The `.collect()` method is only called here, in `execute_plan_with_select_batched`,
//! through `SafeLazyFrame::into_inner()`. External code cannot accidentally
//! materialize data because `SafeLazyFrame` does not expose `.collect()`.

use polars::prelude::*;

use super::lazy_frame::SafeLazyFrame;
use super::value::{RecordsPlan, SourceDescriptor, Transform};

/// Executes RecordsPlan in chunks of configurable size
///
/// This is the ONLY component that can materialize a RecordsPlan.
/// It processes data in fixed-size batches, writing incrementally to output.
pub struct ChunkedExecutor {
    batch_size: usize,
}

impl ChunkedExecutor {
    /// Create a new executor with the specified batch size
    pub fn new(batch_size: usize) -> Self {
        Self { batch_size }
    }

    /// Execute a RecordsPlan with selection and process batches via callback
    ///
    /// This is the ONLY way to materialize a RecordsPlan. It processes data in
    /// fixed-size batches and calls the provided callback for each batch.
    /// Used for RDF serialization and other streaming outputs.
    ///
    /// # Arguments
    ///
    /// * `plan` - The RecordsPlan to execute
    /// * `select_exprs` - Selection expressions to apply (e.g., RDF field mapping)
    /// * `process_batch` - Callback called for each batch DataFrame
    ///
    /// # Returns
    ///
    /// Total number of rows processed
    pub fn execute_plan_with_select_batched<F>(
        &self,
        plan: &RecordsPlan,
        select_exprs: Vec<Expr>,
        mut process_batch: F,
    ) -> PolarsResult<u64>
    where
        F: FnMut(DataFrame) -> PolarsResult<()>,
    {
        let mut safe_lf = self.build_safe_lazy_frame(plan)?;
        if !select_exprs.is_empty() {
            safe_lf = safe_lf.select(select_exprs);
        }

        // Convert to LazyFrame for batched execution
        let lf = safe_lf.into_inner();
        let mut total_rows: u64 = 0;
        let mut offset: i64 = 0;

        loop {
            // Get a batch using slice - THIS is where materialization happens
            let batch_lf = lf.clone().slice(offset, self.batch_size as u32);
            let batch_df = batch_lf.collect()?;

            let batch_len = batch_df.height();
            if batch_len == 0 {
                break;
            }

            // Call the callback with this batch
            process_batch(batch_df)?;

            total_rows += batch_len as u64;
            offset += batch_len as i64;

            // If we got fewer rows than requested, we're done
            if batch_len < self.batch_size {
                break;
            }
        }

        Ok(total_rows)
    }

    /// Build a SafeLazyFrame from a RecordsPlan
    ///
    /// PRIVATE: This is internal to the executor. No external code should
    /// be able to get a LazyFrame from a RecordsPlan.
    fn build_safe_lazy_frame(&self, plan: &RecordsPlan) -> PolarsResult<SafeLazyFrame> {
        let mut safe_lf = self.build_source_safe_lazy_frame(&plan.source)?;

        // Apply all transforms
        for transform in &plan.transforms {
            safe_lf = Self::apply_transform(safe_lf, transform);
        }

        Ok(safe_lf)
    }

    /// Build a SafeLazyFrame from a SourceDescriptor
    fn build_source_safe_lazy_frame(&self, source: &SourceDescriptor) -> PolarsResult<SafeLazyFrame> {
        match source {
            SourceDescriptor::Csv {
                path,
                delimiter,
                has_header,
            } => {
                let lf = LazyCsvReader::new(PlPath::from_str(path))
                    .with_separator(*delimiter)
                    .with_has_header(*has_header)
                    .finish()?;
                Ok(SafeLazyFrame::new(lf))
            }

            SourceDescriptor::Parquet { path } => {
                let lf = LazyFrame::scan_parquet(PlPath::from_str(path), Default::default())?;
                Ok(SafeLazyFrame::new(lf))
            }

            SourceDescriptor::InMemory(df) => Ok(SafeLazyFrame::new(df.as_ref().clone().lazy())),

            SourceDescriptor::Empty => Ok(SafeLazyFrame::new(LazyFrame::default())),

            SourceDescriptor::Pending { .. } => Err(PolarsError::ComputeError(
                "Cannot execute a pending transformation - it must be applied to a source first"
                    .into(),
            )),

            SourceDescriptor::Concat(plans) => {
                // For concat, we need to extract the inner LazyFrames temporarily
                let lfs: PolarsResult<Vec<LazyFrame>> = plans
                    .iter()
                    .map(|p| self.build_safe_lazy_frame(p).map(|slf| slf.into_inner()))
                    .collect();
                let concatenated = concat(lfs?, UnionArgs::default())?;
                Ok(SafeLazyFrame::new(concatenated))
            }

            SourceDescriptor::Join {
                left,
                right,
                left_on,
                right_on,
            } => {
                let left_lf = self.build_safe_lazy_frame(left)?;
                let right_lf = self.build_safe_lazy_frame(right)?;
                Ok(left_lf.inner_join(right_lf, col(left_on), col(right_on)))
            }
        }
    }

    /// Apply a transform to a SafeLazyFrame
    fn apply_transform(safe_lf: SafeLazyFrame, transform: &Transform) -> SafeLazyFrame {
        match transform {
            Transform::Select(exprs) => safe_lf.select(exprs.clone()),
            Transform::Filter(expr) => safe_lf.filter(expr.clone()),
            Transform::WithColumn(expr) => safe_lf.with_column(expr.clone()),
        }
    }
}

/// Estimate optimal batch size based on schema
///
/// Targets approximately 100MB per batch for balanced memory/performance.
pub fn estimate_batch_size(schema: &Schema) -> usize {
    let row_bytes: usize = schema
        .iter()
        .map(|(_, dtype)| estimate_dtype_size(dtype))
        .sum();

    // Target ~100MB per batch
    const TARGET_BYTES: usize = 100 * 1024 * 1024;

    // Clamp between 10K and 500K rows
    (TARGET_BYTES / row_bytes.max(1)).clamp(10_000, 500_000)
}

/// Estimate optimal batch size from a RecordsPlan
pub fn estimate_batch_size_from_plan(plan: &RecordsPlan) -> usize {
    estimate_batch_size(&plan.schema)
}

/// Estimate average memory size for a data type
fn estimate_dtype_size(dtype: &DataType) -> usize {
    match dtype {
        DataType::Boolean => 1,
        DataType::Int8 | DataType::UInt8 => 1,
        DataType::Int16 | DataType::UInt16 => 2,
        DataType::Int32 | DataType::UInt32 | DataType::Float32 => 4,
        DataType::Int64 | DataType::UInt64 | DataType::Float64 => 8,
        DataType::Date => 4,
        DataType::Datetime(_, _) | DataType::Duration(_) | DataType::Time => 8,
        DataType::String => 64, // Conservative estimate for variable-length strings
        DataType::Binary => 128,
        DataType::List(inner) => 8 + estimate_dtype_size(inner) * 10, // Pointer + avg 10 elements
        DataType::Struct(fields) => fields.iter().map(|f| estimate_dtype_size(f.dtype())).sum(),
        DataType::Null => 0,
        _ => 32, // Conservative default for other types
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_estimate_batch_size() {
        // Schema with small types should give larger batches
        let small_schema = Schema::from_iter([
            Field::new("a".into(), DataType::Int32),
            Field::new("b".into(), DataType::Int32),
        ]);
        let small_batch = estimate_batch_size(&small_schema);
        // Should be clamped to max (500K) since 8 bytes per row means ~12M rows for 100MB
        assert_eq!(small_batch, 500_000);

        // Schema with many strings should give smaller batches
        let string_schema = Schema::from_iter([
            Field::new("a".into(), DataType::String),
            Field::new("b".into(), DataType::String),
            Field::new("c".into(), DataType::String),
            Field::new("d".into(), DataType::String),
            Field::new("e".into(), DataType::String),
        ]);
        let string_batch = estimate_batch_size(&string_schema);
        // 5 * 64 = 320 bytes per row, 100MB / 320 = ~327K rows
        assert!(string_batch > 100_000 && string_batch < 500_000);
    }

    #[test]
    fn test_estimate_dtype_size() {
        assert_eq!(estimate_dtype_size(&DataType::Int32), 4);
        assert_eq!(estimate_dtype_size(&DataType::Int64), 8);
        assert_eq!(estimate_dtype_size(&DataType::String), 64);
        assert_eq!(estimate_dtype_size(&DataType::Boolean), 1);
    }
}
