//! Chunked executor — the only place where Plan is materialized into data.

use polars::prelude::*;

use super::lazy_frame::SafeLazyFrame;
use super::value::{Plan, Transform};

/// Executes Plan in chunks of configurable size
///
/// This is the ONLY component that can materialize a Plan.
/// It processes data in fixed-size batches, writing incrementally to output.
pub struct ChunkedExecutor {
    batch_size: usize,
}

impl ChunkedExecutor {
    /// Create a new executor with the specified batch size
    pub fn new(batch_size: usize) -> Self {
        Self { batch_size }
    }

    /// Execute a Plan with selection and process batches via callback
    ///
    /// This is the ONLY way to materialize a Plan. It processes data in
    /// fixed-size batches and calls the provided callback for each batch.
    /// Used for RDF serialization and other streaming outputs.
    ///
    /// # Arguments
    ///
    /// * `plan` - The Plan to execute
    /// * `select_exprs` - Selection expressions to apply (e.g., RDF field mapping)
    /// * `process_batch` - Callback called for each batch DataFrame
    ///
    /// # Returns
    ///
    /// Total number of rows processed
    pub fn execute_plan_with_select_batched<F>(
        &self,
        plan: &Plan,
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

    /// Execute a Plan without selection and process batches via callback
    ///
    /// Similar to `execute_plan_with_select_batched` but without a selection step.
    /// Used for multi-output processing where transformations are applied per-output.
    ///
    /// # Arguments
    ///
    /// * `plan` - The Plan to execute
    /// * `process_batch` - Callback called for each batch DataFrame
    ///
    /// # Returns
    ///
    /// Total number of rows processed
    pub fn execute_plan_batched<F>(&self, plan: &Plan, mut process_batch: F) -> PolarsResult<u64>
    where
        F: FnMut(DataFrame) -> PolarsResult<()>,
    {
        let safe_lf = self.build_safe_lazy_frame(plan)?;

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

    /// Build a SafeLazyFrame from a Plan
    ///
    /// PRIVATE: This is internal to the executor. No external code should
    /// be able to get a LazyFrame from a Plan.
    fn build_safe_lazy_frame(&self, plan: &Plan) -> PolarsResult<SafeLazyFrame> {
        // Build LazyFrame from source (or empty if None)
        let mut safe_lf = match &plan.source {
            Some(src) => SafeLazyFrame::new(src.to_lazy_frame()?),
            None => SafeLazyFrame::new(LazyFrame::default()),
        };

        // Apply all transforms
        for transform in &plan.transforms {
            safe_lf = Self::apply_transform(safe_lf, transform)?;
        }

        Ok(safe_lf)
    }

    /// Apply a transform to a SafeLazyFrame
    fn apply_transform(safe_lf: SafeLazyFrame, transform: &Transform) -> PolarsResult<SafeLazyFrame> {
        match transform {
            Transform::Select(exprs) => Ok(safe_lf.select(exprs.clone())),
            Transform::Filter(expr) => Ok(safe_lf.filter(expr.clone())),
            Transform::Join(join) => {
                let mut right_lf = match &join.right_source {
                    Some(src) => SafeLazyFrame::new(src.to_lazy_frame()?),
                    None => SafeLazyFrame::new(LazyFrame::default()),
                };
                for t in &join.right_transforms {
                    right_lf = Self::apply_transform(right_lf, t)?;
                }

                let args = JoinArgs::new(JoinType::Inner)
                    .with_suffix(Some(join.suffix.clone().into()));

                Ok(safe_lf.join(right_lf, join.left_on.clone(), join.right_on.clone(), args))
            }
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

pub fn estimate_batch_size_from_plan(plan: &Plan) -> usize {
    estimate_batch_size(&plan.schema)
}

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
