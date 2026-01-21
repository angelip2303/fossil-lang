//! Streaming row iterator for SQL queries
//!
//! Provides cursor-based fetching with configurable batch size
//! for memory-efficient processing of large result sets.

use std::sync::Arc;

use polars::prelude::*;
use sqlx::any::AnyRow;
use sqlx::Row;

use super::connection::ConnectionPool;
use super::error::SqlError;
use super::schema::SqlSchema;

/// Execute a SQL query and return results as a DataFrame
///
/// This function fetches all rows and converts them to a polars DataFrame.
/// For memory efficiency with large datasets, consider using batch processing.
pub async fn execute_query_to_dataframe(
    pool: &ConnectionPool,
    query: &str,
    schema: &SqlSchema,
) -> Result<DataFrame, SqlError> {
    let rows: Vec<AnyRow> = sqlx::query(query)
        .fetch_all(&pool.pool)
        .await?;

    rows_to_dataframe(&rows, schema)
}

/// Execute a SQL query with LIMIT/OFFSET for batch processing
pub async fn execute_query_batch(
    pool: &ConnectionPool,
    query: &str,
    schema: &SqlSchema,
    limit: usize,
    offset: usize,
) -> Result<DataFrame, SqlError> {
    let batch_query = format!(
        "SELECT * FROM ({}) AS _batch LIMIT {} OFFSET {}",
        query, limit, offset
    );

    let rows: Vec<AnyRow> = sqlx::query(&batch_query)
        .fetch_all(&pool.pool)
        .await?;

    rows_to_dataframe(&rows, schema)
}

/// Convert SQLx rows to a polars DataFrame
fn rows_to_dataframe(rows: &[AnyRow], schema: &SqlSchema) -> Result<DataFrame, SqlError> {
    if rows.is_empty() {
        // Return empty DataFrame with correct schema
        let columns: Vec<polars::frame::column::Column> = schema
            .columns
            .iter()
            .map(|col| {
                Series::new_empty(col.name.clone().into(), &col.polars_dtype).into()
            })
            .collect();
        return DataFrame::new(columns).map_err(|e| SqlError::QueryError(e.to_string()));
    }

    // Build columns from rows
    let mut column_data: Vec<Vec<AnyValue>> = vec![Vec::with_capacity(rows.len()); schema.columns.len()];

    for row in rows {
        for (i, col_info) in schema.columns.iter().enumerate() {
            let value = extract_value(row, i, &col_info.polars_dtype);
            column_data[i].push(value);
        }
    }

    // Convert to Series
    let series: Vec<polars::frame::column::Column> = schema
        .columns
        .iter()
        .zip(column_data.iter())
        .map(|(col_info, values)| {
            values_to_series(&col_info.name, &col_info.polars_dtype, values)
        })
        .collect::<Result<Vec<_>, _>>()?;

    DataFrame::new(series).map_err(|e| SqlError::QueryError(e.to_string()))
}

/// Extract a value from a row at the given index
fn extract_value(row: &AnyRow, index: usize, dtype: &DataType) -> AnyValue<'static> {
    match dtype {
        DataType::Int64 => {
            match row.try_get::<i64, _>(index) {
                Ok(v) => AnyValue::Int64(v),
                Err(_) => match row.try_get::<i32, _>(index) {
                    Ok(v) => AnyValue::Int64(v as i64),
                    Err(_) => AnyValue::Null,
                },
            }
        }
        DataType::Float64 => {
            match row.try_get::<f64, _>(index) {
                Ok(v) => AnyValue::Float64(v),
                Err(_) => match row.try_get::<f32, _>(index) {
                    Ok(v) => AnyValue::Float64(v as f64),
                    Err(_) => AnyValue::Null,
                },
            }
        }
        DataType::Boolean => {
            match row.try_get::<bool, _>(index) {
                Ok(v) => AnyValue::Boolean(v),
                Err(_) => AnyValue::Null,
            }
        }
        DataType::String => {
            match row.try_get::<String, _>(index) {
                Ok(v) => AnyValue::StringOwned(v.into()),
                Err(_) => AnyValue::Null,
            }
        }
        _ => {
            // Default: try to get as string
            match row.try_get::<String, _>(index) {
                Ok(v) => AnyValue::StringOwned(v.into()),
                Err(_) => AnyValue::Null,
            }
        }
    }
}

/// Convert a vector of AnyValue to a Series
fn values_to_series(name: &str, dtype: &DataType, values: &[AnyValue]) -> Result<polars::frame::column::Column, SqlError> {
    let series = match dtype {
        DataType::Int64 => {
            let vals: Vec<Option<i64>> = values
                .iter()
                .map(|v| match v {
                    AnyValue::Int64(n) => Some(*n),
                    _ => None,
                })
                .collect();
            Series::new(name.into(), vals)
        }
        DataType::Float64 => {
            let vals: Vec<Option<f64>> = values
                .iter()
                .map(|v| match v {
                    AnyValue::Float64(n) => Some(*n),
                    _ => None,
                })
                .collect();
            Series::new(name.into(), vals)
        }
        DataType::Boolean => {
            let vals: Vec<Option<bool>> = values
                .iter()
                .map(|v| match v {
                    AnyValue::Boolean(b) => Some(*b),
                    _ => None,
                })
                .collect();
            Series::new(name.into(), vals)
        }
        DataType::String => {
            let vals: Vec<Option<String>> = values
                .iter()
                .map(|v| match v {
                    AnyValue::StringOwned(s) => Some(s.to_string()),
                    AnyValue::String(s) => Some(s.to_string()),
                    _ => None,
                })
                .collect();
            Series::new(name.into(), vals)
        }
        _ => {
            // Default: treat as string
            let vals: Vec<Option<String>> = values
                .iter()
                .map(|v| match v {
                    AnyValue::StringOwned(s) => Some(s.to_string()),
                    AnyValue::String(s) => Some(s.to_string()),
                    _ => Some(format!("{:?}", v)),
                })
                .collect();
            Series::new(name.into(), vals)
        }
    };

    Ok(series.into())
}

/// SQL Row iterator that fetches in batches
///
/// This struct manages cursor-based iteration over SQL results,
/// fetching `batch_size` rows at a time to maintain O(batch_size) memory usage.
pub struct SqlBatchIterator {
    pool: Arc<ConnectionPool>,
    query: String,
    schema: SqlSchema,
    batch_size: usize,
    current_offset: usize,
    exhausted: bool,
}

impl SqlBatchIterator {
    pub fn new(
        pool: Arc<ConnectionPool>,
        query: String,
        schema: SqlSchema,
        batch_size: usize,
    ) -> Self {
        Self {
            pool,
            query,
            schema,
            batch_size,
            current_offset: 0,
            exhausted: false,
        }
    }

    /// Fetch the next batch of rows
    pub async fn next_batch(&mut self) -> Result<Option<DataFrame>, SqlError> {
        if self.exhausted {
            return Ok(None);
        }

        let df = execute_query_batch(
            &self.pool,
            &self.query,
            &self.schema,
            self.batch_size,
            self.current_offset,
        ).await?;

        if df.height() == 0 {
            self.exhausted = true;
            return Ok(None);
        }

        if df.height() < self.batch_size {
            self.exhausted = true;
        }

        self.current_offset += df.height();
        Ok(Some(df))
    }

    /// Fetch all remaining rows as a single DataFrame
    pub async fn collect_all(&mut self) -> Result<DataFrame, SqlError> {
        let mut frames = Vec::new();

        while let Some(batch) = self.next_batch().await? {
            frames.push(batch);
        }

        if frames.is_empty() {
            // Return empty DataFrame with correct schema
            let columns: Vec<polars::frame::column::Column> = self
                .schema
                .columns
                .iter()
                .map(|col| {
                    Series::new_empty(col.name.clone().into(), &col.polars_dtype).into()
                })
                .collect();
            return DataFrame::new(columns).map_err(|e| SqlError::QueryError(e.to_string()));
        }

        // Vertically concatenate all frames
        let mut result = frames.remove(0);
        for frame in frames {
            result = result.vstack(&frame).map_err(|e| SqlError::QueryError(e.to_string()))?;
        }

        Ok(result)
    }
}
