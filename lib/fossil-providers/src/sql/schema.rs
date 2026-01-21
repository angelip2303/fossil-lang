//! Schema inference from SQL databases

use polars::prelude::*;
use sqlx::any::AnyRow;
use sqlx::{Column, Row, TypeInfo};

use super::config::{DatabaseType, SqlConfig, SqlSource};
use super::connection::ConnectionPool;
use super::error::SqlError;

/// Column metadata inferred from database
#[derive(Debug, Clone)]
pub struct ColumnInfo {
    pub name: String,
    pub sql_type: String,
    pub polars_dtype: DataType,
}

/// Schema inferred from database
#[derive(Debug, Clone)]
pub struct SqlSchema {
    pub columns: Vec<ColumnInfo>,
}

impl SqlSchema {
    /// Convert to polars Schema
    pub fn to_polars_schema(&self) -> Schema {
        let fields: Vec<Field> = self
            .columns
            .iter()
            .map(|col| Field::new(col.name.clone().into(), col.polars_dtype.clone()))
            .collect();
        Schema::from_iter(fields)
    }
}

/// Infer schema from database
///
/// This function connects to the database and infers the schema either by:
/// - Querying the information_schema for table sources
/// - Executing a LIMIT 0 query for custom queries
pub async fn infer_schema(config: &SqlConfig) -> Result<SqlSchema, SqlError> {
    let pool = ConnectionPool::get_or_create(&config.connection_string, config.connect_timeout).await?;

    match &config.source {
        SqlSource::Table(table_name) => infer_table_schema(&pool, table_name, config.db_type).await,
        SqlSource::Query(query) => infer_query_schema(&pool, query).await,
    }
}

/// Infer schema from a table using information_schema
async fn infer_table_schema(
    pool: &ConnectionPool,
    table_name: &str,
    _db_type: DatabaseType,
) -> Result<SqlSchema, SqlError> {
    // Use a LIMIT 0 query to get column info without fetching data
    let query = format!("SELECT * FROM {} LIMIT 0", table_name);
    infer_query_schema(pool, &query).await
}

/// Infer schema from a query using LIMIT 0
async fn infer_query_schema(pool: &ConnectionPool, query: &str) -> Result<SqlSchema, SqlError> {
    // Wrap query in a subquery with LIMIT 0 to get schema without data
    let schema_query = format!("SELECT * FROM ({}) AS _schema_query LIMIT 0", query);

    let row_opt: Option<AnyRow> = sqlx::query(&schema_query)
        .fetch_optional(&pool.pool)
        .await?;

    // If we got a row, extract column info from it
    // If not, we need to describe the query another way
    if let Some(row) = row_opt {
        let columns: Vec<ColumnInfo> = row
            .columns()
            .iter()
            .map(|col| {
                let name = col.name().to_string();
                let type_info = col.type_info();
                let sql_type = type_info.name().to_string();
                let polars_dtype = sql_type_to_polars(&sql_type);
                ColumnInfo {
                    name,
                    sql_type,
                    polars_dtype,
                }
            })
            .collect();

        Ok(SqlSchema { columns })
    } else {
        // No row returned, but we can still get column info from the query result
        // For this, we need to execute the query and inspect the columns
        let rows: Vec<AnyRow> = sqlx::query(&schema_query)
            .fetch_all(&pool.pool)
            .await?;

        // Even with 0 rows, SQLx gives us column metadata
        if rows.is_empty() {
            // Execute query directly to get metadata
            let result = sqlx::query(&schema_query)
                .fetch_one(&pool.pool)
                .await;

            match result {
                Ok(row) => {
                    let columns: Vec<ColumnInfo> = row
                        .columns()
                        .iter()
                        .map(|col| {
                            let name = col.name().to_string();
                            let type_info = col.type_info();
                            let sql_type = type_info.name().to_string();
                            let polars_dtype = sql_type_to_polars(&sql_type);
                            ColumnInfo {
                                name,
                                sql_type,
                                polars_dtype,
                            }
                        })
                        .collect();
                    Ok(SqlSchema { columns })
                }
                Err(_) => {
                    // As a fallback, describe the original query
                    Err(SqlError::SchemaInferenceError(
                        "Unable to infer schema from query".to_string(),
                    ))
                }
            }
        } else {
            let row = &rows[0];
            let columns: Vec<ColumnInfo> = row
                .columns()
                .iter()
                .map(|col| {
                    let name = col.name().to_string();
                    let type_info = col.type_info();
                    let sql_type = type_info.name().to_string();
                    let polars_dtype = sql_type_to_polars(&sql_type);
                    ColumnInfo {
                        name,
                        sql_type,
                        polars_dtype,
                    }
                })
                .collect();

            Ok(SqlSchema { columns })
        }
    }
}

/// Map SQL type names to Polars DataType
fn sql_type_to_polars(sql_type: &str) -> DataType {
    let upper = sql_type.to_uppercase();

    // Integer types
    if upper.contains("INT") {
        return DataType::Int64;
    }

    // Boolean types
    if upper.contains("BOOL") {
        return DataType::Boolean;
    }

    // Floating point types
    if upper.contains("REAL") || upper.contains("FLOAT") || upper.contains("DOUBLE") || upper.contains("NUMERIC") || upper.contains("DECIMAL") {
        return DataType::Float64;
    }

    // Text types
    if upper.contains("TEXT") || upper.contains("VARCHAR") || upper.contains("CHAR") || upper.contains("STRING") {
        return DataType::String;
    }

    // Date/time types
    if upper.contains("TIMESTAMP") || upper.contains("DATETIME") {
        return DataType::Datetime(TimeUnit::Microseconds, None);
    }
    if upper.contains("DATE") {
        return DataType::Date;
    }
    if upper.contains("TIME") {
        return DataType::Time;
    }

    // Binary types
    if upper.contains("BLOB") || upper.contains("BYTEA") || upper.contains("BINARY") {
        return DataType::Binary;
    }

    // Default to string for unknown types
    DataType::String
}
