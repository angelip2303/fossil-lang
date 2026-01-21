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
    db_type: DatabaseType,
) -> Result<SqlSchema, SqlError> {
    // For SQLite, use PRAGMA table_info
    if db_type == DatabaseType::SQLite {
        return infer_sqlite_table_schema(pool, table_name).await;
    }

    // For other databases, use a LIMIT 1 query to get column info
    let query = format!("SELECT * FROM {} LIMIT 1", table_name);
    infer_query_schema(pool, &query).await
}

/// Infer schema from SQLite table using PRAGMA
async fn infer_sqlite_table_schema(
    pool: &ConnectionPool,
    table_name: &str,
) -> Result<SqlSchema, SqlError> {
    let pragma_query = format!("PRAGMA table_info({})", table_name);
    eprintln!("[SQL Schema] SQLite PRAGMA query: {}", pragma_query);

    let rows: Vec<AnyRow> = sqlx::query(&pragma_query)
        .fetch_all(&pool.pool)
        .await
        .map_err(|e| SqlError::QueryError(e.to_string()))?;

    eprintln!("[SQL Schema] PRAGMA returned {} rows", rows.len());

    if rows.is_empty() {
        return Err(SqlError::SchemaInferenceError(format!(
            "Table '{}' not found or has no columns",
            table_name
        )));
    }

    let columns: Vec<ColumnInfo> = rows
        .iter()
        .map(|row| {
            // PRAGMA table_info returns: cid, name, type, notnull, dflt_value, pk
            let name: String = row.try_get(1).unwrap_or_default();
            let sql_type: String = row.try_get(2).unwrap_or_default();
            let polars_dtype = sql_type_to_polars(&sql_type);
            eprintln!("[SQL Schema] Column: {} ({})", name, sql_type);
            ColumnInfo {
                name,
                sql_type,
                polars_dtype,
            }
        })
        .collect();

    Ok(SqlSchema { columns })
}

/// Infer schema from a query using LIMIT 1
async fn infer_query_schema(pool: &ConnectionPool, query: &str) -> Result<SqlSchema, SqlError> {
    // Wrap query in a subquery with LIMIT 1 to get schema with minimal data
    let schema_query = format!("SELECT * FROM ({}) AS _schema_query LIMIT 1", query);

    eprintln!("[SQL Schema] Query: {}", schema_query);

    let row_opt: Option<AnyRow> = match sqlx::query(&schema_query)
        .fetch_optional(&pool.pool)
        .await {
            Ok(r) => {
                eprintln!("[SQL Schema] fetch_optional succeeded, row present: {}", r.is_some());
                r
            }
            Err(e) => {
                eprintln!("[SQL Schema] fetch_optional failed: {}", e);
                return Err(SqlError::QueryError(e.to_string()));
            }
        };

    // If we got a row, extract column info from it
    if let Some(row) = row_opt {
        let columns: Vec<ColumnInfo> = row
            .columns()
            .iter()
            .map(|col| {
                let name = col.name().to_string();
                let type_info = col.type_info();
                let sql_type = type_info.name().to_string();
                let polars_dtype = sql_type_to_polars(&sql_type);
                eprintln!("[SQL Schema] Column from query: {} ({})", name, sql_type);
                ColumnInfo {
                    name,
                    sql_type,
                    polars_dtype,
                }
            })
            .collect();

        Ok(SqlSchema { columns })
    } else {
        // No rows in result - query returned empty set
        Err(SqlError::SchemaInferenceError(
            "Query returned no rows, cannot infer schema".to_string(),
        ))
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
