//! SQL dialect abstraction — pluggable backend for SQL generation.
//!
//! The compiler outputs a backend-agnostic `RelationalQuery`.
//! The host provides a `SqlDialect` that maps sources to SQL.
//!
//! Reference: PRQL target dispatch, F# Type Provider runtime split.

use crate::rq::ScanSource;

/// How the host should execute a source scan.
#[derive(Debug, Clone)]
pub enum ScanStrategy {
    /// Database reads the source directly. Contains the SQL expression.
    Sql(String),
    /// Host must preprocess first (e.g., PDF extraction), creating a temp table.
    Preprocess {
        handler: String,
        output_table: String,
    },
}

/// Backend-specific source resolution. Implemented by the host.
///
/// The compiler never calls this — it's used during plan generation
/// to convert the abstract `RelationalQuery` into a concrete `FossilPlan`.
pub trait SqlDialect {
    /// Decide how to execute a source. Returns SQL or a preprocessing instruction.
    fn scan_strategy(&self, source: &ScanSource) -> ScanStrategy;
}

/// Convention-based dialect: `read_{format}('{path}')`.
///
/// Works for most DuckDB formats out of the box (parquet, json, text, blob, avro, csv).
/// Override only for formats that break the convention (excel → read_xlsx, pdf → preprocess).
pub struct DefaultDialect;

impl SqlDialect for DefaultDialect {
    fn scan_strategy(&self, source: &ScanSource) -> ScanStrategy {
        let mut sql = format!("SELECT * FROM read_{}('{}'", source.format, source.path);
        for (key, value) in &source.params {
            sql.push_str(&format!(", {}={}", key, quote_param(value)));
        }
        sql.push(')');
        ScanStrategy::Sql(sql)
    }
}

fn quote_param(value: &str) -> String {
    match value {
        "true" | "false" => value.to_string(),
        v if v.parse::<i64>().is_ok() => v.to_string(),
        v => format!("'{v}'"),
    }
}
