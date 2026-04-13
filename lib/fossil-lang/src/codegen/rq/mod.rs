//! Relational Query (RQ) — dialect-independent relational algebra.
//!
//! The RQ sits between the Fossil IR (semantic: records, types, refs)
//! and SQL (concrete, DuckDB-specific). It represents data operations
//! as a pipeline of transforms over named tables and columns.
//!
//! Reference: PRQL's RQ, Ontop's IQ tree, Ibis's ops.Node DAG.

pub mod emit_sql;
pub mod lower;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

/// Unique table identifier within a RelationalQuery.
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct TableId(pub usize);

/// Unique column identifier within a RelationalQuery.
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct ColId(pub usize);

/// A complete relational query produced by lowering the Fossil IR.
///
/// The pipeline is linear: transforms execute in order, each consuming
/// input table(s) and producing an output table.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RelationalQuery {
    /// Ordered pipeline of transforms.
    pub transforms: Vec<Transform>,
    /// Column name registry (ColId → name).
    pub columns: Vec<String>,
    /// Table name registry (TableId → name).
    pub tables: Vec<String>,
    /// Which tables map to RDF entity types.
    pub emissions: Vec<EmissionDecl>,
    /// Output materialization instructions.
    pub outputs: Vec<OutputDecl>,
}

/// A single relational operation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Transform {
    /// Load data from source.
    Scan {
        output: TableId,
        source: ScanSource,
    },
    /// SELECT columns FROM input.
    Project {
        input: TableId,
        output: TableId,
        columns: Vec<(ColId, RqExpr)>,
    },
    /// JOIN two tables.
    Join {
        left: TableId,
        right: TableId,
        output: TableId,
        on: Vec<(ColId, ColId)>,
        kind: JoinKind,
        suffix: Option<String>,
    },
    /// WHERE predicate.
    Filter {
        input: TableId,
        output: TableId,
        predicate: RqExpr,
    },
    /// Apply attribute transforms (#[clean], #[anon]).
    ApplyTransforms {
        input: TableId,
        output: TableId,
        ops: Vec<(ColId, String)>, // (column, sql_expression)
    },
}

/// External data source in the RQ. Backend-agnostic.
/// The `SqlDialect` decides how to execute it (direct SQL, preprocessing, etc.).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScanSource {
    pub format: String,
    pub path: String,
    pub params: HashMap<String, String>,
}

/// Dialect-independent column expression.
///
/// Backend-independent: no SQL strings embedded. `Cast` uses `LogicalType`,
/// `Func` uses `RqFn` enum. The SQL dialect (DuckDB, Postgres, etc.) translates
/// these to its native types/functions during `emit_sql`.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RqExpr {
    Col(ColId),
    Lit(RqLiteral),
    Coalesce(Box<RqExpr>, Box<RqExpr>),
    Concat(Vec<RqExpr>),
    /// Type cast to a logical type. The SQL dialect maps `LogicalType::String`
    /// to "VARCHAR" (DuckDB) or "TEXT" (Postgres), etc.
    Cast(Box<RqExpr>, LogicalType),
    /// Function call. Backend-independent via `RqFn` enum.
    Func { name: RqFn, args: Vec<RqExpr> },
    IsNull(Box<RqExpr>, bool),
}

/// Backend-independent logical types. Each dialect maps these to its own
/// SQL type names during emission.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LogicalType {
    String,
    Int,
    Float,
    Bool,
    Date,
    Timestamp,
    /// Escape hatch for dialect-specific types not yet in the enum.
    /// Carries the SQL string verbatim (only safe when the dialect is known).
    Custom(String),
}

impl LogicalType {
    /// Default mapping to SQL type name (DuckDB conventions).
    /// Individual backends may override via dialect-specific emit logic.
    pub fn to_duckdb_type(&self) -> String {
        match self {
            LogicalType::String => "VARCHAR".to_string(),
            LogicalType::Int => "BIGINT".to_string(),
            LogicalType::Float => "DOUBLE".to_string(),
            LogicalType::Bool => "BOOLEAN".to_string(),
            LogicalType::Date => "DATE".to_string(),
            LogicalType::Timestamp => "TIMESTAMP".to_string(),
            LogicalType::Custom(s) => s.clone(),
        }
    }

    /// Parse a SQL type name into a `LogicalType` (best-effort, falls back to
    /// `Custom` for unknown types). Inverse of `to_duckdb_type`.
    pub fn from_sql_type(s: &str) -> Self {
        let upper = s.to_uppercase();
        if upper.contains("VARCHAR") || upper == "TEXT" || upper == "STRING" {
            LogicalType::String
        } else if upper.contains("INT") || upper == "BIGINT" || upper == "INTEGER" {
            LogicalType::Int
        } else if upper.contains("DOUBLE") || upper.contains("FLOAT") || upper.contains("REAL") || upper.contains("DECIMAL") {
            LogicalType::Float
        } else if upper.contains("BOOL") {
            LogicalType::Bool
        } else if upper == "DATE" {
            LogicalType::Date
        } else if upper.contains("TIMESTAMP") || upper == "DATETIME" {
            LogicalType::Timestamp
        } else {
            LogicalType::Custom(s.to_string())
        }
    }
}

/// Backend-independent function identifier. Each dialect translates these
/// to its native SQL function during emission.
///
/// Uses explicit variants for well-known functions (compile-time checked by
/// each backend) and `Custom` as escape hatch for dialect-specific functions.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RqFn {
    /// Cryptographic SHA-256 hash (used by `#[anon(hash)]`).
    Sha256,
    /// String trimming (used by `#[clean(trim)]`).
    Trim,
    /// Lowercase conversion.
    Lower,
    /// Uppercase conversion.
    Upper,
    /// Substring extraction: `Substring(str, start, length)`.
    Substring,
    /// String length.
    Length,
    /// Regex replacement: `RegexpReplace(str, pattern, replacement, flags)`.
    RegexpReplace,
    /// NULLIF: replace value with NULL if it equals a sentinel.
    NullIf,
    /// GREATEST: max of arguments.
    Greatest,
    /// LEAST: min of arguments.
    Least,
    /// REPLACE: string replacement.
    Replace,
    /// Escape hatch for dialect-specific functions not yet enumerated.
    /// Carries the function name verbatim.
    Custom(String),
}

impl RqFn {
    /// Default SQL function name (matches DuckDB / standard SQL).
    /// Individual backends may override during emission.
    pub fn to_sql_name(&self) -> &str {
        match self {
            RqFn::Sha256 => "SHA256",
            RqFn::Trim => "TRIM",
            RqFn::Lower => "LOWER",
            RqFn::Upper => "UPPER",
            RqFn::Substring => "SUBSTRING",
            RqFn::Length => "LENGTH",
            RqFn::RegexpReplace => "REGEXP_REPLACE",
            RqFn::NullIf => "NULLIF",
            RqFn::Greatest => "GREATEST",
            RqFn::Least => "LEAST",
            RqFn::Replace => "REPLACE",
            RqFn::Custom(name) => name.as_str(),
        }
    }

    /// Parse a SQL function name into an `RqFn` (falls back to `Custom`).
    pub fn from_sql_name(s: &str) -> Self {
        match s.to_uppercase().as_str() {
            "SHA256" => RqFn::Sha256,
            "TRIM" => RqFn::Trim,
            "LOWER" => RqFn::Lower,
            "UPPER" => RqFn::Upper,
            "SUBSTRING" | "SUBSTR" => RqFn::Substring,
            "LENGTH" | "LEN" => RqFn::Length,
            "REGEXP_REPLACE" => RqFn::RegexpReplace,
            "NULLIF" => RqFn::NullIf,
            "GREATEST" => RqFn::Greatest,
            "LEAST" => RqFn::Least,
            "REPLACE" => RqFn::Replace,
            _ => RqFn::Custom(s.to_string()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RqLiteral {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum JoinKind {
    Inner,
    Left,
}

/// Maps a table to an RDF entity type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EmissionDecl {
    pub table: TableId,
    pub type_name: String,
    pub subject_template: RqExpr,
    pub fields: Vec<(String, ColId)>,
    pub identity_columns: Vec<ColId>,
}

/// Output materialization instruction.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OutputDecl {
    pub emissions: Vec<usize>,
    pub format: String,
    pub path: String,
    pub params: HashMap<String, String>,
}

// ── Builder helpers ──────────────────────────────────────────────────

impl RelationalQuery {
    pub fn new() -> Self {
        Self {
            transforms: Vec::new(),
            columns: Vec::new(),
            tables: Vec::new(),
            emissions: Vec::new(),
            outputs: Vec::new(),
        }
    }

    /// Intern a column name, returning its ColId.
    pub fn intern_col(&mut self, name: &str) -> ColId {
        if let Some(idx) = self.columns.iter().position(|c| c == name) {
            ColId(idx)
        } else {
            let id = ColId(self.columns.len());
            self.columns.push(name.to_string());
            id
        }
    }

    /// Allocate a new table name, returning its TableId.
    pub fn alloc_table(&mut self, name: &str) -> TableId {
        let id = TableId(self.tables.len());
        self.tables.push(name.to_string());
        id
    }

    pub fn col_name(&self, id: ColId) -> &str {
        &self.columns[id.0]
    }

    pub fn table_name(&self, id: TableId) -> &str {
        &self.tables[id.0]
    }
}

impl Default for RelationalQuery {
    fn default() -> Self {
        Self::new()
    }
}
