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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RqExpr {
    Col(ColId),
    Lit(RqLiteral),
    Coalesce(Box<RqExpr>, Box<RqExpr>),
    Concat(Vec<RqExpr>),
    Cast(Box<RqExpr>, String),
    Func { name: String, args: Vec<RqExpr> },
    IsNull(Box<RqExpr>, bool),
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
