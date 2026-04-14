//! Relational Query (RQ) — dialect-independent relational algebra.
//!
//! The RQ sits between the Fossil IR (semantic: records, types, refs)
//! and SQL (concrete, DuckDB-specific). It represents data operations
//! as a pipeline of transforms over named tables and columns.
//!
//! Column expressions are stored as `sqlparser::ast::Expr` directly. This
//! matches DataFusion's `Unparser` pattern (DataFusion also builds sqlparser
//! AST nodes internally and uses their `Display` impl for emission). Building
//! a custom expression IR on top of sqlparser would duplicate the work
//! sqlparser already does — pretty printing, dialect awareness, AST visitors.
//!
//! Reference: PRQL's RQ, Ontop's IQ tree, Ibis's ops.Node DAG, DataFusion
//! `datafusion/sql/src/unparser/expr.rs`.

pub mod build;
pub mod to_ast;
pub mod lower;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use sqlparser::ast::Expr;

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
///
/// **Not serializable.** Holds `sqlparser::ast::Expr` which (without enabling
/// sqlparser's `serde` feature) does not implement `Serialize` / `Deserialize`.
/// Hosts that need to serialize compiled output should use `FossilPlan`,
/// which is serializable and embeds only the emitted SQL string.
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
        columns: Vec<(ColId, Expr)>,
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
        predicate: Expr,
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum JoinKind {
    Inner,
    Left,
}

/// Maps a table to an RDF entity type.
#[derive(Debug, Clone, PartialEq)]
pub struct EmissionDecl {
    pub table: TableId,
    pub type_name: String,
    pub subject_template: Expr,
    pub fields: Vec<(String, ColId)>,
    pub identity_columns: Vec<ColId>,
}

/// Output materialization instruction.
#[derive(Debug, Clone, PartialEq)]
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
