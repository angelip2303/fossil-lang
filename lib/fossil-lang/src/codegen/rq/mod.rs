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
use sqlparser::ast::{Expr, Ident};

/// A complete relational query produced by lowering the Fossil IR.
///
/// The pipeline is linear: transforms execute in order, each consuming
/// input table(s) and producing an output table. Tables and columns are
/// referenced by `sqlparser::ast::Ident` directly — the same type sqlparser
/// uses to emit SQL — so there is no parallel name registry to maintain.
///
/// **Not serializable.** Holds `sqlparser::ast::Expr` which (without enabling
/// sqlparser's `serde` feature) does not implement `Serialize` / `Deserialize`.
/// Hosts that need to serialize compiled output should use `FossilPlan`,
/// which is serializable and embeds only the emitted SQL string.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RelationalQuery {
    /// Ordered pipeline of transforms.
    pub transforms: Vec<Transform>,
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
        output: Ident,
        source: ScanSource,
    },
    /// SELECT columns FROM input.
    Project {
        input: Ident,
        output: Ident,
        columns: Vec<(Ident, Expr)>,
    },
    /// JOIN two tables.
    Join {
        left: Ident,
        right: Ident,
        output: Ident,
        on: Vec<(Ident, Ident)>,
        kind: JoinKind,
        suffix: Option<String>,
    },
    /// WHERE predicate.
    Filter {
        input: Ident,
        output: Ident,
        predicate: Expr,
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
    pub table: Ident,
    pub type_name: String,
    pub subject_template: Expr,
    pub fields: Vec<(String, Ident)>,
    pub identity_columns: Vec<Ident>,
}

/// Output materialization instruction.
#[derive(Debug, Clone, PartialEq)]
pub struct OutputDecl {
    pub emissions: Vec<usize>,
    pub format: String,
    pub path: String,
    pub params: HashMap<String, String>,
}

impl RelationalQuery {
    pub fn new() -> Self {
        Self::default()
    }
}
