//! Layer 5: code generation (RelationalQuery + SQL emission).
//!
//! Owns:
//! - `RelationalQuery` — lowered CTEs (sqlparser AST) + source manifest.
//! - `FossilPlan` — the serializable host-facing compiled artifact.
//! - `RelationalQuery::to_query` — assembles the final `sqlparser::ast::Query`;
//!   callers stringify via `.to_string()`.
//! - `validate_duckdb_sql` — round-trip safety net.
//!
//! Fossil-lang has no dialect abstraction. Source resolution is the host's
//! responsibility, mirroring DataFusion `TableProvider`, PRQL catalog context,
//! and Ibis backends.

pub mod plan;
pub mod rq;

pub use plan::{EntityProjection, FieldMapping, FossilPlan, OutputDef, OutputResult, SourceDef};
pub use rq::{expr_to_sql, rq_to_query, validate_duckdb_sql};
pub use rq::{build, EmissionDecl, OutputDecl, RelationalQuery, SourceRef};
