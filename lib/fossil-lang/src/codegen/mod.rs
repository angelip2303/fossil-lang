//! Layer 5: code generation (RelationalPlan + SQL emission).
//!
//! Mirror of rust-analyzer's backend crates. Owns:
//! - `RelationalQuery` (relational algebra IR — column expressions are
//!   `sqlparser::ast::Expr`, matching DataFusion's `Unparser`)
//! - `SqlDialect` trait + `DefaultDialect`
//! - `FossilPlan` (the host-facing compiled artifact)
//! - `rq_to_sql` + `validate_duckdb_sql` (round-trip validation via sqlparser-rs)

pub mod dialect;
pub mod plan;
pub mod rq;

pub use dialect::{DefaultDialect, ScanStrategy, SqlDialect};
pub use plan::{EntityProjection, FieldMapping, FossilPlan, OutputDef, OutputResult, SourceDef};
pub use rq::emit_sql::{expr_to_sql, rq_to_sql, validate_duckdb_sql};
pub use rq::{
    build, ColId, EmissionDecl, JoinKind, OutputDecl, RelationalQuery, ScanSource, TableId,
    Transform,
};
