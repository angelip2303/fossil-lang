//! Layer 5: code generation (RelationalPlan + SQL emission).
//!
//! Facade module mirroring rust-analyzer's backend crates. Exposes the
//! relational algebra IR (`RelationalQuery`), the SQL emission entry point
//! `rq_to_sql`, and the `SqlDialect` trait for backend-specific dispatch.
//!
//! Also exposes `FossilPlan` (the host-facing compiled artifact) and
//! `validate_duckdb_sql` (round-trip parse validation via sqlparser-rs).
//!
//! Future refactor: physically move `rq/`, `dialect.rs`, `plan/` into
//! this module as `codegen/{rq,dialect,plan}`.

pub use crate::dialect::{DefaultDialect, ScanStrategy, SqlDialect};
pub use crate::plan::{EntityProjection, FieldMapping, FossilPlan, OutputDef, OutputResult, SourceDef};
pub use crate::rq::emit_sql::{expr_to_sql, rq_to_sql, validate_duckdb_sql};
pub use crate::rq::{
    ColId, EmissionDecl, JoinKind, OutputDecl, RelationalQuery, RqExpr, RqLiteral, ScanSource,
    TableId, Transform,
};
