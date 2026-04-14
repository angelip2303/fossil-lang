// Architectural layers (rust-analyzer style: base → syntax → def → ty → codegen).
pub mod base;
pub mod codegen;
pub mod def;
pub mod syntax;
pub mod ty;

// Implementation modules not yet migrated into a layer directory.
pub mod error;
pub mod ir;
pub mod passes;
pub mod queries;

// Back-compat aliases for modules physically migrated into layer directories.
// Keeps existing `use crate::X::*` imports working without a mass rename.
// Kept only for paths still used internally (ast, parser, metadata, def_map)
// or externally by keasy (db, dialect, plan, rq, registry).
pub use base::db;
pub use codegen::dialect;
pub use codegen::plan;
pub use codegen::rq;
pub use def::def_map;
pub use def::registry;
pub use syntax::ast;
pub use syntax::parser;
pub use ty::metadata;

// Public re-exports — primary API for hosts (keasy).
pub use db::{Db, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver, SourceFile};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
