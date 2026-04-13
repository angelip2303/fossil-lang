// Architectural layers (rust-analyzer style: base → syntax → def → ty → codegen).
pub mod base;
pub mod codegen;
pub mod def;
pub mod syntax;
pub mod ty;

// Implementation modules (partially migrated to layered layout).
pub mod ast;
pub mod common;
pub mod db;
pub mod error;
pub mod ir;
pub mod parser;
pub mod passes;

// Back-compat alias: `metadata` moved to `ty/metadata.rs`.
pub use ty::metadata;
pub mod queries;
pub mod resolver;

// Back-compat aliases for files/directories that moved into layer directories.
// Keeps existing imports working without a mass rename across the codebase.
pub use codegen::dialect;
pub use codegen::plan;
pub use codegen::rq;
pub use def::def_map;
pub use def::item_tree;
pub use def::registry;

// Public re-exports — primary API for hosts (keasy).
pub use db::{Db, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver, SourceFile};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
