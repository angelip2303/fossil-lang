// Concrete implementation modules (current physical layout).
pub mod ast;
pub mod common;
pub mod db;
pub mod def_map;
pub mod dialect;
pub mod error;
pub mod ir;
pub mod item_tree;
pub mod metadata;
pub mod parser;
pub mod passes;
pub mod plan;
pub mod queries;
pub mod registry;
pub mod resolver;
pub mod rq;

// Architectural facade layers (rust-analyzer style: base → syntax → def → ty → codegen).
// These are re-export facades that document the layered architecture without
// physically moving files. Future PRs can migrate implementations into these
// modules non-breakingly as long as the public re-exports remain stable.
pub mod base;
pub mod codegen;
pub mod def;
pub mod syntax;
pub mod ty;

// Public re-exports — primary API for hosts (keasy).
pub use db::{Db, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver, SourceFile};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
