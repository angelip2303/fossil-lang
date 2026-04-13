pub mod ast;
pub mod common;
pub mod db;
pub mod def_map;
pub mod dialect;
pub mod error;
pub mod ir;
pub mod metadata;
pub mod parser;
pub mod passes;
pub mod plan;
pub mod queries;
pub mod registry;
pub mod resolver;
pub mod rq;

// Public re-exports — primary API for hosts (keasy).
pub use db::{Db, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver, SourceFile};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
