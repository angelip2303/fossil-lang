//! Layer 3: definitions + name resolution + item tree + registry.
//!
//! Mirror of rust-analyzer's `hir-def` crate. Owns:
//! - `ItemTree` per-file item layout (invalidation barrier for body edits)
//! - `DefMap` name resolution (scope tree)
//! - `FossilRegistry` (sources + sinks + attribute ops)
//! - Stable item locations (`LetLoc`, `TypeDeclLoc`, `PipelineLoc`)

pub mod def_map;
pub mod item_tree;
pub mod registry;

pub use def_map::{BuiltInFieldType, DefMap, RegisteredTypes, TypeMetadataMap};
pub use item_tree::{
    file_item_tree, ItemRef, ItemTree, LetItem, LetLoc, PipelineItem, PipelineLoc, TypeDeclLoc,
    TypeItem,
};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
