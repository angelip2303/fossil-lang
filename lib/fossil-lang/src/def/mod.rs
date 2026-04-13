//! Layer 3: definitions + name resolution + item tree + registry.
//!
//! Facade module mirroring rust-analyzer's `hir-def` crate. Owns:
//! - `ItemTree` per-file item layout (invalidation barrier)
//! - `DefMap` name resolution
//! - `FossilRegistry` (sources + sinks + attribute ops)
//! - Stable item locations (`LetLoc`, `TypeDeclLoc`, `PipelineLoc`)
//!
//! Future refactor: physically move `def_map.rs`, `registry/`, `item_tree.rs`
//! into this module.

pub use crate::def_map::{BuiltInFieldType, DefMap, RegisteredTypes, TypeMetadataMap};
pub use crate::item_tree::{
    file_item_tree, ItemRef, ItemTree, LetItem, LetLoc, PipelineItem, PipelineLoc, TypeDeclLoc,
    TypeItem,
};
pub use crate::registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
