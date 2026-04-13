//! Layer 3: definitions + name resolution + item tree + registry.
//!
//! Mirror of rust-analyzer's `hir-def` crate. Owns:
//! - `ItemTree` per-file item layout (invalidation barrier for body edits)
//! - `DefMap` name resolution (scope tree)
//! - `FossilRegistry` (sources + sinks + attribute ops)
//! - Stable item locations (`LetLoc`, `TypeDeclLoc`, `PipelineLoc`)

pub mod body;
pub mod def_map;
pub mod item_tree;
pub mod registry;

pub use body::{
    file_def_map_for_body, let_body, let_body_query, pipeline_body, pipeline_body_query,
    type_decl_body, type_decl_body_query, FileDefMapForBody, HirBody,
};
pub use def_map::{BuiltInFieldType, DefMap, RegisteredTypes, TypeMetadataMap};
pub use item_tree::{
    file_item_tree, find_let_by_name, find_type_by_name, item_count, let_names, ItemRef, ItemTree,
    LetItem, LetLoc, PipelineItem, PipelineLoc, TypeDeclLoc, TypeItem,
};
pub use registry::{
    AnonOp, AttributeOp, AttributeOpKind, AttributeRegistry, CleanOp, FossilRegistry, ParamDef,
    SinkDef, SinkFormat, SinkRegistry, SourceDef, SourceRegistry,
};
