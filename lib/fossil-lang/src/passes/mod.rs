use crate::context::global::{DefMap, RegisteredTypes, TypeMetadataMap};
use crate::ir;

pub mod lower;
pub mod parse;
pub mod typecheck;

#[derive(Clone, PartialEq)]
pub struct IrProgram {
    pub ir: ir::Ir,
    pub def_map: DefMap,
    pub registered_types: RegisteredTypes,
    pub type_metadata: TypeMetadataMap,
    pub type_index: ir::TypeIndex,
    pub resolutions: ir::Resolutions,
    pub typeck_results: ir::TypeckResults,
}

/// Result of lowering AST → IR.
#[derive(Clone, PartialEq)]
pub struct LowerResult {
    pub ir: ir::Ir,
    pub def_map: DefMap,
    pub type_metadata: TypeMetadataMap,
    pub registered_types: RegisteredTypes,
    pub resolutions: ir::Resolutions,
}
