use crate::def_map::{DefMap, RegisteredTypes, TypeMetadataMap};
use crate::ir;

pub mod lower;
pub mod parse;
// Back-compat alias: `passes::typecheck` → `ty::typecheck` after the migration
// that moved the typecheck module into the `ty/` layer directory.
pub use crate::ty::typecheck;

/// Result of type-checking: only the new artifacts produced by the checker.
/// `def_map`, `registered_types`, `type_metadata` are read from earlier queries.
#[derive(Clone, PartialEq)]
pub struct InferResult {
    pub ir: ir::Ir,
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
