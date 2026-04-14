use crate::def_map::{DefMap, RegisteredTypes, TypeMetadataMap};
use crate::ir;
use crate::ty::typecheck::{TypeIndex, TypeckResults};

pub mod lower;

/// Result of type-checking: only the new artifacts produced by the checker.
/// `def_map`, `registered_types`, `type_metadata` are read from earlier queries.
///
/// `type_index` and `typeck_results` speak in terms of the interned `Ty`
/// (canonical identity, O(1) equality) rather than per-IR arena indices —
/// see [`crate::ty::types`] for the rationale.
#[derive(Clone, PartialEq)]
pub struct InferResult {
    pub ir: ir::Ir,
    pub type_index: TypeIndex,
    pub resolutions: ir::Resolutions,
    pub typeck_results: TypeckResults,
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
