use crate::ir;
use crate::ty::typecheck::{TypeIndex, TypeckResults};

pub mod lower;

/// Result of type-checking: only the new artifacts produced by the checker.
/// Returned by `TypeChecker::check` / `check_tolerant` and consumed by
/// per-item `let_infer_query` to pull `typeck_results.binding_types`.
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
