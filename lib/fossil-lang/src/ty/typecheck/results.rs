//! Type-checker outputs — the artifacts produced once inference is done.
//!
//! These types replace the old `ir::TypeckResults`/`ir::TypeIndex`/
//! `ir::TypeDeclInfo`. They speak in terms of the interned `Ty` (canonical
//! identity, O(1) equality) instead of the per-IR `la_arena::Idx<Type>`,
//! matching the rustc `Ty<'tcx>` idiom.

use std::collections::HashMap;

use crate::db::{DefId, Symbol};
use crate::ir::ExprId;
use crate::ty::types::Ty;

/// Type declaration metadata for user-defined and provider types. The `ty`
/// field holds the *interned* type (typically a `TyKind::Record`) — codegen
/// only consumes `ctor_param_names` and `field_names`, but the type is kept
/// for completeness and future passes (formatting, doc generation, …).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDeclInfo {
    pub ty: Ty,
    pub ctor_param_count: usize,
    pub ctor_param_names: Vec<Symbol>,
    pub field_names: Vec<Symbol>,
}

/// Index of type declarations: `DefId` → metadata.
pub type TypeIndex = HashMap<DefId, TypeDeclInfo>;

/// All per-expression / per-binding types produced by inference.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct TypeckResults {
    pub expr_types: HashMap<ExprId, Ty>,
    pub binding_types: HashMap<DefId, Ty>,
}
