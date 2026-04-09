use crate::ast::Ast;
use crate::ir;

pub mod lower;
pub mod parse;
pub mod typecheck;

pub use crate::context::GlobalContext;

#[derive(Clone)]
pub struct ParsedProgram {
    pub ast: Ast,
    pub gcx: GlobalContext,
}

#[derive(Clone)]
pub struct IrProgram {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub type_index: ir::TypeIndex,
    pub resolutions: ir::Resolutions,
    pub typeck_results: ir::TypeckResults,
}

// Salsa Update impls — always_update since these contain Arc<dyn Any> which
// can't implement PartialEq. With #[salsa::tracked(no_eq)], Salsa never
// compares values, just overwrites. This is safe because:
// - old_pointer is valid allocated memory owned by Salsa
// - new_value is a freshly computed value
// SAFETY: same contract as salsa::update::always_update
unsafe impl salsa::Update for ParsedProgram {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe { salsa::plumbing::always_update(old_pointer, new_value) }
    }
}

unsafe impl salsa::Update for IrProgram {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe { salsa::plumbing::always_update(old_pointer, new_value) }
    }
}

/// Result of lowering AST → IR.
#[derive(Clone)]
pub struct LowerResult {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub resolutions: ir::Resolutions,
}

unsafe impl salsa::Update for LowerResult {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        unsafe { salsa::plumbing::always_update(old_pointer, new_value) }
    }
}
