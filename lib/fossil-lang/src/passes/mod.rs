use crate::ast::Ast;
use crate::ir;

pub mod lower;
pub mod parse;
pub mod typecheck;

pub use crate::context::GlobalContext;

#[derive(Clone, PartialEq)]
pub struct ParsedProgram {
    pub ast: Ast,
    pub gcx: GlobalContext,
}

#[derive(Clone, PartialEq)]
pub struct IrProgram {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub type_index: ir::TypeIndex,
    pub resolutions: ir::Resolutions,
    pub typeck_results: ir::TypeckResults,
}

/// Result of lowering AST → IR.
#[derive(Clone, PartialEq)]
pub struct LowerResult {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub resolutions: ir::Resolutions,
}

