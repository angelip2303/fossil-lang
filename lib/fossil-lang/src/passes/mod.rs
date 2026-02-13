use crate::ast::Ast;
use crate::ir;

pub mod convert;
pub mod expand;
pub mod parse;
pub mod resolve;
pub mod typecheck;

pub use crate::context::GlobalContext;

pub struct ParsedProgram {
    pub ast: Ast,
    pub gcx: GlobalContext,
}

pub struct IrProgram {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub type_index: ir::TypeIndex,
    pub resolutions: ir::Resolutions,
    pub typeck_results: ir::TypeckResults,
}
