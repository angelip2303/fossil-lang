use crate::ast::ast;
use crate::ir;

pub mod expand;
pub mod parse;
pub mod resolve;
pub mod typecheck;

// Re-export GlobalContext and related types from context module
pub use crate::context::{BuiltinTraits, GlobalContext, TraitImplInfo};

/// Parsed program (AST representation)
pub struct ParsedProgram {
    pub ast: ast::Ast,
    pub gcx: GlobalContext,
}

/// Unified IR program (after resolution and type checking)
pub struct IrProgram {
    pub ir: ir::Ir,
    pub gcx: GlobalContext,
    pub env: crate::passes::typecheck::TypeEnv,
}
