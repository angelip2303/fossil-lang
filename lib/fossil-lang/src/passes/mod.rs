use crate::ast::*;
use crate::context::{Definitions, Interner};

pub mod expand;
pub mod lower;
pub mod parse;
pub mod resolve;
// pub mod typecheck;

pub struct GlobalContext {
    pub interner: Interner,
    pub definitions: Definitions,
}

impl GlobalContext {
    pub fn new() -> Self {
        Self {
            interner: Interner::default(),
            definitions: Definitions::default(),
        }
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ParsedProgram {
    pub ast: ast::Ast,
    pub gcx: GlobalContext,
}

pub struct HirProgram {
    pub hir: hir::Hir,
    pub gcx: GlobalContext,
}

pub struct ThirProgram {
    pub thir: thir::TypedHir,
    pub gcx: GlobalContext,
}
