use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::context::Interner;
use crate::module::BindingId;

pub mod parse;
pub mod resolve;
pub mod typecheck;
pub mod typegen;

#[derive(Default)]
pub struct AstCtx {
    pub ast: Rc<RefCell<Ast>>,
    pub symbols: Rc<RefCell<Interner>>,
}

impl AstCtx {
    pub fn ast<'a>(&'a self) -> RefMut<'a, Ast> {
        self.ast.borrow_mut()
    }

    pub fn symbols<'a>(&'a self) -> RefMut<'a, Interner> {
        self.symbols.borrow_mut()
    }

    pub fn take(self) -> (Ast, Interner) {
        let ast = self.ast.take();
        let symbols = self.symbols.take();
        (ast, symbols)
    }
}

/// The parsing result, e.g. AST + symbol table
pub struct ParsedProgram {
    pub ast: Ast,
    pub symbols: Interner,
}

/// The resolution result, i.e. associates names with their definitions
pub struct ResolvedProgram {
    pub ast: Ast,
    pub symbols: Interner,
    pub resolution: ResolutionTable,
}

/// Reference to a binding (where an identifier is defined)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingRef {
    /// A local declaration (let or type)
    Local(DeclId),
    /// A module binding (function or provider)
    Module(BindingId),
    /// A function parameter (includes which function defines it)
    Parameter { function: ExprId },
}

#[derive(Debug, Default)]
pub struct ResolutionTable {
    pub exprs: HashMap<ExprId, BindingRef>,
    pub types: HashMap<TypeId, BindingRef>,
    pub providers: HashMap<TypeId, BindingRef>,
}

/// The result of type checking
#[derive(Debug)]
pub struct TypedProgram {
    pub ast: Ast,
    pub symbols: Interner,
    pub resolution: ResolutionTable,
    pub expr_types: HashMap<ExprId, TypeId>,
}
