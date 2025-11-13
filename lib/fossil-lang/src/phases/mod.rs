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
    pub fn ast(&self) -> RefMut<Ast> {
        self.ast.borrow_mut()
    }

    pub fn symbols(&self) -> RefMut<Interner> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingRef {
    Local(DeclId),
    Module(BindingId),
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

impl std::fmt::Display for TypedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (expr, ty) in self.expr_types.iter() {
            let expr = self.ast.exprs.get(*expr);
            let ty = self.ast.types.get(*ty);
            writeln!(f, "(Expr: {:?}) -> (Type: {:?})", expr, ty)?;
        }

        Ok(())
    }
}
