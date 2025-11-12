use std::collections::HashMap;

use crate::ast::Literal;
use crate::context::*;
use crate::module::BindingId;

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct ResolvedAst {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub symbols: Interner,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Let(Symbol, ExprId),
    Type(Symbol, TypeId),
    Expr(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    LocalItem(DeclId),
    ModuleItem(BindingId),
    Literal(Literal),
    List(Vec<ExprId>),
    Record(Vec<(Symbol, ExprId)>),
    Function { params: Vec<Symbol>, body: ExprId },
    Application { callee: ExprId, args: Vec<ExprId> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    String,
    Bool,
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
    Provider {
        provider: BindingId,
        args: Vec<Literal>,
    },
}

/// Scope for name resolution
#[derive(Default)]
pub struct Scope {
    pub vars: HashMap<Symbol, DeclId>,
    pub types: HashMap<Symbol, TypeId>,
}

impl Scope {
    /// This implements variable shadowing
    pub fn define_var(&mut self, name: Symbol, decl_id: DeclId) {
        self.vars.insert(name, decl_id);
    }

    /// This implements type shadowing
    pub fn define_type(&mut self, name: Symbol, type_id: TypeId) {
        self.types.insert(name, type_id);
    }

    pub fn lookup_var(&self, name: Symbol) -> Option<DeclId> {
        self.vars.get(&name).copied()
    }

    pub fn lookup_type(&self, name: Symbol) -> Option<TypeId> {
        self.types.get(&name).copied()
    }
}
