use crate::ast::Literal;
use crate::context::*;
use crate::module::BindingId;

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

/// AST representation after type provider execution (all types are concrete)
#[derive(Default, Debug)]
pub struct ConcreteAst {
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
}
