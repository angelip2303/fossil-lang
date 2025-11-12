use crate::context::*;

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct Ast {
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
    Identifier(Symbol),
    Qualified(Vec<Symbol>),
    Literal(Literal),
    List(Vec<ExprId>),
    Record(Vec<(Symbol, ExprId)>),
    Function { params: Vec<Symbol>, body: ExprId },
    Application { callee: ExprId, args: Vec<ExprId> },
    // TODO: pipe
    // TODO: member access
    // TODO: type annotation
    // TODO: cast
    // TODO: unary op
    // TODO: binary op
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Named(Symbol),
    Provider(Symbol, Vec<Literal>),
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
}
