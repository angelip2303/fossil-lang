use crate::ast::Span;
use crate::context::*;

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct Hir {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
}

#[derive(Debug)]
pub struct Decl {
    pub span: Span,
    pub kind: DeclKind,
}

/// A declaration in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclKind {
    /// An import declaration `open Module as alias`
    Import { module: Path, alias: Symbol },
    /// A value binding `let name = expr`
    Let { name: Symbol, value: ExprId },
    /// A type definition `type name = type`
    Type { name: Symbol, ty: TypeId },
    /// An expression declaration `expr`
    Expr(ExprId),
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

/// An expression in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// A local or qualified identifier (unresolved)
    Identifier(Path),
    /// A unit value `()`
    Unit,
    /// A literal value, e.g. `1`, `"hello"`, `true`
    Literal(Literal),
    /// A list `[expr, expr, ...]`
    List(Vec<ExprId>),
    /// A record `{ field = expr, field = expr, ... }`
    Record(Vec<(Symbol, ExprId)>),
    /// A function definition `fn (param1, param2, ...) -> expr`
    Function { params: Vec<Symbol>, body: ExprId },
    /// A function application `callee(arg1, arg2, ...)`
    Application { callee: ExprId, args: Vec<ExprId> },
    // TODO: member access
}

#[derive(Debug)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// A named type
    Named(Path),
    /// A primitive type
    Primitive(PrimitiveType),
    /// A type function type `(T1, T2, ...) -> T`
    Function(Vec<TypeId>, TypeId),
    /// A list type `[T]`
    List(TypeId),
    /// A record type `{ field: T, field: T, ... }`
    Record(Vec<(Symbol, TypeId)>),
    /// A type variable (for type inference)
    Var(TypeVar),
}

/// A path to an identifier (either simple or qualified)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    Simple(Symbol),
    Qualified(Vec<Symbol>),
}

impl Path {
    pub fn simple(sym: Symbol) -> Self {
        Path::Simple(sym)
    }

    pub fn qualified(parts: Vec<Symbol>) -> Self {
        if parts.len() == 1 {
            Path::Simple(parts[0])
        } else {
            Path::Qualified(parts)
        }
    }

    pub fn as_slice(&self) -> &[Symbol] {
        match self {
            Path::Simple(s) => std::slice::from_ref(s),
            Path::Qualified(v) => v.as_slice(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    String,
    Bool,
    Unit,
}
