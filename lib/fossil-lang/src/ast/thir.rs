use polars::prelude::DataType;

use crate::ast::Span;
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct TypedHir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

/// A statement in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
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
    pub ty: TypeId,
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

impl From<DataType> for PrimitiveType {
    fn from(value: DataType) -> Self {
        match value {
            DataType::Boolean => PrimitiveType::Bool,

            DataType::Int8
            | DataType::Int16
            | DataType::Int32
            | DataType::Int64
            | DataType::Int128
            | DataType::UInt8
            | DataType::UInt16
            | DataType::UInt32
            | DataType::UInt64 => PrimitiveType::Int,

            DataType::Float32 | DataType::Float64 => todo!(),

            DataType::String => PrimitiveType::String,

            DataType::Null => PrimitiveType::Unit,

            _ => todo!(),
        }
    }
}

/// Type variable for polymorphism
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

/// Polytypes (or type schemes) are types containing variables bound by zero or more for-all
/// quantifiers, e.g. `forall a. a -> a`. As an example, a function `forall a. a -> a` with
/// polytype can map any value of the same type to itself and the identity function is a value
/// for this type. As another example, `forall a. Set(a) -> int` is the type of a function mapping
/// all finite sets to integers. A function which returns the size of a set is a value for this type.
#[derive(Clone, Debug)]
pub struct Polytype {
    pub forall: Vec<TypeVar>,
    pub ty: TypeId,
}

impl Polytype {
    pub fn mono(ty: TypeId) -> Self {
        Polytype { forall: vec![], ty }
    }

    pub fn poly(forall: Vec<TypeVar>, ty: TypeId) -> Self {
        Polytype { forall, ty }
    }
}
