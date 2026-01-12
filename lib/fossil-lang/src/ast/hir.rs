use crate::ast::Loc;
use crate::ast::ast::{Attribute, Literal, Path, PrimitiveType};
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

/// HIR Param with resolved DefId
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Symbol,
    pub def_id: DefId,
}

/// HIR RecordField with attributes preserved from AST
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Symbol,
    pub ty: TypeId,
    pub attrs: Vec<Attribute>,
}

#[derive(Default, Debug)]
pub struct Hir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
}

#[derive(Debug)]
pub struct Stmt {
    pub loc: Loc,
    pub kind: StmtKind,
}

/// A declaration in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    /// An import declaration `open Module as alias`
    Import { module: Path, alias: Symbol },
    /// A value binding `let name = expr`
    Let { name: Symbol, def_id: DefId, value: ExprId },
    /// A type definition `type name = type`
    Type { name: Symbol, ty: TypeId },
    /// An expression declaration `expr`
    Expr(ExprId),
}

#[derive(Debug)]
pub struct Expr {
    pub loc: Loc,
    pub kind: ExprKind,
}

/// An expression in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// A local or qualified identifier (resolved)
    Identifier(DefId),
    /// A unit value `()`
    Unit,
    /// A literal value, e.g. `1`, `"hello"`, `true`
    Literal(Literal),
    /// A list `[expr, expr, ...]`
    List(Vec<ExprId>),
    /// A record `{ field = expr, field = expr, ... }`
    Record(Vec<(Symbol, ExprId)>),
    /// A function definition `fn (param1, param2, ...) -> expr`
    Function { params: Vec<Param>, body: ExprId },
    /// A function application `callee(arg1, arg2, ...)`
    Application { callee: ExprId, args: Vec<ExprId> },
    /// Field access `expr.field`
    FieldAccess { expr: ExprId, field: Symbol },
    /// A block expression `{ stmt* }`
    Block { stmts: Vec<StmtId> },
}

#[derive(Debug)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// A named type
    Named(DefId),
    /// A primitive type
    Primitive(PrimitiveType),
    /// A type function type `(T1, T2, ...) -> T`
    Function(Vec<TypeId>, TypeId),
    /// An applied type (type constructor application)
    ///
    /// Represents generic types like `List<T>`, `Entity<Person>`.
    /// See THIR TypeKind::App for full documentation.
    App {
        /// DefId of the type constructor
        ctor: DefId,
        /// Type arguments
        args: Vec<TypeId>,
    },
    /// A record type `{ field: T, field: T, ... }` with attributes
    Record(Vec<RecordField>),
}
