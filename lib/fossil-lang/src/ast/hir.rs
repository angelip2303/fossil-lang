use crate::ast::Loc;
use crate::ast::ast::{Attribute, Literal, Path, PrimitiveType};
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

/// HIR Param with resolved DefId and optional default value
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Symbol,
    pub def_id: DefId,
    /// Optional default value for this parameter
    pub default: Option<ExprId>,
}

/// An argument in a function call (positional or named) - HIR version
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    /// A positional argument: `func(expr)`
    Positional(ExprId),
    /// A named argument: `func(name: expr)`
    Named { name: Symbol, value: ExprId },
}

impl Argument {
    /// Get the expression value of this argument
    pub fn value(&self) -> ExprId {
        match self {
            Argument::Positional(expr) => *expr,
            Argument::Named { value, .. } => *value,
        }
    }
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
    /// An import declaration `open Module { items } as alias`
    Import {
        module: Path,
        items: Option<Vec<Symbol>>,
        alias: Symbol,
    },
    /// A value binding `let name: ty = expr` with optional type annotation
    Let {
        name: Symbol,
        def_id: DefId,
        /// Optional type annotation (e.g., `let x: int = ...`)
        ty: Option<TypeId>,
        value: ExprId,
    },
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
    /// A function application `callee(arg1, arg2, ...)` with positional and named arguments
    Application { callee: ExprId, args: Vec<Argument> },
    /// Field access `expr.field`
    FieldAccess { expr: ExprId, field: Symbol },
    /// A block expression `{ stmt* }`
    Block { stmts: Vec<StmtId> },
    /// A string interpolation `"Hello ${name}, you are ${age} years old!"`
    /// Invariant: parts.len() == exprs.len() + 1
    StringInterpolation {
        parts: Vec<Symbol>,
        exprs: Vec<ExprId>,
    },
    /// Placeholder `_` for field selectors (e.g., `_.field`)
    Placeholder,
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
