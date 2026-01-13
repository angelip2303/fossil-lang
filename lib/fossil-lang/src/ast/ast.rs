use crate::ast::Loc;
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct Ast {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    /// Root statement IDs of the program
    pub root: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub loc: Loc,
    pub kind: StmtKind,
}

/// A declaration in the language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    /// An import declaration `open Module as alias`
    Import { module: Path, alias: Option<Symbol> },
    /// A value binding `let name = expr` or `let name: Type = expr`
    Let {
        name: Symbol,
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
    Function { params: Vec<Param>, body: ExprId },
    /// A function application `callee(arg1, arg2, ...)`
    Application { callee: ExprId, args: Vec<ExprId> },
    /// A pipe expression `lhs |> rhs`
    Pipe { lhs: ExprId, rhs: ExprId },
    /// Field access `expr.field`
    FieldAccess { expr: ExprId, field: Symbol },
    /// A block expression `{ stmt* }`
    /// Contains zero or more statements.
    /// If the last statement is Stmt::Expr(e), the block returns e.
    /// Otherwise, returns Unit.
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
    Named(Path),
    /// A unit type
    Unit,
    /// A primitive type
    Primitive(PrimitiveType),
    /// A type provider invocation `Provider<arg1, arg2, ...>` (unresolved)
    Provider { provider: Path, args: Vec<Literal> },
    /// A type function type `(T1, T2, ...) -> T`
    Function(Vec<TypeId>, TypeId),
    /// A list type `[T]`
    List(TypeId),
    /// A record type `{ field: T, field: T, ... }`
    Record(Vec<RecordField>),
    /// An applied type (type constructor application) `Name<T1, T2, ...>`
    /// For example: `Entity<Person>`, `List<Int>`, `Map<String, Int>`
    App { ctor: Path, args: Vec<TypeId> },
}

/// A path to an identifier (either simple, qualified, or relative)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    /// A simple, unqualified identifier: `foo`
    Simple(Symbol),
    /// A qualified path with multiple components: `std::list::map`
    Qualified(Vec<Symbol>),
    /// A relative path starting with ./ or ../
    /// - dots = 0 means ./ (current directory)
    /// - dots = 1 means ../ (parent directory)
    /// - dots = 2 means ../../ (grandparent directory), etc.
    Relative {
        dots: u8,
        components: Vec<Symbol>,
    },
}

impl Path {
    pub fn simple(sym: Symbol) -> Self {
        Path::Simple(sym)
    }

    pub fn qualified(parts: Vec<Symbol>) -> Self {
        let slice = parts.as_slice();
        match slice {
            [sym] => Path::Simple(*sym),
            _ => Path::Qualified(parts),
        }
    }

    pub fn parent(&self) -> Option<Symbol> {
        match self {
            Path::Simple(_) => None,
            Path::Qualified(parts) => parts.get(parts.len() - 2).copied(),
            Path::Relative { components, .. } => {
                components.get(components.len().saturating_sub(2)).copied()
            }
        }
    }

    pub fn item(&self) -> Symbol {
        match self {
            Path::Simple(sym) => *sym,
            Path::Qualified(parts) => {
                // SAFETY: Qualified paths are validated to be non-empty at construction
                // via Path::qualified() which converts single-element paths to Simple.
                *parts
                    .last()
                    .expect("BUG: Qualified path with zero parts")
            }
            Path::Relative { components, .. } => {
                // Relative paths must have at least one component
                *components
                    .last()
                    .expect("BUG: Relative path with zero components")
            }
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Path::Simple(_) => 1,
            Path::Qualified(parts) => parts.len(),
            Path::Relative { components, .. } => components.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Path::Simple(_) => false,
            Path::Qualified(parts) => parts.is_empty(),
            Path::Relative { components, .. } => components.is_empty(),
        }
    }
}

impl From<Path> for Vec<Symbol> {
    fn from(path: Path) -> Self {
        match path {
            Path::Simple(sym) => vec![sym],
            Path::Qualified(parts) => parts,
            Path::Relative { components, .. } => components,
        }
    }
}

impl From<Vec<Symbol>> for Path {
    fn from(parts: Vec<Symbol>) -> Self {
        if parts.len() == 1 {
            Path::Simple(parts[0])
        } else {
            Path::Qualified(parts)
        }
    }
}

impl From<Symbol> for Path {
    fn from(sym: Symbol) -> Self {
        Path::Simple(sym)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Symbol,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Float,
    String,
    Bool,
}

/// Attribute annotation on record fields
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: Symbol,
    pub args: Vec<Literal>,
}

/// Record field with optional attributes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Symbol,
    pub ty: TypeId,
    pub attrs: Vec<Attribute>,
}
