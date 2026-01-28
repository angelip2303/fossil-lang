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
    /// A value binding `let name = expr` or `let name: Type = expr`
    Let {
        name: Symbol,
        ty: Option<TypeId>,
        value: ExprId,
    },
    /// A constant binding `const name = expr`
    Const { name: Symbol, value: ExprId },
    /// A type definition `type name = type`
    Type { name: Symbol, ty: TypeId },
    /// A trait definition `trait Name { method: type, ... }`
    Trait {
        name: Symbol,
        methods: Vec<TraitMethod>,
    },
    /// A trait implementation `impl Trait for Type { method = expr, ... }`
    Impl {
        trait_name: Path,
        type_name: Path,
        methods: Vec<(Symbol, ExprId)>,
    },
    /// An expression declaration `expr`
    Expr(ExprId),
}

/// A method signature in a trait definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethod {
    pub name: Symbol,
    pub ty: TypeId,
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
    /// A function application `callee(arg1, arg2, ...)` with positional and named arguments
    Application { callee: ExprId, args: Vec<Argument> },
    /// A pipe expression `lhs |> rhs`
    Pipe { lhs: ExprId, rhs: ExprId },
    /// Field access `expr.field`
    FieldAccess { expr: ExprId, field: Symbol },
    /// A block expression `{ stmt* }`
    /// Contains zero or more statements.
    /// If the last statement is Stmt::Expr(e), the block returns e.
    /// Otherwise, returns Unit.
    Block { stmts: Vec<StmtId> },
    /// A string interpolation `"Hello ${name}, you are ${age} years old!"`
    /// Invariant: parts.len() == exprs.len() + 1
    /// For example: "a${x}b${y}c" has parts=["a", "b", "c"] and exprs=[x, y]
    StringInterpolation {
        parts: Vec<Symbol>,
        exprs: Vec<ExprId>,
    },
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
    /// A type provider invocation `provider!(arg1, arg2, ...)` with named/positional args
    Provider {
        provider: Path,
        args: Vec<ProviderArgument>,
    },
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
    Relative { dots: u8, components: Vec<Symbol> },
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
                *parts.last().expect("BUG: Qualified path with zero parts")
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

/// An argument in a function call (positional or named)
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

/// A provider argument (literal-based, positional or named)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProviderArgument {
    /// A positional argument: `csv!("file.csv")`
    Positional(Literal),
    /// A named argument: `csv!(path: "file.csv")`
    Named { name: Symbol, value: Literal },
    /// A reference to a compile-time const: `sql!(DB, "customers")`
    ConstRef(Symbol),
}

impl ProviderArgument {
    /// Get the literal value of this argument
    ///
    /// # Panics
    /// Panics if called on a `ConstRef` that hasn't been resolved during expansion.
    pub fn value(&self) -> &Literal {
        match self {
            ProviderArgument::Positional(lit) => lit,
            ProviderArgument::Named { value, .. } => value,
            ProviderArgument::ConstRef(_) => {
                panic!(
                    "ConstRef must be resolved during provider expansion before accessing its value"
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Symbol,
    /// Optional type annotation for this parameter (e.g., `fn(x: int)`)
    pub ty: Option<TypeId>,
    /// Optional default value for this parameter
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Float,
    String,
    Bool,
}

impl From<polars::prelude::DataType> for PrimitiveType {
    fn from(value: polars::prelude::DataType) -> Self {
        use polars::prelude::DataType;
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
            DataType::Float32 | DataType::Float64 => PrimitiveType::Float,
            DataType::String => PrimitiveType::String,
            _ => todo!("Unsupported data type: {:?}", value),
        }
    }
}

/// Attribute annotation on record fields
///
/// Supports named arguments like `#[rdf(uri = "...", required = true)]`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: Symbol,
    pub args: Vec<AttributeArg>,
    pub loc: Loc,
}

/// A single argument in an attribute
///
/// Represents `key = value` pairs like `uri = "http://..."`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttributeArg {
    pub key: Symbol,
    pub value: Literal,
}

/// Record field with optional attributes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Symbol,
    pub ty: TypeId,
    pub attrs: Vec<Attribute>,
}
