//! Unified Intermediate Representation (IR)
//!
//! This IR serves as the single representation throughout the compilation pipeline:
//! - After parsing: identifiers are Unresolved(Path), types are Unknown
//! - After resolution: identifiers become Resolved(DefId), Pipe is desugared
//! - After typechecking: types become Known(TypeId)
//!
//! This design enables in-place mutation and avoids rebuilding the tree at each pass.

pub mod convert;
pub mod resolve;

use polars::prelude::DataType;

use crate::ast::Loc;
use crate::context::{Arena, DefId, NodeId, Symbol};

pub use convert::ast_to_ir;
pub use resolve::IrResolver;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

/// The intermediate representation
#[derive(Default, Debug)]
pub struct Ir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    /// Root statement IDs of the program
    pub root: Vec<StmtId>,
}

/// An identifier that can be unresolved (Path) or resolved (DefId)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    /// Unresolved path from parsing
    Unresolved(Path),
    /// Resolved DefId from name resolution
    Resolved(DefId),
}

impl Ident {
    /// Get the DefId if resolved, panics if unresolved
    pub fn def_id(&self) -> DefId {
        match self {
            Ident::Resolved(id) => *id,
            Ident::Unresolved(_) => panic!("Attempted to access DefId of unresolved identifier"),
        }
    }

    /// Check if the identifier is resolved
    pub fn is_resolved(&self) -> bool {
        matches!(self, Ident::Resolved(_))
    }
}

impl From<Path> for Ident {
    fn from(path: Path) -> Self {
        Ident::Unresolved(path)
    }
}

impl From<DefId> for Ident {
    fn from(def_id: DefId) -> Self {
        Ident::Resolved(def_id)
    }
}

/// Type reference that can be unknown or known
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum TypeRef {
    /// Type not yet inferred
    #[default]
    Unknown,
    /// Known type ID
    Known(TypeId),
}

impl TypeRef {
    /// Get the TypeId if known, panics if unknown
    pub fn type_id(&self) -> TypeId {
        match self {
            TypeRef::Known(id) => *id,
            TypeRef::Unknown => panic!("Attempted to access TypeId of unknown type"),
        }
    }

    /// Check if the type is known
    pub fn is_known(&self) -> bool {
        matches!(self, TypeRef::Known(_))
    }
}

impl From<TypeId> for TypeRef {
    fn from(type_id: TypeId) -> Self {
        TypeRef::Known(type_id)
    }
}

// =============================================================================
// Statements
// =============================================================================

#[derive(Debug, Clone)]
pub struct Stmt {
    pub loc: Loc,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(loc: Loc, kind: StmtKind) -> Self {
        Self { loc, kind }
    }
}

/// A statement in the IR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    /// A value binding `let name = expr` with optional type annotation and resolved DefId
    Let {
        name: Symbol,
        def_id: Option<DefId>,
        ty: Option<TypeId>,
        value: ExprId,
    },
    /// A constant binding `const name = expr` with resolved DefId
    Const {
        name: Symbol,
        def_id: Option<DefId>,
        value: ExprId,
    },
    /// A type definition `type name = type`
    Type { name: Symbol, ty: TypeId },
    /// A trait definition `trait Name { method: type, ... }`
    Trait {
        name: Symbol,
        def_id: Option<DefId>,
        methods: Vec<TraitMethod>,
    },
    /// A trait implementation `impl Trait for Type { method = expr, ... }`
    Impl {
        trait_name: Ident,
        type_name: Ident,
        methods: Vec<(Symbol, ExprId)>,
    },
    /// An expression statement `expr`
    Expr(ExprId),
}

/// A method signature in a trait definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitMethod {
    pub name: Symbol,
    pub def_id: Option<DefId>,
    pub ty: TypeId,
}

// =============================================================================
// Expressions
// =============================================================================

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub kind: ExprKind,
    /// Type of this expression (filled in during type checking)
    pub ty: TypeRef,
}

/// An expression in the IR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// An identifier (unresolved or resolved)
    Identifier(Ident),
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
    /// A pipe expression `lhs |> rhs` (only present before resolution, desugared after)
    Pipe { lhs: ExprId, rhs: ExprId },
    /// Field access `expr.field`
    FieldAccess { expr: ExprId, field: Symbol },
    /// A block expression `{ stmt* }`
    Block { stmts: Vec<StmtId> },
    /// A string interpolation `"Hello ${name}, you are ${age} years old!"`
    StringInterpolation {
        parts: Vec<Symbol>,
        exprs: Vec<ExprId>,
    },
    /// Placeholder `_` for field selectors (e.g., `_.field`)
    Placeholder,
    /// Field selector `_.field` - type-safe reference to a field (created during type checking)
    FieldSelector {
        field: Symbol,
        record_ty: TypeId,
    },
}

/// A parameter in a function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub name: Symbol,
    pub def_id: Option<DefId>,
    pub ty: Option<TypeId>,
    pub default: Option<ExprId>,
}

/// An argument in a function call
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Argument {
    Positional(ExprId),
    Named { name: Symbol, value: ExprId },
}

impl Argument {
    pub fn value(&self) -> ExprId {
        match self {
            Argument::Positional(expr) => *expr,
            Argument::Named { value, .. } => *value,
        }
    }
}

/// A literal value
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

// =============================================================================
// Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeKind,
}

/// A type in the IR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// A named type (unresolved or resolved)
    Named(Ident),
    /// A unit type
    Unit,
    /// A primitive type
    Primitive(PrimitiveType),
    /// A type provider invocation (only present before expansion)
    Provider {
        provider: Ident,
        args: Vec<ProviderArgument>,
    },
    /// A function type `(T1, T2, ...) -> T`
    Function(Vec<TypeId>, TypeId),
    /// A list type `[T]`
    List(TypeId),
    /// A record type with extensible rows
    Record(RecordRow),
    /// An applied type `Name<T1, T2, ...>`
    App { ctor: Ident, args: Vec<TypeId> },
    /// A type variable (for type inference)
    Var(TypeVar),
    /// Field selector type
    FieldSelector {
        record_ty: TypeId,
        field_ty: TypeId,
        field: Symbol,
    },
}

/// A path to an identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    /// A simple, unqualified identifier: `foo`
    Simple(Symbol),
    /// A qualified path: `std::list::map`
    Qualified(Vec<Symbol>),
    /// A relative path starting with ./ or ../
    Relative { dots: u8, components: Vec<Symbol> },
}

impl Path {
    pub fn simple(sym: Symbol) -> Self {
        Path::Simple(sym)
    }

    pub fn qualified(parts: Vec<Symbol>) -> Self {
        match parts.as_slice() {
            [sym] => Path::Simple(*sym),
            _ => Path::Qualified(parts),
        }
    }

    pub fn item(&self) -> Symbol {
        match self {
            Path::Simple(sym) => *sym,
            Path::Qualified(parts) => *parts.last().expect("BUG: Empty qualified path"),
            Path::Relative { components, .. } => {
                *components.last().expect("BUG: Empty relative path")
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

impl From<Symbol> for Path {
    fn from(sym: Symbol) -> Self {
        Path::Simple(sym)
    }
}

impl From<Vec<Symbol>> for Path {
    fn from(parts: Vec<Symbol>) -> Self {
        Path::qualified(parts)
    }
}

/// Primitive types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Unit,
    Int,
    Float,
    String,
    Bool,
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
            DataType::Float32 | DataType::Float64 => PrimitiveType::Float,
            DataType::String => PrimitiveType::String,
            _ => todo!("Unsupported data type: {:?}", value),
        }
    }
}

/// Provider argument
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProviderArgument {
    Positional(Literal),
    Named { name: Symbol, value: Literal },
    ConstRef(Symbol),
}

impl ProviderArgument {
    pub fn value(&self) -> &Literal {
        match self {
            ProviderArgument::Positional(lit) => lit,
            ProviderArgument::Named { value, .. } => value,
            ProviderArgument::ConstRef(_) => {
                panic!("ConstRef must be resolved before accessing value")
            }
        }
    }
}

/// Row type for extensible records (supports row polymorphism)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordRow {
    /// Empty row
    Empty,
    /// Field extension: (| field :: Type | rest |)
    Extend {
        field: Symbol,
        ty: TypeId,
        rest: Box<RecordRow>,
    },
    /// Row variable for polymorphism
    Var(TypeVar),
}

impl RecordRow {
    /// Convert a vector of fields into a RecordRow
    pub fn from_fields(fields: Vec<(Symbol, TypeId)>) -> Self {
        fields
            .into_iter()
            .rev()
            .fold(RecordRow::Empty, |acc, (field, ty)| RecordRow::Extend {
                field,
                ty,
                rest: Box::new(acc),
            })
    }

    /// Look up a field in the row
    pub fn lookup(&self, field: Symbol) -> Option<TypeId> {
        match self {
            RecordRow::Empty => None,
            RecordRow::Extend { field: f, ty, rest } => {
                if *f == field {
                    Some(*ty)
                } else {
                    rest.lookup(field)
                }
            }
            RecordRow::Var(_) => None,
        }
    }

    /// Convert to a vector of fields (if concrete)
    pub fn to_fields(&self) -> Option<Vec<(Symbol, TypeId)>> {
        match self {
            RecordRow::Empty => Some(Vec::new()),
            RecordRow::Extend { field, ty, rest } => rest.to_fields().map(|mut fields| {
                fields.push((*field, *ty));
                fields
            }),
            RecordRow::Var(_) => None,
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

/// A trait constraint on a type variable
#[derive(Clone, Debug)]
pub struct TraitConstraint {
    pub ty: TypeId,
    pub trait_def_id: DefId,
    pub loc: Loc,
}

/// Polytypes (type schemes) with quantified variables
#[derive(Clone, Debug)]
pub struct Polytype {
    pub forall: Vec<TypeVar>,
    pub constraints: Vec<TraitConstraint>,
    pub ty: TypeId,
}

impl Polytype {
    pub fn mono(ty: TypeId) -> Self {
        Polytype {
            forall: vec![],
            constraints: vec![],
            ty,
        }
    }

    pub fn poly(forall: Vec<TypeVar>, ty: TypeId) -> Self {
        Polytype {
            forall,
            constraints: vec![],
            ty,
        }
    }
}

// =============================================================================
// Attribute support (from AST)
// =============================================================================

/// Attribute annotation on record fields
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: Symbol,
    pub args: Vec<AttributeArg>,
    pub loc: Loc,
}

/// A single argument in an attribute
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
