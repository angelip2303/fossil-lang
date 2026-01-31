//! Unified Intermediate Representation (IR)
//!
//! This IR serves as the single representation throughout the compilation pipeline:
//! - After parsing: identifiers are Unresolved(Path), types are Unknown
//! - After resolution: identifiers become Resolved(DefId), Pipe is desugared
//! - After typechecking: types become Known(TypeId)
//!
//! This design enables in-place mutation and avoids rebuilding the tree at each pass.

use crate::ast::Loc;
use crate::ast::ast::Attribute;
use crate::context::{Arena, DefId, NodeId, Symbol};

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

/// The intermediate representation
#[derive(Default, Debug)]
pub struct Ir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
}

impl Ir {
    /// Create a String type
    pub fn string_type(&mut self) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        })
    }

    /// Create a Unit type
    pub fn unit_type(&mut self) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Unit,
        })
    }

    /// Create an Int type
    pub fn int_type(&mut self) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Int),
        })
    }

    /// Create a Float type
    pub fn float_type(&mut self) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Float),
        })
    }

    /// Create a Bool type
    pub fn bool_type(&mut self) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Bool),
        })
    }

    /// Create a type variable
    pub fn var_type(&mut self, var: TypeVar) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(var),
        })
    }

    /// Create a function type: (params) -> return_type
    pub fn fn_type(&mut self, params: Vec<TypeId>, return_type: TypeId) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(params, return_type),
        })
    }

    /// Create a List type with the given element type
    pub fn list_type(&mut self, elem_type: TypeId) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::List(elem_type),
        })
    }

    /// Create a Named type referencing a defined type by its DefId
    pub fn named_type(&mut self, def_id: DefId) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Named(Ident::Resolved(def_id)),
        })
    }
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
    /// A type definition `type name = type` with optional type-level attributes
    Type {
        name: Symbol,
        ty: TypeId,
        attrs: Vec<Attribute>,
    },
    /// An expression statement `expr`
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub kind: ExprKind,
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
    /// A named record construction `TypeName { @id = ..., field = value, ... }`
    NamedRecordConstruction {
        /// Identifier for the type (resolved to DefId after resolution)
        type_ident: Ident,
        /// Named fields (field_name, value_expr)
        fields: Vec<(Symbol, ExprId)>,
        /// Meta-fields (@name = expr) - keys are names without the @ prefix
        meta_fields: Vec<(Symbol, ExprId)>,
    },
    /// A function definition `fn (param1, param2, ...) -> expr`
    Function {
        params: Vec<Param>,
        body: ExprId,
        attrs: Vec<Attribute>,
    },
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
    /// A record type with named fields (no row polymorphism)
    Record(RecordFields),
    /// An applied type `Name<T1, T2, ...>`
    App { ctor: Ident, args: Vec<TypeId> },
    /// A type variable (for type inference)
    Var(TypeVar),
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

/// Provider argument
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProviderArgument {
    Positional(Literal),
    Named { name: Symbol, value: Literal },
}

impl ProviderArgument {
    pub fn value(&self) -> &Literal {
        match self {
            ProviderArgument::Positional(lit) => lit,
            ProviderArgument::Named { value, .. } => value,
        }
    }
}

/// Simplified record type - just a map from field names to types
/// No row polymorphism - records are closed/concrete.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct RecordFields {
    pub fields: Vec<(Symbol, TypeId)>,
}

impl RecordFields {
    /// Create from a vector of fields
    pub fn from_fields(fields: Vec<(Symbol, TypeId)>) -> Self {
        Self { fields }
    }

    /// Look up a field by name
    pub fn lookup(&self, field: Symbol) -> Option<TypeId> {
        self.fields
            .iter()
            .find(|(f, _)| *f == field)
            .map(|(_, ty)| *ty)
    }

    /// Convert to a vector of fields
    pub fn to_fields(&self) -> Vec<(Symbol, TypeId)> {
        self.fields.clone()
    }

    /// Check if the record has a field
    pub fn contains(&self, field: Symbol) -> bool {
        self.fields.iter().any(|(f, _)| *f == field)
    }

    /// Get all field names
    pub fn field_names(&self) -> Vec<Symbol> {
        self.fields.iter().map(|(f, _)| *f).collect()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Number of fields
    pub fn len(&self) -> usize {
        self.fields.len()
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

/// Polytypes (type schemes) with quantified variables
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
