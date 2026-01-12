use polars::prelude::DataType;

use crate::ast::Loc;
use crate::ast::ast::{Literal, Path, PrimitiveType};
use crate::ast::hir::Param;
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct TypedHir {
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
    pub loc: Loc,
    pub kind: ExprKind,
    pub ty: TypeId,
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
    /// Represents generic types like `List<T>`, `Entity<Person>`, `Map<K, V>`.
    ///
    /// # Examples
    ///
    /// - `List<Int>` → `App { ctor: list_def_id, args: [int_type_id] }`
    /// - `Entity<Person>` → `App { ctor: entity_def_id, args: [person_type_id] }`
    /// - `Map<String, Int>` → `App { ctor: map_def_id, args: [string_type_id, int_type_id] }`
    ///
    /// The `ctor` field is a DefId that must be registered as a type constructor
    /// in `GlobalContext::type_constructors`.
    App {
        /// DefId of the type constructor (e.g., List, Entity, Option)
        ctor: DefId,
        /// Type arguments to apply to the constructor
        args: Vec<TypeId>,
    },
    /// A record type with extensible rows `{ field: T, field: T, ... }`
    Record(RecordRow),
    /// A type variable (for type inference)
    Var(TypeVar),
}

/// Row type for extensible records (supports row polymorphism)
///
/// Enables polymorphic field access like `fn(r) -> r.name` where r can be
/// any record containing a `name` field.
///
/// Based on "Extensible records with scoped labels" (Leijen, 2005)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordRow {
    /// Empty row: (| |)
    Empty,

    /// Field extension: (| field :: Type | rest |)
    Extend {
        field: Symbol,
        ty: TypeId,
        rest: Box<RecordRow>,
    },

    /// Row variable for polymorphism: (| ...r |)
    Var(TypeVar),
}

impl RecordRow {
    /// Converts a vector of fields into a RecordRow
    ///
    /// # Example
    /// ```no_run
    /// # use fossil_lang::ast::thir::{RecordRow, TypeId};
    /// # use fossil_lang::context::Symbol;
    /// # let field1 = Symbol::synthetic();
    /// # let field2 = Symbol::synthetic();
    /// # let ty1 = TypeId::new(0);
    /// # let ty2 = TypeId::new(1);
    /// let fields = vec![(field1, ty1), (field2, ty2)];
    /// let row = RecordRow::from_fields(fields);
    /// // Results in: Extend(field2, ty2, Extend(field1, ty1, Empty))
    /// ```
    pub fn from_fields(fields: Vec<(Symbol, TypeId)>) -> Self {
        fields.into_iter().rev().fold(RecordRow::Empty, |acc, (field, ty)| {
            RecordRow::Extend {
                field,
                ty,
                rest: Box::new(acc),
            }
        })
    }

    /// Looks up a field in the row, returning its type if found
    ///
    /// # Arguments
    /// * `field` - The field name to look up
    ///
    /// # Returns
    /// * `Some(TypeId)` - If the field exists in the row
    /// * `None` - If the field doesn't exist or if the row is a variable
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

    /// Converts the row back to a vector of fields (if concrete)
    ///
    /// Returns None if the row contains a type variable.
    pub fn to_fields(&self) -> Option<Vec<(Symbol, TypeId)>> {
        match self {
            RecordRow::Empty => Some(Vec::new()),
            RecordRow::Extend { field, ty, rest } => {
                rest.to_fields().map(|mut fields| {
                    fields.push((*field, *ty));
                    fields
                })
            }
            RecordRow::Var(_) => None,
        }
    }
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
