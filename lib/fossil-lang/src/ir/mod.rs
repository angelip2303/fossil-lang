use crate::ast::Loc;
use crate::ast::Attribute;
pub use crate::common::{Literal, Path, PrimitiveType};
use crate::context::{Arena, DefId, NodeId, Symbol};

pub mod resolutions;
pub mod type_index;
pub mod typeck_results;

pub use resolutions::Resolutions;
pub use type_index::{TypeDeclInfo, TypeIndex};
pub use typeck_results::TypeckResults;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Debug)]
pub struct CommonTypes {
    pub int: TypeId,
    pub float: TypeId,
    pub string: TypeId,
    pub bool: TypeId,
    pub unit: TypeId,
}

#[derive(Debug)]
pub struct Ir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
    pub common: CommonTypes,
}

impl Default for Ir {
    fn default() -> Self {
        let mut types = Arena::default();
        let int = types.alloc(Type { loc: Loc::generated(), kind: TypeKind::Primitive(PrimitiveType::Int) });
        let float = types.alloc(Type { loc: Loc::generated(), kind: TypeKind::Primitive(PrimitiveType::Float) });
        let string = types.alloc(Type { loc: Loc::generated(), kind: TypeKind::Primitive(PrimitiveType::String) });
        let bool = types.alloc(Type { loc: Loc::generated(), kind: TypeKind::Primitive(PrimitiveType::Bool) });
        let unit = types.alloc(Type { loc: Loc::generated(), kind: TypeKind::Unit });
        Self {
            stmts: Arena::default(),
            exprs: Arena::default(),
            types,
            root: Vec::new(),
            common: CommonTypes { int, float, string, bool, unit },
        }
    }
}

impl Ir {
    pub fn int_type(&self) -> TypeId { self.common.int }
    pub fn float_type(&self) -> TypeId { self.common.float }
    pub fn string_type(&self) -> TypeId { self.common.string }
    pub fn bool_type(&self) -> TypeId { self.common.bool }
    pub fn unit_type(&self) -> TypeId { self.common.unit }

    pub fn fn_type(&mut self, params: Vec<TypeId>, return_type: TypeId) -> TypeId {
        self.alloc_type(TypeKind::Function(params, return_type))
    }

    pub fn var_type(&mut self, var: TypeVar) -> TypeId {
        self.alloc_type(TypeKind::Var(var))
    }

    pub fn optional_type(&mut self, inner: TypeId) -> TypeId {
        self.alloc_type(TypeKind::Optional(inner))
    }

    pub fn named_type(&mut self, def_id: DefId) -> TypeId {
        self.alloc_type(TypeKind::Named(def_id))
    }

    pub(crate) fn alloc_type(&mut self, kind: TypeKind) -> TypeId {
        self.types.alloc(Type { loc: Loc::generated(), kind })
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtorParam {
    pub name: Symbol,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    Let {
        name: Symbol,
        value: ExprId,
    },
    Type {
        name: Symbol,
        ty: TypeId,
        attrs: Vec<Attribute>,
        ctor_params: Vec<CtorParam>,
    },
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub loc: Loc,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Identifier(Path),
    Unit,
    Literal(Literal),
    RecordInstance {
        type_name: Path,
        ctor_args: Vec<Argument>,
        fields: Vec<(Symbol, ExprId)>,
    },
    Application {
        callee: ExprId,
        args: Vec<Argument>,
    },
    Projection {
        source: ExprId,
        binding: Symbol,
        outputs: Vec<ExprId>,
    },
    Join {
        left: ExprId,
        right: ExprId,
        left_on: Vec<Symbol>,
        right_on: Vec<Symbol>,
        suffix: Option<Symbol>,
    },
    FieldAccess {
        expr: ExprId,
        field: Symbol,
    },
    StringInterpolation {
        parts: Vec<Symbol>,
        exprs: Vec<ExprId>,
    },
    Reference {
        type_name: Path,
        ctor_args: Vec<Argument>,
    },
}

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

#[derive(Debug, Clone)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Named(DefId),
    Unresolved(Path),
    Unit,
    Primitive(PrimitiveType),
    Function(Vec<TypeId>, TypeId),
    Optional(TypeId),
    Record(RecordFields),
    Var(TypeVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct RecordFields {
    pub fields: Vec<(Symbol, TypeId)>,
}

impl RecordFields {
    pub fn from_fields(fields: Vec<(Symbol, TypeId)>) -> Self {
        Self { fields }
    }

    pub fn lookup(&self, field: Symbol) -> Option<TypeId> {
        self.fields
            .iter()
            .find(|(f, _)| *f == field)
            .map(|(_, ty)| *ty)
    }

    pub fn field_names(&self) -> Vec<Symbol> {
        self.fields.iter().map(|(f, _)| *f).collect()
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

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
