use crate::ast::Loc;
use crate::ast::Attribute;
pub use crate::common::{Literal, Path, PrimitiveType};
use crate::context::{Arena, DefId, NodeId, Symbol};

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct Ir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
}

impl Ir {
    pub fn string_type(&mut self) -> TypeId {
        self.alloc_type(TypeKind::Primitive(PrimitiveType::String))
    }

    pub fn unit_type(&mut self) -> TypeId {
        self.alloc_type(TypeKind::Unit)
    }

    pub fn int_type(&mut self) -> TypeId {
        self.alloc_type(TypeKind::Primitive(PrimitiveType::Int))
    }

    pub fn bool_type(&mut self) -> TypeId {
        self.alloc_type(TypeKind::Primitive(PrimitiveType::Bool))
    }

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
        self.alloc_type(TypeKind::Named(Ident::Resolved(def_id)))
    }

    pub(crate) fn alloc_type(&mut self, kind: TypeKind) -> TypeId {
        self.types.alloc(Type {
            loc: Loc::generated(),
            kind,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Unresolved(Path),
    Resolved(DefId),
}

impl Ident {
    pub fn def_id(&self) -> DefId {
        match self {
            Ident::Resolved(id) => *id,
            Ident::Unresolved(path) => {
                panic!("Attempted to access DefId of unresolved identifier: {path:?}")
            }
        }
    }

    pub fn try_def_id(&self) -> Option<DefId> {
        match self {
            Ident::Resolved(id) => Some(*id),
            Ident::Unresolved(_) => None,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum TypeRef {
    #[default]
    Unknown,
    Known(TypeId),
}

impl TypeRef {
    pub fn type_id(&self) -> TypeId {
        match self {
            TypeRef::Known(id) => *id,
            TypeRef::Unknown => panic!("Attempted to access TypeId of unknown type"),
        }
    }

    pub fn try_type_id(&self) -> Option<TypeId> {
        match self {
            TypeRef::Known(id) => Some(*id),
            TypeRef::Unknown => None,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CtorParam {
    pub name: Symbol,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StmtKind {
    Let {
        name: Symbol,
        def_id: Option<DefId>,
        value: ExprId,
    },
    Type {
        name: Symbol,
        def_id: Option<DefId>,
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
    pub ty: TypeRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Identifier(Ident),
    Unit,
    Literal(Literal),
    RecordInstance {
        type_ident: Ident,
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
        binding_def: Option<DefId>,
        outputs: Vec<ExprId>,
    },
    FieldAccess {
        expr: ExprId,
        field: Symbol,
    },
    StringInterpolation {
        parts: Vec<Symbol>,
        exprs: Vec<ExprId>,
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
    Named(Ident),
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
