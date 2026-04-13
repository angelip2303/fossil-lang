use crate::ast::Loc;
use crate::ast::Attribute;
pub use crate::common::{Literal, Path, PrimitiveType};
use crate::db::{DefId, Symbol};
use la_arena::{Arena, Idx as NodeId};

pub mod resolutions;

pub use resolutions::Resolutions;

// Type-checker outputs (`TypeIndex`, `TypeDeclInfo`, `TypeckResults`) live in
// [`crate::ty::typecheck::results`] now — they speak in terms of interned
// `Ty` instead of per-IR `TypeId` arena indices. Type-level concepts such as
// `Polytype`, `TypeVar` and the unification-time `Function` / `Var`
// variants likewise live in [`crate::ty::types`]; the IR keeps only the
// concrete shapes the lowering pass can produce.

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Ir {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
        spread: Option<ExprId>,
        fields: Vec<(Symbol, ExprId)>,
    },
    Application {
        callee: ExprId,
        args: Vec<Argument>,
        /// Resolved type arguments for generic calls: `f<Type1, Type2>(args)`
        type_args: Vec<crate::db::DefId>,
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
    /// `expr ?? default` — null coalescing operator.
    Coalesce {
        value: ExprId,
        default: ExprId,
    },
    /// `ref Type(args)` — explicit reference to another record type.
    Ref {
        type_name: Path,
        args: Vec<ExprId>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeKind,
}

/// Surface-syntax type kinds. The lowering pass produces these from the AST;
/// the type checker re-interns them into [`crate::ty::types::Ty`] before
/// inference. There are deliberately no `Function` / `Var` variants here:
/// function signatures are constructed directly by the type checker, and
/// type variables are introduced only by inference.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Named(DefId),
    Unit,
    Primitive(PrimitiveType),
    Optional(TypeId),
    Record(RecordFields),
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
