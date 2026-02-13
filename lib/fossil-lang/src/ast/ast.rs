use crate::ast::Loc;
pub use crate::common::{Literal, Path, PrimitiveType, ProviderArgument};
use crate::context::*;

pub type StmtId = NodeId<Stmt>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

#[derive(Default, Debug)]
pub struct Ast {
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub root: Vec<StmtId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub loc: Loc,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstructorParam {
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
        ctor_params: Vec<ConstructorParam>,
    },
    Expr(ExprId),
}

#[derive(Debug)]
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
        type_path: Path,
        ctor_args: Vec<Argument>,
        fields: Vec<(Symbol, ExprId)>,
    },
    Application {
        callee: ExprId,
        args: Vec<Argument>,
    },
    Projection {
        source: ExprId,
        param: Symbol,
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
    ProviderInvocation {
        provider: Path,
        args: Vec<ProviderArgument>,
    },
}

#[derive(Debug)]
pub struct Type {
    pub loc: Loc,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Named(Path),
    Unit,
    Primitive(PrimitiveType),
    Provider {
        provider: Path,
        args: Vec<ProviderArgument>,
    },
    Optional(TypeId),
    Record(Vec<RecordField>),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: Symbol,
    pub args: Vec<AttributeArg>,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeArg {
    Named { key: Symbol, value: Literal },
    Positional(Literal),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub name: Symbol,
    pub ty: TypeId,
    pub attrs: Vec<Attribute>,
}
