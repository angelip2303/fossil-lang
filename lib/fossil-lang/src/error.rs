use ariadne::{Label, Report, ReportKind};

use crate::{
    ast::{Loc, ast::Path, thir::TypeId},
    context::Symbol,
};

#[derive(Debug)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub loc: Loc,
}

#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    Parse(Symbol),

    TypeMismatch { expected: TypeId, actual: TypeId },

    UndefinedVariable { name: Symbol },

    UndefinedPath { path: Path },

    ArityMismatch { expected: usize, actual: usize },

    AlreadyDefined(Symbol),

    UndefinedType(Path),

    UndefinedModule(Path),

    UndefinedProvider(Path),

    NotAFunction(Symbol),
    NotAProvider(Path),
    InvalidBinding,
    RecordSizeMismatch,
    RecordFieldMismatch,
    InfiniteType(TypeVar),
    InvalidListElement(TypeId),
    InvalidRecordField(Symbol, TypeId),
    ProviderError(Symbol),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

impl CompileError {
    pub fn new(kind: CompileErrorKind, loc: Loc) -> Self {
        Self { kind, loc }
    }

    pub fn report(&self) -> Report<'_, Loc> {
        Report::build(ReportKind::Error, self.loc.clone())
            .with_message(format!("{:?}", self.kind))
            .with_label(Label::new(self.loc.clone()))
            .finish()
    }
}

// Type aliases for compatibility
pub type ParseError = CompileError;
pub type TypeError = CompileError;
pub type RuntimeError = CompileError;
pub type ProviderError = CompileError;
