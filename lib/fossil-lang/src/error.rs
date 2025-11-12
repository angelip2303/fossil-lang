use thiserror::Error;

use crate::checked::{TypeId, TypeVar};
use crate::traits::provider::ProviderError;

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined module: {0}")]
    UndefinedModule(String),
}

/// Errors from the lowering phase
#[derive(Error, Debug)]
pub enum LowerError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined type: {0}")]
    UndefinedType(String),

    #[error("Not a type provider: {0}")]
    NotAProvider(String),

    #[error(transparent)]
    Registry(#[from] RegistryError),
}

/// Errors from the type generation phase
#[derive(Error, Debug)]
pub enum TypeGenError {
    #[error(transparent)]
    ProviderError(#[from] ProviderError),
}

/// Errors from the type checking phase
#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Illegal recursive type: {0} appears in itself")]
    RecursiveType(TypeVar),

    #[error("Type mismatch: cannot unify types at {:?} and {:?}", .expected, .found)]
    TypeMismatch { expected: TypeId, found: TypeId },

    #[error("Arity mismatch: expected {expected} arguments, got {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
}

/// Overall compilation errors
#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    Lower(#[from] LowerError),

    #[error(transparent)]
    TypeGen(#[from] TypeGenError),

    #[error(transparent)]
    TypeCheck(#[from] TypeError),
}
