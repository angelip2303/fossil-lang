use thiserror::Error;

use crate::ast::{TypeId, TypeVar};
use crate::context::Symbol;

/// Top-level compilation error
#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Resolve(#[from] ResolveError),

    #[error(transparent)]
    TypeGen(#[from] TypeGenError),

    #[error(transparent)]
    TypeCheck(#[from] TypeError),
}

/// Errors from the parsing phase
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unexpected token at {location}: expected {expected}, found {found}")]
    UnexpectedToken {
        location: String,
        expected: String,
        found: String,
    },

    #[error("Unclosed delimiter at {location}: expected {expected}")]
    UnclosedDelimiter { location: String, expected: char },

    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),

    #[error("Invalid string literal: {0}")]
    InvalidString(String),

    #[error("Unexpected end of file")]
    UnexpectedEof,
}

/// Errors from the name resolution phase
#[derive(Error, Debug)]
pub enum ResolveError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined type: {0}")]
    UndefinedType(String),

    #[error("Undefined module: {0}")]
    UndefinedModule(String),

    #[error("Undefined type provider: {0}")]
    UndefinedProvider(String),

    #[error("Not a type provider: {0} is a function, not a type provider")]
    NotAProvider(String),

    #[error("Not a function: {0} is a type provider, not a function")]
    NotAFunction(String),

    #[error("Duplicate definition: {0} is already defined in this scope")]
    DuplicateDefinition(String),
}

/// Errors from the type provider execution phase
#[derive(Error, Debug)]
pub enum TypeGenError {
    #[error(transparent)]
    Provider(#[from] ProviderError),
}

/// Errors from the provider execution phase
#[derive(Error, Debug)]
pub enum ProviderError {
    #[error("Invalid arguments received")]
    InvalidArguments,

    #[error("File not found: {0}")]
    FileNotFound(String),

    #[error("Path is not a file: {0}")]
    NotAFile(String),

    #[error("Invalid file extension: expected .{expected}, found {found}")]
    InvalidExtension { expected: String, found: String },

    #[error(transparent)]
    IO(#[from] std::io::Error),

    #[error(transparent)]
    Polars(#[from] polars::error::PolarsError),
}

/// Errors from the type checking phase
#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected:?}, found {found:?}")]
    TypeMismatch { expected: TypeId, found: TypeId },

    #[error("Record fields in lhs and rhs do not match")]
    RecordFieldMismatch,

    #[error("Record fields' sizes in lhs and rhs do not match")]
    RecordSizeMismatch,

    #[error("Arity mismatch: expected {expected}, found {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("Infinite type: {0}")]
    InfiniteType(TypeVar),

    #[error("Cannot bind type")]
    InvalidBinding,

    #[error("List element cannot of type {0:?}")] // TODO: use display here
    InvalidListElement(TypeId),

    #[error("Record field with name {0:?} cannot of type {1:?}")] // TODO: use display here
    InvalidRecordField(Symbol, TypeId),

    #[error("Expression's type should have been already processed")]
    InternalError,
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error(transparent)]
    Polars(#[from] polars::error::PolarsError),
}
