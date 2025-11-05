use thiserror::Error;

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Error, Debug, Clone)]
pub enum CompileError {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch { expected: String, found: String },

    #[error("Unbound variable: {0}")]
    UnboundVariable(String),

    #[error("Occurs check failed: cannot construct infinite type")]
    OccursCheckFailed,

    #[error("Field not found: {0}")]
    FieldNotFound(String),

    #[error("Expected function, found {0}")]
    ExpectedFunction(String),

    #[error("Expected list, found {0}")]
    ExpectedList(String),

    #[error("Expected record, found {0}")]
    ExpectedRecord(String),

    #[error("Invalid argument count: expected {expected}, got {actual}")]
    InvalidArgumentCount { expected: usize, actual: usize },

    #[error("Cannot create list from empty vector")]
    EmptyList,

    #[error("Heterogeneous list types")]
    HeterogeneousTypes,

    #[error("Record schema mismatch")]
    RecordSchemaMismatch,

    #[error("Unsupported type for operation")]
    UnsupportedType,

    #[error("Provider not found: {0}")]
    ProviderNotFound(String),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Polars error: {0}")]
    PolarsError(String),
}

impl From<polars::error::PolarsError> for CompileError {
    fn from(e: polars::error::PolarsError) -> Self {
        CompileError::PolarsError(e.to_string())
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::IoError(e.to_string())
    }
}
