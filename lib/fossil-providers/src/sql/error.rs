//! SQL Provider error types

use std::fmt;

use fossil_lang::ast::Loc;
use fossil_lang::error::{CompileErrorKind, ProviderError};

/// SQL-specific errors
#[derive(Debug)]
pub enum SqlError {
    /// Connection string is missing or invalid
    InvalidConnectionString(String),
    /// Failed to connect to database
    ConnectionFailed(String),
    /// Failed to infer schema
    SchemaInferenceError(String),
    /// Invalid SQL query
    InvalidQuery(String),
    /// Unsupported database type
    UnsupportedDatabase(String),
    /// Runtime query execution error
    QueryError(String),
    /// Configuration parsing error
    ConfigError(String),
}

impl fmt::Display for SqlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SqlError::InvalidConnectionString(msg) => write!(f, "Invalid connection string: {}", msg),
            SqlError::ConnectionFailed(msg) => write!(f, "Failed to connect to database: {}", msg),
            SqlError::SchemaInferenceError(msg) => write!(f, "Failed to infer schema: {}", msg),
            SqlError::InvalidQuery(msg) => write!(f, "Invalid SQL query: {}", msg),
            SqlError::UnsupportedDatabase(msg) => write!(f, "Unsupported database type: {}", msg),
            SqlError::QueryError(msg) => write!(f, "Query execution error: {}", msg),
            SqlError::ConfigError(msg) => write!(f, "Configuration error: {}", msg),
        }
    }
}

impl std::error::Error for SqlError {}

impl SqlError {
    /// Convert to a ProviderError for compile-time errors
    pub fn to_provider_error(self, interner: &mut fossil_lang::context::Interner) -> ProviderError {
        let msg = interner.intern(&self.to_string());
        ProviderError::new(CompileErrorKind::ProviderError(msg), Loc::generated())
    }
}

impl From<sqlx::Error> for SqlError {
    fn from(err: sqlx::Error) -> Self {
        SqlError::QueryError(err.to_string())
    }
}
