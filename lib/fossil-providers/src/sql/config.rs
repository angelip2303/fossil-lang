//! SQL Provider configuration parsing

use fossil_lang::ast::ast::{Literal, ProviderArgument};
use fossil_lang::ast::Loc;
use fossil_lang::context::Interner;
use fossil_lang::error::{CompileErrorKind, ProviderError};

use super::error::SqlError;

/// Supported database types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DatabaseType {
    PostgreSQL,
    SQLite,
    MySQL,
}

impl DatabaseType {
    /// Detect database type from connection string
    pub fn from_connection_string(conn_str: &str) -> Result<Self, SqlError> {
        if conn_str.starts_with("postgres://") || conn_str.starts_with("postgresql://") {
            Ok(DatabaseType::PostgreSQL)
        } else if conn_str.starts_with("sqlite://") || conn_str.starts_with("sqlite:") {
            Ok(DatabaseType::SQLite)
        } else if conn_str.starts_with("mysql://") {
            Ok(DatabaseType::MySQL)
        } else {
            Err(SqlError::InvalidConnectionString(format!(
                "Unsupported database scheme. Expected postgres://, sqlite://, or mysql://, got: {}",
                conn_str.split("://").next().unwrap_or("unknown")
            )))
        }
    }
}

/// Source for SQL data - either a table name or a custom query
#[derive(Debug, Clone)]
pub enum SqlSource {
    /// Load from a table by name
    Table(String),
    /// Execute a custom SQL query
    Query(String),
}

impl SqlSource {
    /// Parse from a string - if it looks like SQL, treat as query, otherwise as table name
    pub fn parse(s: &str) -> Self {
        let upper = s.trim().to_uppercase();
        if upper.starts_with("SELECT ") || upper.starts_with("WITH ") {
            SqlSource::Query(s.to_string())
        } else {
            SqlSource::Table(s.to_string())
        }
    }

    /// Get the SQL query to execute
    pub fn to_query(&self) -> String {
        match self {
            SqlSource::Table(name) => format!("SELECT * FROM {}", name),
            SqlSource::Query(sql) => sql.clone(),
        }
    }
}

/// Configuration for SQL provider
#[derive(Debug, Clone)]
pub struct SqlConfig {
    /// Database connection string
    pub connection_string: String,
    /// Type of database (auto-detected from connection string)
    pub db_type: DatabaseType,
    /// Source - table name or custom query
    pub source: SqlSource,
    /// Batch size for streaming (default: 1000)
    pub batch_size: usize,
    /// Connection timeout in seconds (default: 30)
    pub connect_timeout: u64,
}

impl Default for SqlConfig {
    fn default() -> Self {
        Self {
            connection_string: String::new(),
            db_type: DatabaseType::PostgreSQL,
            source: SqlSource::Table(String::new()),
            batch_size: 1000,
            connect_timeout: 30,
        }
    }
}

/// Parse SQL provider arguments from provider arguments (positional or named)
///
/// Syntax: `sql!("connection_string", "table_or_query", batch_size: 1000, timeout: 30)`
/// Or named: `sql!(url: "connection_string", source: "table", batch_size: 1000)`
pub fn parse_sql_config(
    args: &[ProviderArgument],
    interner: &mut Interner,
) -> Result<SqlConfig, ProviderError> {
    if args.is_empty() {
        return Err(ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("SQL provider requires at least a connection string argument"),
            ),
            Loc::generated(),
        ));
    }

    let mut config = SqlConfig::default();

    // Symbols for named parameters
    let url_sym = interner.intern("url");
    let source_sym = interner.intern("source");
    let batch_size_sym = interner.intern("batch_size");
    let timeout_sym = interner.intern("timeout");

    // First pass: collect named arguments
    for arg in args {
        if let ProviderArgument::Named { name, value } = arg {
            if *name == url_sym {
                if let Literal::String(sym) = value {
                    config.connection_string = interner.resolve(*sym).to_string();
                }
            } else if *name == source_sym {
                if let Literal::String(sym) = value {
                    config.source = SqlSource::parse(interner.resolve(*sym));
                }
            } else if *name == batch_size_sym {
                if let Literal::Integer(n) = value {
                    config.batch_size = *n as usize;
                }
            } else if *name == timeout_sym {
                if let Literal::Integer(n) = value {
                    config.connect_timeout = *n as u64;
                }
            }
        }
    }

    // Second pass: handle positional arguments (only if not set by named)
    if config.connection_string.is_empty() {
        let mut positional_idx = 0;
        for arg in args {
            if let ProviderArgument::Positional(lit) = arg {
                match positional_idx {
                    0 => {
                        // First positional: connection string
                        if let Literal::String(sym) = lit {
                            config.connection_string = interner.resolve(*sym).to_string();
                        } else {
                            return Err(ProviderError::new(
                                CompileErrorKind::ProviderError(
                                    interner.intern("SQL provider connection string must be a string literal"),
                                ),
                                Loc::generated(),
                            ));
                        }
                    }
                    1 => {
                        // Second positional: source (table/query)
                        if let Literal::String(sym) = lit {
                            config.source = SqlSource::parse(interner.resolve(*sym));
                        }
                    }
                    2 => {
                        // Third positional: batch_size
                        if let Literal::Integer(n) = lit {
                            config.batch_size = *n as usize;
                        }
                    }
                    3 => {
                        // Fourth positional: timeout
                        if let Literal::Integer(n) = lit {
                            config.connect_timeout = *n as u64;
                        }
                    }
                    _ => {}
                }
                positional_idx += 1;
            }
        }
    }

    // Validate connection string was provided
    if config.connection_string.is_empty() {
        return Err(ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("SQL provider requires a connection string"),
            ),
            Loc::generated(),
        ));
    }

    // Detect database type
    config.db_type = DatabaseType::from_connection_string(&config.connection_string)
        .map_err(|e| e.to_provider_error(interner))?;

    Ok(config)
}
