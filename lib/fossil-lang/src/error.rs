//! Error types and reporting
//!
//! This module defines the error types used throughout the compiler and provides
//! functionality for generating user-friendly error reports using the Ariadne library.
//!
//! ## Error Structure
//!
//! Errors consist of three main components:
//! - **Kind**: The specific type of error (type mismatch, undefined variable, etc.)
//! - **Location**: Source location where the error occurred
//! - **Context**: Optional additional information to help users understand the error
//!
//! ## Example
//!
//! ```rust,ignore
//! CompileError::new(
//!     CompileErrorKind::UndefinedVariable { name },
//!     loc,
//! )
//! .with_context(format!("Variable '{}' is not defined", name))
//! ```

use ariadne::{Label, Report, ReportKind};

use crate::{
    ast::{Loc, ast::Path, thir::TypeId},
    context::Symbol,
};

/// A compilation error with location and optional context
///
/// This is the main error type used throughout the compiler. It includes:
/// - The specific error kind
/// - Source location information for reporting
/// - Optional contextual message to help users
#[derive(Debug)]
pub struct CompileError {
    /// The specific kind of error that occurred
    pub kind: CompileErrorKind,
    /// Location in source code where the error occurred
    pub loc: Loc,
    /// Optional context message to help users understand the error
    pub context: Option<String>,
}

/// Collection of compilation errors
///
/// This type is used to accumulate multiple errors during compilation
/// instead of failing on the first error. This provides a better user
/// experience by showing all errors at once.
///
/// # Example
/// ```rust,ignore
/// let mut errors = CompileErrors::new();
/// for item in items {
///     match process(item) {
///         Ok(result) => results.push(result),
///         Err(e) => errors.push(e),
///     }
/// }
/// errors.into_result(results)
/// ```
#[derive(Debug)]
pub struct CompileErrors(pub Vec<CompileError>);

impl CompileErrors {
    /// Create a new empty error collection
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Add an error to the collection
    pub fn push(&mut self, error: CompileError) {
        self.0.push(error);
    }

    /// Check if there are any errors
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Convert to a Result, returning Ok if no errors, Err otherwise
    ///
    /// This is a convenience method for converting an error accumulation
    /// into a Result type.
    pub fn into_result<T>(self, ok: T) -> Result<T, Self> {
        if self.is_empty() {
            Ok(ok)
        } else {
            Err(self)
        }
    }

    /// Generate Ariadne reports for all errors
    ///
    /// Returns a vector of reports that can be printed or written to show
    /// all accumulated errors to the user.
    pub fn reports(&self) -> Vec<Report<'_, Loc>> {
        self.0.iter().map(|e| e.report()).collect()
    }
}

impl Default for CompileErrors {
    fn default() -> Self {
        Self::new()
    }
}

impl From<CompileError> for CompileErrors {
    fn from(err: CompileError) -> Self {
        Self(vec![err])
    }
}

/// The specific kind of compilation error
///
/// Each variant represents a different category of error that can occur
/// during compilation, from parsing through type checking.
#[derive(Clone, Debug)]
pub enum CompileErrorKind {
    /// Parsing error with message
    Parse(Symbol),

    /// Type mismatch during unification
    TypeMismatch {
        /// The expected type
        expected: TypeId,
        /// The actual type encountered
        actual: TypeId
    },

    /// Reference to an undefined variable
    UndefinedVariable {
        /// Name of the undefined variable
        name: Symbol
    },

    /// Reference to an undefined path (qualified name)
    UndefinedPath {
        /// The path that could not be resolved
        path: Path
    },

    /// Function called with wrong number of arguments
    ArityMismatch {
        /// Expected number of arguments
        expected: usize,
        /// Actual number of arguments provided
        actual: usize
    },

    /// Attempt to redefine an already-defined name
    AlreadyDefined(Symbol),

    /// Reference to an undefined type
    UndefinedType(Path),

    /// Reference to an undefined module
    UndefinedModule(Path),

    /// Reference to an undefined type provider
    UndefinedProvider(Path),

    /// Attempted to import something that is not a module or provider
    NotAModule(Path),

    /// Attempted to call a non-function value
    NotAFunction(Symbol),

    /// Expected a provider but got something else
    NotAProvider(Path),

    /// Invalid binding pattern
    InvalidBinding,

    /// Record types have different numbers of fields
    RecordSizeMismatch,

    /// Record types have incompatible fields
    RecordFieldMismatch,

    /// Type variable occurs in the type it's being unified with (infinite type)
    InfiniteType(TypeVar),

    /// List element has incompatible type
    InvalidListElement(TypeId),

    /// Record field has invalid type
    InvalidRecordField(Symbol, TypeId),

    /// Error from type provider
    ProviderError(Symbol),

    /// Runtime error with message string (avoids needing Symbol/Interner in runtime code)
    Runtime(String),
}

/// A type variable used in error messages
///
/// Type variables are displayed as 't0, 't1, etc.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

impl CompileError {
    /// Create a new compilation error
    ///
    /// # Arguments
    /// * `kind` - The specific error kind
    /// * `loc` - Source location where the error occurred
    ///
    /// # Example
    /// ```rust,ignore
    /// CompileError::new(
    ///     CompileErrorKind::UndefinedVariable { name },
    ///     loc
    /// )
    /// ```
    pub fn new(kind: CompileErrorKind, loc: Loc) -> Self {
        Self {
            kind,
            loc,
            context: None,
        }
    }

    /// Add contextual information to this error
    ///
    /// This is useful for providing additional details about what went wrong.
    /// The context will be displayed in the error report.
    ///
    /// # Example
    /// ```rust,ignore
    /// error.with_context("While checking function application")
    /// ```
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    /// Get a human-readable error message
    ///
    /// Returns a user-friendly description of the error based on its kind.
    pub fn message(&self) -> String {
        use CompileErrorKind::*;
        match &self.kind {
            Parse(msg) => format!("Parse error: {:?}", msg),
            TypeMismatch { .. } => "Type mismatch".to_string(),
            UndefinedVariable { name } => format!("Undefined variable '{:?}'", name),
            UndefinedPath { path } => format!("Undefined path '{:?}'", path),
            ArityMismatch { expected, actual } => {
                format!("Arity mismatch: expected {} arguments, got {}", expected, actual)
            }
            AlreadyDefined(name) => format!("Name '{:?}' is already defined", name),
            UndefinedType(path) => format!("Undefined type '{:?}'", path),
            UndefinedModule(path) => format!("Undefined module '{:?}'", path),
            UndefinedProvider(path) => format!("Undefined provider '{:?}'", path),
            NotAModule(path) => format!("'{:?}' is not a module or provider", path),
            NotAFunction(name) => format!("'{:?}' is not a function", name),
            NotAProvider(path) => format!("'{:?}' is not a provider", path),
            InvalidBinding => "Invalid binding".to_string(),
            RecordSizeMismatch => "Record size mismatch".to_string(),
            RecordFieldMismatch => "Record field mismatch".to_string(),
            InfiniteType(var) => format!("Infinite type detected: {}", var),
            InvalidListElement(_) => "Invalid list element type".to_string(),
            InvalidRecordField(field, _) => format!("Invalid record field '{:?}'", field),
            ProviderError(_) => "Provider error".to_string(),
            Runtime(msg) => msg.clone(),
        }
    }

    /// Generate an Ariadne error report
    ///
    /// Creates a formatted error report using the Ariadne library that can be
    /// displayed to users with nice formatting, source snippets, and colors.
    ///
    /// # Returns
    /// An Ariadne `Report` that can be printed or written to a file.
    pub fn report(&self) -> Report<'_, Loc> {
        let mut report = Report::build(ReportKind::Error, self.loc.clone())
            .with_message(self.message());

        // Add main label
        let mut label = Label::new(self.loc.clone());

        // Add context if available
        if let Some(ctx) = &self.context {
            label = label.with_message(ctx);
        }

        report = report.with_label(label);
        report.finish()
    }
}

// Type aliases for compatibility
pub type ParseError = CompileError;
pub type TypeError = CompileError;
pub type RuntimeError = CompileError;
pub type ProviderError = CompileError;

// Conversion from PolarsError to CompileError
impl From<polars::error::PolarsError> for CompileError {
    fn from(err: polars::error::PolarsError) -> Self {
        // We'll create a synthetic error with a provider error kind
        // Unfortunately we don't have access to an interner here, so we use a synthetic symbol
        CompileError::new(
            CompileErrorKind::ProviderError(Symbol::synthetic()),
            crate::ast::Loc::generated(),
        )
        .with_context(format!("Polars error: {}", err))
    }
}

// Conversion from std::io::Error to CompileError
impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::new(
            CompileErrorKind::ProviderError(Symbol::synthetic()),
            crate::ast::Loc::generated(),
        )
        .with_context(format!("IO error: {}", err))
    }
}
