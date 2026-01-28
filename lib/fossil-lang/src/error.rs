//! Error types and reporting
//!
//! This module defines the error types used throughout the compiler and provides
//! functionality for generating user-friendly error reports using the Ariadne library.

use ariadne::{Label, Report, ReportKind};
use thiserror::Error;

use crate::{
    ast::{Loc, ast::Path},
    context::{Interner, Symbol},
    ir::TypeId,
};

/// Format a Symbol using the interner to get the actual name.
fn format_symbol(sym: Symbol, interner: &Interner) -> String {
    interner.try_resolve(sym).unwrap_or("<unknown>").to_string()
}

/// Format a Path using the interner to get readable names
fn format_path(path: &Path, interner: &Interner) -> String {
    match path {
        Path::Simple(sym) => interner
            .try_resolve(*sym)
            .unwrap_or("<unknown>")
            .to_string(),
        Path::Qualified(parts) => parts
            .iter()
            .map(|s| interner.try_resolve(*s).unwrap_or("<unknown>"))
            .collect::<Vec<_>>()
            .join("::"),
        Path::Relative { dots, components } => {
            let prefix = if *dots == 0 {
                "./".to_string()
            } else {
                "../".repeat(*dots as usize)
            };
            let path_str = components
                .iter()
                .map(|s| interner.try_resolve(*s).unwrap_or("<unknown>"))
                .collect::<Vec<_>>()
                .join("::");
            format!("{}{}", prefix, path_str)
        }
    }
}

/// A compilation error with location and optional context
#[derive(Debug, Error)]
#[error("{kind:?}")]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub loc: Loc,
    pub context: Option<String>,
    pub suggestions: Vec<ErrorSuggestion>,
}

/// Collection of compilation errors
#[derive(Debug)]
pub struct CompileErrors(pub Vec<CompileError>);

impl CompileErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, error: CompileError) {
        self.0.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn into_result<T>(self, ok: T) -> Result<T, Self> {
        if self.is_empty() { Ok(ok) } else { Err(self) }
    }

    pub fn reports(&self, interner: &Interner) -> Vec<Report<'_, Loc>> {
        self.0.iter().map(|e| e.report(interner)).collect()
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

/// Suggestion for fixing a compilation error
#[derive(Debug, Clone)]
pub enum ErrorSuggestion {
    DidYouMean {
        wrong: String,
        suggestion: String,
        confidence: f32,
    },
    AddTypeAnnotation {
        name: String,
        suggested_type: String,
    },
    FixTypo {
        wrong: String,
        correct: String,
        explanation: String,
    },
    Help(String),
}

impl ErrorSuggestion {
    pub fn format(&self) -> String {
        match self {
            Self::DidYouMean { wrong, suggestion, confidence } => {
                if *confidence > 0.8 {
                    format!("Did you mean '{}'?", suggestion)
                } else {
                    format!("Did you mean '{}' (similar to '{}')?", suggestion, wrong)
                }
            }
            Self::AddTypeAnnotation { name, suggested_type } => {
                format!("Add type annotation: let {}: {} = ...", name, suggested_type)
            }
            Self::FixTypo { wrong, correct, explanation } => {
                format!("Replace '{}' with '{}': {}", wrong, correct, explanation)
            }
            Self::Help(msg) => msg.clone(),
        }
    }
}

/// The specific kind of compilation error
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
    NotAModule(Path),
    NotAFunction(Symbol),
    NotAProvider(Path),
    InvalidBinding,
    RecordSizeMismatch,
    RecordFieldMismatch,
    InfiniteType(TypeVar),
    InvalidListElement(TypeId),
    InvalidRecordField(Symbol, TypeId),
    Provider(ProviderErrorKind),
    Runtime(String),
    InternalCompilerError { phase: &'static str, message: String },
}

// =============================================================================
// Provider Errors
// =============================================================================

/// Specific error types for type providers (csv!, shex!, etc.)
///
/// Using an enum instead of strings provides:
/// - Type safety: no typos in error messages
/// - Consistency: same error always has same message
/// - Extensibility: easy to add new error types
/// - Documentation: the enum documents what errors are possible
#[derive(Clone, Debug, Error)]
pub enum ProviderErrorKind {
    // -------------------------------------------------------------------------
    // Argument Errors
    // -------------------------------------------------------------------------
    #[error("{provider} provider requires {name} argument")]
    MissingArgument {
        name: &'static str,
        provider: &'static str,
    },

    #[error("{name} argument must be {expected}")]
    InvalidArgumentType {
        name: &'static str,
        expected: &'static str,
    },

    // -------------------------------------------------------------------------
    // File Errors
    // -------------------------------------------------------------------------
    #[error("File not found: {path}")]
    FileNotFound { path: String },

    #[error("Not a file: {path}")]
    NotAFile { path: String },

    #[error("Invalid file extension '{found}', expected: {expected}")]
    InvalidExtension { found: String, expected: String },

    #[error("Failed to read {path}: {cause}")]
    ReadError { path: String, cause: String },

    // -------------------------------------------------------------------------
    // Parse Errors
    // -------------------------------------------------------------------------
    #[error("Failed to parse {format}: {cause}")]
    ParseError {
        format: &'static str,
        cause: String,
    },

    // -------------------------------------------------------------------------
    // Schema Errors (ShEx specific)
    // -------------------------------------------------------------------------
    #[error("Schema has no shapes defined")]
    NoShapesDefined,

    #[error("Shape '{name}' not found in schema")]
    ShapeNotFound { name: String },

    // -------------------------------------------------------------------------
    // Polars/Data Errors
    // -------------------------------------------------------------------------
    #[error("Data error: {0}")]
    DataError(String),

    // -------------------------------------------------------------------------
    // Fallback for edge cases
    // -------------------------------------------------------------------------
    #[error("{0}")]
    Custom(String),
}

/// A type variable used in error messages
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

impl CompileError {
    pub fn new(kind: CompileErrorKind, loc: Loc) -> Self {
        Self {
            kind,
            loc,
            context: None,
            suggestions: Vec::new(),
        }
    }

    pub fn with_suggestion(mut self, suggestion: ErrorSuggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    pub fn internal(phase: &'static str, message: impl Into<String>, loc: Loc) -> Self {
        Self::new(
            CompileErrorKind::InternalCompilerError {
                phase,
                message: message.into(),
            },
            loc,
        )
    }

    /// Get a human-readable error message with proper symbol resolution
    pub fn message(&self, interner: &Interner) -> String {
        use CompileErrorKind::*;

        // For errors with context, prefer the context as it contains pre-formatted details
        match &self.kind {
            Parse(msg) => format!("Parse error: {}", format_symbol(*msg, interner)),
            TypeMismatch { .. } => {
                if let Some(ctx) = &self.context {
                    ctx.clone()
                } else {
                    "Type mismatch".to_string()
                }
            }
            UndefinedVariable { name } => {
                format!("Undefined variable '{}'", format_symbol(*name, interner))
            }
            UndefinedPath { path } => {
                format!("Undefined path '{}'", format_path(path, interner))
            }
            ArityMismatch { expected, actual } => {
                format!("Expected {} arguments, got {}", expected, actual)
            }
            AlreadyDefined(name) => {
                format!("Name '{}' is already defined", format_symbol(*name, interner))
            }
            UndefinedType(path) => {
                format!("Undefined type '{}'", format_path(path, interner))
            }
            UndefinedModule(path) => {
                format!("Undefined module '{}'", format_path(path, interner))
            }
            UndefinedProvider(path) => {
                format!("Undefined provider '{}'", format_path(path, interner))
            }
            NotAModule(path) => {
                format!("'{}' is not a module or provider", format_path(path, interner))
            }
            NotAFunction(name) => {
                format!("'{}' is not a function", format_symbol(*name, interner))
            }
            NotAProvider(path) => {
                format!("'{}' is not a provider", format_path(path, interner))
            }
            InvalidBinding => "Invalid binding".to_string(),
            RecordSizeMismatch => "Record size mismatch".to_string(),
            RecordFieldMismatch => "Record field mismatch".to_string(),
            InfiniteType(var) => format!("Infinite type detected: {}", var),
            InvalidListElement(_) => "Invalid list element type".to_string(),
            InvalidRecordField(field, _) => {
                format!("Invalid record field '{}'", format_symbol(*field, interner))
            }
            Provider(kind) => kind.to_string(),
            Runtime(msg) => msg.clone(),
            InternalCompilerError { phase, message } => {
                format!("Internal compiler error in {}: {}", phase, message)
            }
        }
    }

    /// Generate an Ariadne error report
    pub fn report(&self, interner: &Interner) -> Report<'_, Loc> {
        let mut report = Report::build(ReportKind::Error, self.loc.clone())
            .with_message(self.message(interner));

        let mut label = Label::new(self.loc.clone());

        if let Some(ctx) = &self.context {
            label = label.with_message(ctx);
        }

        report = report.with_label(label);

        for suggestion in &self.suggestions {
            report = report.with_note(suggestion.format());
        }

        match &self.kind {
            CompileErrorKind::UndefinedVariable { .. } => {
                if self.suggestions.is_empty() {
                    report = report.with_help(
                        "Variables must be defined before use. Check spelling and scope.",
                    );
                }
            }
            CompileErrorKind::TypeMismatch { .. } => {
                if self.suggestions.is_empty() {
                    report = report
                        .with_help("Type mismatches can be fixed with explicit type annotations.");
                }
            }
            CompileErrorKind::ArityMismatch { .. } => {
                report = report.with_help(
                    "Check the function definition to see the expected number of arguments.",
                );
            }
            CompileErrorKind::InternalCompilerError { phase, .. } => {
                report = report.with_help(format!(
                    "This is a bug in the {} phase of the compiler. Please file a bug report.",
                    phase
                ));
            }
            _ => {}
        }

        report.finish()
    }
}

// Type aliases for compatibility
pub type ParseError = CompileError;
pub type TypeError = CompileError;
pub type RuntimeError = CompileError;
pub type ProviderError = CompileError;

impl From<polars::error::PolarsError> for CompileError {
    fn from(err: polars::error::PolarsError) -> Self {
        CompileError::new(
            CompileErrorKind::Provider(ProviderErrorKind::DataError(err.to_string())),
            crate::ast::Loc::generated(),
        )
    }
}

impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::new(
            CompileErrorKind::Provider(ProviderErrorKind::Custom(format!("IO error: {}", err))),
            crate::ast::Loc::generated(),
        )
    }
}
