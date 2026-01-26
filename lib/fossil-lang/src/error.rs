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
    ast::{ast::Path, Loc},
    context::{Interner, Symbol},
    ir::TypeId,
};

/// Format a Symbol using the interner to get the actual name.
/// Uses try_resolve to safely handle symbols that may not exist in this interner
/// (can happen when errors are created in a different compilation context).
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
///
/// This is the main error type used throughout the compiler. It includes:
/// - The specific error kind
/// - Source location information for reporting
/// - Optional contextual message to help users
/// - Actionable suggestions for fixing the error
#[derive(Debug)]
pub struct CompileError {
    /// The specific kind of error that occurred
    pub kind: CompileErrorKind,
    /// Location in source code where the error occurred
    pub loc: Loc,
    /// Optional context message to help users understand the error
    pub context: Option<String>,
    /// Suggestions for fixing the error
    pub suggestions: Vec<ErrorSuggestion>,
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
        if self.is_empty() { Ok(ok) } else { Err(self) }
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

/// Suggestion for fixing a compilation error
///
/// Provides actionable suggestions to help users fix errors, similar to
/// Rust and Elm compiler error messages.
#[derive(Debug, Clone)]
pub enum ErrorSuggestion {
    /// "Did you mean this similar name?"
    ///
    /// Suggests a name that's similar to the undefined/misspelled identifier.
    /// The confidence score (0.0-1.0) indicates how likely this is the intended name.
    DidYouMean {
        wrong: String,
        suggestion: String,
        confidence: f32,
    },

    /// "Add a type annotation"
    ///
    /// Suggests adding an explicit type annotation to help the type checker.
    AddTypeAnnotation {
        name: String,
        suggested_type: String,
    },

    /// "Fix a common mistake"
    ///
    /// Suggests correcting a known common mistake or typo.
    FixTypo {
        wrong: String,
        correct: String,
        explanation: String,
    },

    /// General help text
    ///
    /// Provides general guidance for fixing the error.
    Help(String),
}

impl ErrorSuggestion {
    /// Format the suggestion as a human-readable string
    pub fn format(&self) -> String {
        match self {
            Self::DidYouMean {
                wrong,
                suggestion,
                confidence,
            } => {
                if *confidence > 0.8 {
                    format!("Did you mean '{}'?", suggestion)
                } else {
                    format!("Did you mean '{}' (similar to '{}')?", suggestion, wrong)
                }
            }
            Self::AddTypeAnnotation {
                name,
                suggested_type,
            } => {
                format!(
                    "Add type annotation: let {}: {} = ...",
                    name, suggested_type
                )
            }
            Self::FixTypo {
                wrong,
                correct,
                explanation,
            } => {
                format!("Replace '{}' with '{}': {}", wrong, correct, explanation)
            }
            Self::Help(msg) => msg.clone(),
        }
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
        actual: TypeId,
    },

    /// Reference to an undefined variable
    UndefinedVariable {
        /// Name of the undefined variable
        name: Symbol,
    },

    /// Reference to an undefined path (qualified name)
    UndefinedPath {
        /// The path that could not be resolved
        path: Path,
    },

    /// Function called with wrong number of arguments
    ArityMismatch {
        /// Expected number of arguments
        expected: usize,
        /// Actual number of arguments provided
        actual: usize,
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

    /// Invalid use of placeholder `_` - only valid in field access context (_.field)
    InvalidPlaceholder,

    /// Internal compiler error - indicates a bug in the compiler itself
    ///
    /// These errors should never occur in correct compiler code.
    /// If users encounter this error, they should file a bug report.
    ///
    /// # Fields
    /// * `phase` - The compiler phase where the error occurred (e.g., "lowering", "type checking")
    /// * `message` - Description of what went wrong
    InternalCompilerError {
        phase: &'static str,
        message: String,
    },

    // === Attribute validation errors ===
    /// Unknown attribute name
    ///
    /// The attribute is not registered in the attribute registry.
    UnknownAttribute(Symbol),

    /// Missing required attribute argument
    ///
    /// A required argument was not provided for the attribute.
    MissingAttributeArg {
        /// The attribute name
        attr: Symbol,
        /// The missing argument name
        arg: &'static str,
    },

    /// Attribute argument type mismatch
    ///
    /// The argument value has the wrong type.
    AttributeArgTypeMismatch {
        /// The attribute name
        attr: Symbol,
        /// The argument name
        arg: Symbol,
        /// Expected type
        expected: &'static str,
        /// Actual type found
        actual: &'static str,
    },

    /// Unknown attribute argument
    ///
    /// The argument is not defined in the attribute schema.
    UnknownAttributeArg {
        /// The attribute name
        attr: Symbol,
        /// The unknown argument name
        arg: Symbol,
    },

    /// Attribute applied to invalid target
    ///
    /// The attribute cannot be applied to this location.
    InvalidAttributeTarget {
        /// The attribute name
        attr: Symbol,
        /// Where the attribute was applied
        actual_target: &'static str,
        /// Where the attribute can be applied
        expected_target: &'static str,
    },
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
            suggestions: Vec::new(),
        }
    }

    /// Add a suggestion to this error
    ///
    /// Suggestions help users fix errors by providing actionable advice.
    /// Multiple suggestions can be added to a single error.
    ///
    /// # Example
    /// ```rust,ignore
    /// error.with_suggestion(ErrorSuggestion::DidYouMean {
    ///     wrong: "lenght".to_string(),
    ///     suggestion: "length".to_string(),
    ///     confidence: 0.9,
    /// })
    /// ```
    pub fn with_suggestion(mut self, suggestion: ErrorSuggestion) -> Self {
        self.suggestions.push(suggestion);
        self
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

    /// Create an internal compiler error
    ///
    /// This is a convenience method for creating InternalCompilerError instances.
    /// These errors indicate bugs in the compiler itself, not user code errors.
    ///
    /// # Arguments
    /// * `phase` - The compiler phase where the error occurred (e.g., "lowering", "type checking")
    /// * `message` - Description of what went wrong
    /// * `loc` - Source location (often Loc::generated() for internal errors)
    ///
    /// # Example
    /// ```rust,ignore
    /// CompileError::internal(
    ///     "lowering",
    ///     "Let binding missing DefId after name resolution",
    ///     loc
    /// )
    /// ```
    pub fn internal(phase: &'static str, message: impl Into<String>, loc: Loc) -> Self {
        Self::new(
            CompileErrorKind::InternalCompilerError {
                phase,
                message: message.into(),
            },
            loc,
        )
    }

    /// Get a human-readable error message (uses debug formatting for symbols)
    ///
    /// Returns a user-friendly description of the error based on its kind.
    /// Note: This method uses debug formatting for symbols. For proper symbol resolution,
    /// use `message_with_interner` instead.
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
            InvalidPlaceholder => "Invalid use of placeholder `_`. Placeholder is only valid in field access context (_.field)".to_string(),
            InternalCompilerError { phase, message } => {
                format!("Internal compiler error in {}: {}", phase, message)
            }
            UnknownAttribute(_) => "Unknown attribute".to_string(),
            MissingAttributeArg { arg, .. } => {
                format!("Missing required argument '{}'", arg)
            }
            AttributeArgTypeMismatch { expected, actual, .. } => {
                format!("Type mismatch: expected {}, got {}", expected, actual)
            }
            UnknownAttributeArg { .. } => "Unknown attribute argument".to_string(),
            InvalidAttributeTarget { actual_target, expected_target, .. } => {
                format!(
                    "Invalid target: {} (expected {})",
                    actual_target, expected_target
                )
            }
        }
    }

    /// Get a human-readable error message with proper symbol resolution
    ///
    /// Returns a user-friendly description of the error with symbols resolved
    /// to their actual names using the provided interner.
    pub fn message_with_interner(&self, interner: &Interner) -> String {
        use CompileErrorKind::*;

        // For errors with context, prefer the context as it contains pre-formatted details
        match &self.kind {
            Parse(msg) => format!("Parse error: {}", format_symbol(*msg, interner)),
            TypeMismatch { .. } => {
                // Context contains the formatted type names from the type checker
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
                format!("Arity mismatch: expected {} arguments, got {}", expected, actual)
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
            ProviderError(_) => {
                // Context contains the actual provider error message
                if let Some(ctx) = &self.context {
                    ctx.clone()
                } else {
                    "Provider error".to_string()
                }
            }
            Runtime(msg) => msg.clone(),
            InvalidPlaceholder => "Invalid use of placeholder `_`. Placeholder is only valid in field access context (_.field)".to_string(),
            InternalCompilerError { phase, message } => {
                format!("Internal compiler error in {}: {}", phase, message)
            }
            UnknownAttribute(name) => {
                format!("Unknown attribute '{}'", format_symbol(*name, interner))
            }
            MissingAttributeArg { attr, arg } => {
                format!(
                    "Missing required argument '{}' for attribute '#[{}]'",
                    arg,
                    format_symbol(*attr, interner)
                )
            }
            AttributeArgTypeMismatch { attr, arg, expected, actual } => {
                format!(
                    "Attribute '#[{}]' argument '{}': expected {}, got {}",
                    format_symbol(*attr, interner),
                    format_symbol(*arg, interner),
                    expected,
                    actual
                )
            }
            UnknownAttributeArg { attr, arg } => {
                format!(
                    "Unknown argument '{}' for attribute '#[{}]'",
                    format_symbol(*arg, interner),
                    format_symbol(*attr, interner)
                )
            }
            InvalidAttributeTarget { attr, actual_target, expected_target } => {
                format!(
                    "Attribute '#[{}]' cannot be applied to {}, only to {}",
                    format_symbol(*attr, interner),
                    actual_target,
                    expected_target
                )
            }
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
        let mut report =
            Report::build(ReportKind::Error, self.loc.clone()).with_message(self.message());

        // Add main label
        let mut label = Label::new(self.loc.clone());

        // Add context if available
        if let Some(ctx) = &self.context {
            label = label.with_message(ctx);
        }

        report = report.with_label(label);

        // Add suggestions as notes
        for suggestion in &self.suggestions {
            report = report.with_note(suggestion.format());
        }

        // Add help text based on error kind
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
