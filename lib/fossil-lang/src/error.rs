//! Error types using miette for diagnostics
//!
//! Single unified error type with all variants. Uses miette's derive macros
//! for automatic diagnostic generation.

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Loc;

/// Convert Loc to miette's SourceSpan
impl From<Loc> for SourceSpan {
    fn from(loc: Loc) -> Self {
        (loc.span.start, loc.span.end - loc.span.start).into()
    }
}

/// Type variable for type inference
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

/// Unified error type for the Fossil compiler
#[derive(Debug, Error, Diagnostic)]
pub enum FossilError {
    // ===== Parse Errors =====
    #[error("syntax error: {message}")]
    #[diagnostic(code(fossil::parse::syntax))]
    Syntax {
        message: String,
        #[label("here")]
        span: SourceSpan,
    },

    // ===== Resolution Errors =====
    #[error("undefined variable '{name}'")]
    #[diagnostic(
        code(fossil::resolve::undefined_var),
        help("check spelling or define it first")
    )]
    UndefinedVariable {
        name: String,
        #[label("not found in scope")]
        span: SourceSpan,
    },

    #[error("undefined path '{path}'")]
    #[diagnostic(code(fossil::resolve::undefined_path))]
    UndefinedPath {
        path: String,
        #[label("not found")]
        span: SourceSpan,
    },

    #[error("'{name}' is already defined")]
    #[diagnostic(code(fossil::resolve::already_defined))]
    AlreadyDefined {
        name: String,
        #[label("redefined here")]
        span: SourceSpan,
        #[label("first defined here")]
        first_def: SourceSpan,
    },

    #[error("undefined type '{path}'")]
    #[diagnostic(code(fossil::resolve::undefined_type))]
    UndefinedType {
        path: String,
        #[label("type not found")]
        span: SourceSpan,
    },

    #[error("undefined module '{path}'")]
    #[diagnostic(code(fossil::resolve::undefined_module))]
    UndefinedModule {
        path: String,
        #[label("module not found")]
        span: SourceSpan,
    },

    #[error("undefined provider '{path}'")]
    #[diagnostic(code(fossil::resolve::undefined_provider))]
    UndefinedProvider {
        path: String,
        #[label("provider not found")]
        span: SourceSpan,
    },

    #[error("'{path}' is not a module")]
    #[diagnostic(code(fossil::resolve::not_a_module))]
    NotAModule {
        path: String,
        #[label("expected a module")]
        span: SourceSpan,
    },

    #[error("'{name}' is not a function")]
    #[diagnostic(code(fossil::resolve::not_a_function))]
    NotAFunction {
        name: String,
        #[label("cannot be called")]
        span: SourceSpan,
    },

    #[error("'{path}' is not a provider")]
    #[diagnostic(code(fossil::resolve::not_a_provider))]
    NotAProvider {
        path: String,
        #[label("expected a type provider")]
        span: SourceSpan,
    },

    // ===== Type Errors =====
    #[error("type mismatch: {message}")]
    #[diagnostic(code(fossil::types::mismatch))]
    TypeMismatch {
        message: String,
        #[label("{message}")]
        span: SourceSpan,
    },

    #[error("expected {expected} arguments, got {actual}")]
    #[diagnostic(code(fossil::types::arity))]
    ArityMismatch {
        expected: usize,
        actual: usize,
        #[label("expected {expected} arguments")]
        span: SourceSpan,
    },

    #[error("infinite type: {var} occurs in the type being unified")]
    #[diagnostic(code(fossil::types::infinite))]
    InfiniteType {
        var: TypeVar,
        #[label("recursive type")]
        span: SourceSpan,
    },

    #[error("field '{field}' not found in record")]
    #[diagnostic(code(fossil::types::field_not_found))]
    FieldNotFound {
        field: String,
        #[label("field not found")]
        span: SourceSpan,
    },

    #[error("record has {actual} fields but expected {expected}")]
    #[diagnostic(code(fossil::types::record_size))]
    RecordSizeMismatch {
        expected: usize,
        actual: usize,
        #[label("wrong number of fields")]
        span: SourceSpan,
    },

    // ===== Runtime Errors =====
    #[error("{message}")]
    #[diagnostic(code(fossil::runtime::eval))]
    Evaluation {
        message: String,
        #[label("error occurred here")]
        span: SourceSpan,
    },

    #[error("stack overflow: maximum recursion depth ({depth}) exceeded")]
    #[diagnostic(code(fossil::runtime::stack_overflow))]
    StackOverflow {
        depth: usize,
        #[label("recursive call")]
        span: SourceSpan,
    },

    // ===== Provider Errors =====
    #[error("{provider} provider requires '{name}' argument")]
    #[diagnostic(code(fossil::provider::missing_arg))]
    MissingArgument {
        name: &'static str,
        provider: &'static str,
        #[label("missing argument")]
        span: SourceSpan,
    },

    #[error("'{name}' argument must be {expected}")]
    #[diagnostic(code(fossil::provider::invalid_arg))]
    InvalidArgumentType {
        name: &'static str,
        expected: &'static str,
        #[label("invalid type")]
        span: SourceSpan,
    },

    #[error("file not found: {path}")]
    #[diagnostic(code(fossil::io::not_found))]
    FileNotFound {
        path: String,
        #[label("file does not exist")]
        span: SourceSpan,
    },

    #[error("not a file: {path}")]
    #[diagnostic(code(fossil::io::not_a_file))]
    NotAFile {
        path: String,
        #[label("expected a file")]
        span: SourceSpan,
    },

    #[error("invalid extension '{found}', expected: {expected}")]
    #[diagnostic(code(fossil::io::invalid_extension))]
    InvalidExtension {
        found: String,
        expected: String,
        #[label("wrong extension")]
        span: SourceSpan,
    },

    #[error("failed to read {path}: {cause}")]
    #[diagnostic(code(fossil::io::read_error))]
    ReadError {
        path: String,
        cause: String,
        #[label("read failed")]
        span: SourceSpan,
    },

    #[error("failed to parse {format}: {cause}")]
    #[diagnostic(code(fossil::provider::parse_error))]
    ParseError {
        format: &'static str,
        cause: String,
        #[label("parse failed")]
        span: SourceSpan,
    },

    #[error("schema has no shapes defined")]
    #[diagnostic(code(fossil::provider::no_shapes))]
    NoShapesDefined {
        #[label("no shapes")]
        span: SourceSpan,
    },

    #[error("shape '{name}' not found")]
    #[diagnostic(code(fossil::provider::shape_not_found))]
    ShapeNotFound {
        name: String,
        #[label("shape not found")]
        span: SourceSpan,
    },

    #[error("data error: {message}")]
    #[diagnostic(code(fossil::data))]
    DataError {
        message: String,
        #[label("data error")]
        span: SourceSpan,
    },

    #[error("internal compiler error in {phase}: {message}")]
    #[diagnostic(code(fossil::internal), help("this is a bug, please report it"))]
    Internal {
        phase: &'static str,
        message: String,
        #[label("internal error")]
        span: SourceSpan,
    },

    // ===== External Errors (wrapped) =====
    #[error("IO error: {0}")]
    #[diagnostic(code(fossil::io))]
    Io(#[from] std::io::Error),

    #[error("polars error: {0}")]
    #[diagnostic(code(fossil::polars))]
    Polars(#[from] polars::error::PolarsError),
}

// ===== Constructors that resolve Symbol → String =====
impl FossilError {
    pub fn syntax(message: impl Into<String>, loc: Loc) -> Self {
        Self::Syntax {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn undefined_variable(name: impl Into<String>, loc: Loc) -> Self {
        Self::UndefinedVariable {
            name: name.into(),
            span: loc.into(),
        }
    }

    pub fn undefined_path(path: impl Into<String>, loc: Loc) -> Self {
        Self::UndefinedPath {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn already_defined(name: impl Into<String>, first: Loc, second: Loc) -> Self {
        Self::AlreadyDefined {
            name: name.into(),
            first_def: first.into(),
            span: second.into(),
        }
    }

    pub fn undefined_type(path: impl Into<String>, loc: Loc) -> Self {
        Self::UndefinedType {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn undefined_module(path: impl Into<String>, loc: Loc) -> Self {
        Self::UndefinedModule {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn undefined_provider(path: impl Into<String>, loc: Loc) -> Self {
        Self::UndefinedProvider {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn not_a_module(path: impl Into<String>, loc: Loc) -> Self {
        Self::NotAModule {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn not_a_function(name: impl Into<String>, loc: Loc) -> Self {
        Self::NotAFunction {
            name: name.into(),
            span: loc.into(),
        }
    }

    pub fn not_a_provider(path: impl Into<String>, loc: Loc) -> Self {
        Self::NotAProvider {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn type_mismatch(message: impl Into<String>, loc: Loc) -> Self {
        Self::TypeMismatch {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn arity_mismatch(expected: usize, actual: usize, loc: Loc) -> Self {
        Self::ArityMismatch {
            expected,
            actual,
            span: loc.into(),
        }
    }

    pub fn infinite_type(var: TypeVar, loc: Loc) -> Self {
        Self::InfiniteType {
            var,
            span: loc.into(),
        }
    }

    pub fn field_not_found(field: impl Into<String>, loc: Loc) -> Self {
        Self::FieldNotFound {
            field: field.into(),
            span: loc.into(),
        }
    }

    pub fn record_size_mismatch(expected: usize, actual: usize, loc: Loc) -> Self {
        Self::RecordSizeMismatch {
            expected,
            actual,
            span: loc.into(),
        }
    }

    pub fn evaluation(message: impl Into<String>, loc: Loc) -> Self {
        Self::Evaluation {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn stack_overflow(depth: usize, loc: Loc) -> Self {
        Self::StackOverflow {
            depth,
            span: loc.into(),
        }
    }

    pub fn missing_argument(name: &'static str, provider: &'static str, loc: Loc) -> Self {
        Self::MissingArgument {
            name,
            provider,
            span: loc.into(),
        }
    }

    pub fn invalid_argument_type(name: &'static str, expected: &'static str, loc: Loc) -> Self {
        Self::InvalidArgumentType {
            name,
            expected,
            span: loc.into(),
        }
    }

    pub fn file_not_found(path: impl Into<String>, loc: Loc) -> Self {
        Self::FileNotFound {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn not_a_file(path: impl Into<String>, loc: Loc) -> Self {
        Self::NotAFile {
            path: path.into(),
            span: loc.into(),
        }
    }

    pub fn invalid_extension(
        found: impl Into<String>,
        expected: impl Into<String>,
        loc: Loc,
    ) -> Self {
        Self::InvalidExtension {
            found: found.into(),
            expected: expected.into(),
            span: loc.into(),
        }
    }

    pub fn read_error(path: impl Into<String>, cause: impl Into<String>, loc: Loc) -> Self {
        Self::ReadError {
            path: path.into(),
            cause: cause.into(),
            span: loc.into(),
        }
    }

    pub fn parse_error(format: &'static str, cause: impl Into<String>, loc: Loc) -> Self {
        Self::ParseError {
            format,
            cause: cause.into(),
            span: loc.into(),
        }
    }

    pub fn no_shapes_defined(loc: Loc) -> Self {
        Self::NoShapesDefined { span: loc.into() }
    }

    pub fn shape_not_found(name: impl Into<String>, loc: Loc) -> Self {
        Self::ShapeNotFound {
            name: name.into(),
            span: loc.into(),
        }
    }

    pub fn data_error(message: impl Into<String>, loc: Loc) -> Self {
        Self::DataError {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn internal(phase: &'static str, message: impl Into<String>, loc: Loc) -> Self {
        Self::Internal {
            phase,
            message: message.into(),
            span: loc.into(),
        }
    }
}

/// Extension trait to add location to errors
pub trait AtLoc {
    fn at(self, loc: Loc) -> FossilError;
}

impl AtLoc for std::io::Error {
    fn at(self, loc: Loc) -> FossilError {
        FossilError::read_error("<unknown>", self.to_string(), loc)
    }
}

impl AtLoc for polars::error::PolarsError {
    fn at(self, loc: Loc) -> FossilError {
        FossilError::data_error(self.to_string(), loc)
    }
}

/// Collection of errors
#[derive(Debug, Default)]
pub struct FossilErrors(pub Vec<FossilError>);

impl FossilErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, error: impl Into<FossilError>) {
        self.0.push(error.into());
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn into_result<T>(self, ok: T) -> Result<T, Self> {
        if self.is_empty() {
            Ok(ok)
        } else {
            Err(self)
        }
    }
}

impl std::fmt::Display for FossilErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} error(s)", self.0.len())
    }
}

impl std::error::Error for FossilErrors {}

impl<E: Into<FossilError>> From<E> for FossilErrors {
    fn from(err: E) -> Self {
        Self(vec![err.into()])
    }
}

impl IntoIterator for FossilErrors {
    type Item = FossilError;
    type IntoIter = std::vec::IntoIter<FossilError>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

// ===== Warnings =====
#[derive(Debug, Clone)]
pub enum FossilWarning {
    RdfTypeConflict {
        type_name: String,
        attribute_type: String,
        field_name: String,
        span: SourceSpan,
    },
    ShexRdfTypeIgnored {
        shape_name: String,
        span: SourceSpan,
    },
    Generic {
        message: String,
        span: SourceSpan,
    },
}

impl FossilWarning {
    pub fn rdf_type_conflict(
        type_name: impl Into<String>,
        attribute_type: impl Into<String>,
        field_name: impl Into<String>,
        loc: Loc,
    ) -> Self {
        Self::RdfTypeConflict {
            type_name: type_name.into(),
            attribute_type: attribute_type.into(),
            field_name: field_name.into(),
            span: loc.into(),
        }
    }

    pub fn shex_rdf_type_ignored(shape_name: impl Into<String>, loc: Loc) -> Self {
        Self::ShexRdfTypeIgnored {
            shape_name: shape_name.into(),
            span: loc.into(),
        }
    }

    pub fn generic(message: impl Into<String>, loc: Loc) -> Self {
        Self::Generic {
            message: message.into(),
            span: loc.into(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct FossilWarnings(pub Vec<FossilWarning>);

impl FossilWarnings {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, warning: FossilWarning) {
        self.0.push(warning);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn extend(&mut self, other: FossilWarnings) {
        self.0.extend(other.0);
    }
}

impl IntoIterator for FossilWarnings {
    type Item = FossilWarning;
    type IntoIter = std::vec::IntoIter<FossilWarning>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
