use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Loc;

impl From<Loc> for SourceSpan {
    fn from(loc: Loc) -> Self {
        (loc.span.start, loc.span.end - loc.span.start).into()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum FossilError {
    #[error("syntax error: {message}")]
    #[diagnostic(code(fossil::parse::syntax))]
    Syntax {
        message: String,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("undefined {kind} '{name}'")]
    #[diagnostic(code(fossil::resolve::undefined))]
    Undefined {
        kind: &'static str,
        name: String,
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

    #[error("'{name}' is not a {kind}")]
    #[diagnostic(code(fossil::resolve::not_a))]
    NotA {
        kind: &'static str,
        name: String,
        #[label("expected a {kind}")]
        span: SourceSpan,
    },

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

    #[error("{message}")]
    #[diagnostic(code(fossil::runtime::eval))]
    Evaluation {
        message: String,
        #[label("error occurred here")]
        span: SourceSpan,
    },

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

    #[error("data error: {message}")]
    #[diagnostic(code(fossil::data))]
    DataError {
        message: String,
        #[label("data error")]
        span: SourceSpan,
    },

    #[error("provider '{provider}' {message}")]
    #[diagnostic(code(fossil::provider::kind_mismatch))]
    ProviderKindMismatch {
        provider: String,
        message: &'static str,
        #[label("wrong syntax for this provider")]
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

    #[error("IO error: {0}")]
    #[diagnostic(code(fossil::io))]
    Io(#[from] std::io::Error),

    #[error("polars error: {0}")]
    #[diagnostic(code(fossil::polars))]
    Polars(#[from] polars::error::PolarsError),
}

impl FossilError {
    pub fn syntax(message: impl Into<String>, loc: Loc) -> Self {
        Self::Syntax {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn undefined(kind: &'static str, name: impl Into<String>, loc: Loc) -> Self {
        Self::Undefined {
            kind,
            name: name.into(),
            span: loc.into(),
        }
    }

    pub fn undefined_variable(name: impl Into<String>, loc: Loc) -> Self {
        Self::undefined("variable", name, loc)
    }

    pub fn undefined_path(path: impl Into<String>, loc: Loc) -> Self {
        Self::undefined("path", path, loc)
    }

    pub fn already_defined(name: impl Into<String>, first: Loc, second: Loc) -> Self {
        Self::AlreadyDefined {
            name: name.into(),
            first_def: first.into(),
            span: second.into(),
        }
    }

    pub fn undefined_type(path: impl Into<String>, loc: Loc) -> Self {
        Self::undefined("type", path, loc)
    }

    pub fn not_a(kind: &'static str, name: impl Into<String>, loc: Loc) -> Self {
        Self::NotA {
            kind,
            name: name.into(),
            span: loc.into(),
        }
    }

    pub fn not_a_provider(path: impl Into<String>, loc: Loc) -> Self {
        Self::not_a("provider", path, loc)
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

    pub fn data_error(message: impl Into<String>, loc: Loc) -> Self {
        Self::DataError {
            message: message.into(),
            span: loc.into(),
        }
    }

    pub fn provider_kind_mismatch(
        provider: impl Into<String>,
        message: &'static str,
        loc: Loc,
    ) -> Self {
        Self::ProviderKindMismatch {
            provider: provider.into(),
            message,
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
        if self.is_empty() { Ok(ok) } else { Err(self) }
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

#[derive(Debug, Clone)]
pub struct FossilWarning {
    pub message: String,
    pub span: SourceSpan,
}

impl FossilWarning {
    pub fn generic(message: impl Into<String>, loc: Loc) -> Self {
        Self {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Loc;

    fn dummy_loc() -> Loc {
        Loc::new(0, 0..10)
    }

    #[test]
    fn errors_push_and_len() {
        let mut errors = FossilErrors::new();
        errors.push(FossilError::syntax("bad token", dummy_loc()));
        errors.push(FossilError::syntax("another error", dummy_loc()));
        assert_eq!(errors.len(), 2);
    }

    #[test]
    fn errors_is_empty() {
        let mut errors = FossilErrors::new();
        assert!(errors.is_empty());
        errors.push(FossilError::syntax("oops", dummy_loc()));
        assert!(!errors.is_empty());
    }

    #[test]
    fn errors_into_result_ok() {
        let errors = FossilErrors::new();
        let result = errors.into_result(42);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn errors_into_result_err() {
        let mut errors = FossilErrors::new();
        errors.push(FossilError::syntax("fail", dummy_loc()));
        let result = errors.into_result(42);
        assert!(result.is_err());
    }

    #[test]
    fn error_constructors() {
        let loc = dummy_loc();

        let syntax_err = FossilError::syntax("unexpected token", loc);
        assert!(matches!(syntax_err, FossilError::Syntax { ref message, .. } if message == "unexpected token"));

        let undef_err = FossilError::undefined_variable("x", loc);
        assert!(matches!(undef_err, FossilError::Undefined { kind: "variable", ref name, .. } if name == "x"));

        let mismatch_err = FossilError::type_mismatch("expected Int, got String", loc);
        assert!(matches!(mismatch_err, FossilError::TypeMismatch { ref message, .. } if message == "expected Int, got String"));
    }
}
