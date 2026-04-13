use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Loc;
use crate::db::Db;

/// Proof that a compilation error has been emitted to the diagnostic
/// accumulator. Zero-sized; deliberately impossible to construct without
/// going through [`emit_error`]. Modeled on rustc's `ErrorGuaranteed`
/// (see `compiler/rustc_errors/src/diagnostic.rs`).
///
/// Holding an `ErrorGuaranteed` is a static guarantee that the user will
/// see at least one diagnostic, which lets downstream code propagate
/// "tainted" results (e.g. `TyKind::Error(ErrorGuaranteed)`) without having
/// to re-yell the same problem at every layer.
///
/// The inner `()` field is private to this module — outside `error.rs` the
/// only way to obtain a value is to call [`emit_error`].
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ErrorGuaranteed(());

/// Emit a `FossilError` into the Salsa diagnostic accumulator and return
/// proof that it was reported. This is the single public constructor for
/// [`ErrorGuaranteed`]; every site that wants to taint a downstream value
/// (for example, build a `Ty::mk_error`) must go through here.
pub fn emit_error(db: &dyn Db, err: FossilError) -> ErrorGuaranteed {
    use salsa::Accumulator;
    crate::db::Diagnostic::from_error(&err).accumulate(db);
    ErrorGuaranteed(())
}

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

    /// "Did you mean" undefined source error with Levenshtein-based suggestions.
    pub fn undefined_source(
        name: impl Into<String>,
        available: impl IntoIterator<Item = String>,
        loc: Loc,
    ) -> Self {
        let name_str = name.into();
        let suggestions = closest_matches(&name_str, available, 3);
        let display = if suggestions.is_empty() {
            name_str.clone()
        } else {
            format!("{} (did you mean: {}?)", name_str, suggestions.join(", "))
        };
        Self::Undefined {
            kind: "source",
            name: display,
            span: loc.into(),
        }
    }

    /// "Did you mean" undefined sink error with Levenshtein-based suggestions.
    pub fn undefined_sink(
        namespace: impl Into<String>,
        name: impl Into<String>,
        available: impl IntoIterator<Item = (String, String)>,
        loc: Loc,
    ) -> Self {
        let ns_str = namespace.into();
        let name_str = name.into();
        let qualified = format!("{}.{}", ns_str, name_str);
        let suggestions = closest_matches(
            &qualified,
            available.into_iter().map(|(ns, n)| format!("{}.{}", ns, n)),
            3,
        );
        let display = if suggestions.is_empty() {
            qualified
        } else {
            format!(
                "{}.{} (did you mean: {}?)",
                ns_str,
                name_str,
                suggestions.join(", ")
            )
        };
        Self::Undefined {
            kind: "sink",
            name: display,
            span: loc.into(),
        }
    }

    /// Extract (offset, len) from the variant's miette span for Diagnostic accumulator.
    pub fn span_info(&self) -> Option<(usize, usize)> {
        let span = match self {
            Self::Syntax { span, .. }
            | Self::Undefined { span, .. }
            | Self::AlreadyDefined { span, .. }
            | Self::TypeMismatch { span, .. }
            | Self::ArityMismatch { span, .. }
            | Self::InfiniteType { span, .. }
            | Self::FieldNotFound { span, .. }
            | Self::RecordSizeMismatch { span, .. }
            | Self::Evaluation { span, .. }
            | Self::InvalidArgumentType { span, .. }
            | Self::FileNotFound { span, .. }
            | Self::NotAFile { span, .. }
            | Self::InvalidExtension { span, .. }
            | Self::ReadError { span, .. }
            | Self::ParseError { span, .. }
            | Self::DataError { span, .. }
            | Self::ProviderKindMismatch { span, .. }
            | Self::Internal { span, .. } => Some(span),
            Self::Io(_) => None,
        }?;
        Some((span.offset(), span.len()))
    }
}

/// Levenshtein-based "did you mean" suggestions.
/// Returns top-N candidates within distance threshold.
fn closest_matches(
    input: &str,
    candidates: impl IntoIterator<Item = String>,
    max: usize,
) -> Vec<String> {
    let mut scored: Vec<(usize, String)> = candidates
        .into_iter()
        .map(|c| (levenshtein(input, &c), c))
        .filter(|(d, _)| *d <= 3) // threshold
        .collect();
    scored.sort_by_key(|(d, _)| *d);
    scored.into_iter().take(max).map(|(_, c)| c).collect()
}

/// Simple Levenshtein distance — small fn, no extra dep needed.
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let m = a.len();
    let n = b.len();
    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }
    let mut prev: Vec<usize> = (0..=n).collect();
    let mut curr = vec![0; n + 1];
    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            curr[j] = (prev[j] + 1)
                .min(curr[j - 1] + 1)
                .min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[n]
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Loc;

    fn dummy_loc() -> Loc {
        Loc::new(0, 0..10)
    }

    #[test]
    fn errors_vec_push_and_len() {
        let mut errors: Vec<FossilError> = Vec::new();
        errors.push(FossilError::syntax("bad token", dummy_loc()));
        errors.push(FossilError::syntax("another error", dummy_loc()));
        assert_eq!(errors.len(), 2);
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
