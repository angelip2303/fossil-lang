use fossil_lang::error::{FossilError, FossilWarning};
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "utoipa", derive(utoipa::ToSchema))]
pub struct DiagnosticItem {
    pub from: usize,
    pub to: usize,
    pub severity: &'static str,
    pub message: String,
}

pub fn map_errors(errors: &[FossilError]) -> Vec<DiagnosticItem> {
    errors.iter().filter_map(map_error).collect()
}

pub fn map_warnings(warnings: &[FossilWarning]) -> Vec<DiagnosticItem> {
    warnings
        .iter()
        .map(|w| DiagnosticItem {
            from: w.span.offset(),
            to: w.span.offset() + w.span.len(),
            severity: "warning",
            message: w.message.clone(),
        })
        .collect()
}

fn map_error(error: &FossilError) -> Option<DiagnosticItem> {
    let span = extract_span(error)?;
    Some(DiagnosticItem {
        from: span.0,
        to: span.1,
        severity: "error",
        message: error.to_string(),
    })
}

fn extract_span(error: &FossilError) -> Option<(usize, usize)> {
    use FossilError::*;
    let span = match error {
        Syntax { span, .. }
        | Undefined { span, .. }
        | AlreadyDefined { span, .. }
        | NotA { span, .. }
        | TypeMismatch { span, .. }
        | ArityMismatch { span, .. }
        | InfiniteType { span, .. }
        | FieldNotFound { span, .. }
        | RecordSizeMismatch { span, .. }
        | Evaluation { span, .. }
        | MissingArgument { span, .. }
        | InvalidArgumentType { span, .. }
        | FileNotFound { span, .. }
        | NotAFile { span, .. }
        | InvalidExtension { span, .. }
        | ReadError { span, .. }
        | ParseError { span, .. }
        | DataError { span, .. }
        | ProviderKindMismatch { span, .. }
        | Internal { span, .. } => span,
        Io(_) | Polars(_) => return None,
    };
    Some((span.offset(), span.offset() + span.len()))
}
