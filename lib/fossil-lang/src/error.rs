//! Error types and reporting
//!
//! This module defines atomic error types for each compiler phase and provides
//! functionality for generating user-friendly error reports using the Ariadne library.
//!
//! The error system is organized into:
//! - `ParseError`: Syntax errors from parsing
//! - `ResolutionError`: Name resolution errors (undefined variables, types, etc.)
//! - `TypeError`: Type checking errors (mismatches, arity, infinite types)
//! - `RuntimeError`: Evaluation errors at runtime
//! - `ProviderError`: Type provider errors (file access, schema validation)
//! - `CompileError`: Unified wrapper for the compilation pipeline

use ariadne::{Color, Config, Label, Report, ReportKind};
use thiserror::Error;

use crate::{
    ast::{Loc, ast::Path},
    context::{Interner, Symbol},
    ir::TypeId,
};

fn format_symbol(sym: Symbol, interner: &Interner) -> String {
    interner.try_resolve(sym).unwrap_or("<unknown>").to_string()
}

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

pub fn report_config() -> Config {
    Config::default().with_compact(false).with_cross_gap(true)
}

pub trait Reportable {
    fn report(&self, interner: &Interner) -> Report<'static, Loc>;
    fn loc(&self) -> &Loc;
    fn color(&self) -> Color;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub loc: Loc,
}

impl StackFrame {
    pub fn new(function_name: impl Into<String>, loc: Loc) -> Self {
        Self {
            function_name: function_name.into(),
            loc,
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Parse error")]
    Syntax { message: Symbol, loc: Loc },
}

impl ParseError {
    pub fn syntax(message: Symbol, loc: Loc) -> Self {
        Self::Syntax { message, loc }
    }
}

impl Reportable for ParseError {
    fn report(&self, interner: &Interner) -> Report<'static, Loc> {
        match self {
            ParseError::Syntax { message, loc } => {
                let msg = format_symbol(*message, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Parse error: {}", msg))
                    .with_label(Label::new(*loc).with_message(&msg).with_color(self.color()))
                    .finish()
            }
        }
    }

    fn loc(&self) -> &Loc {
        match self {
            ParseError::Syntax { loc, .. } => loc,
        }
    }

    fn color(&self) -> Color {
        Color::Yellow
    }
}

#[derive(Clone, Debug, Error)]
pub enum ResolutionError {
    #[error("Undefined variable")]
    UndefinedVariable {
        name: Symbol,
        loc: Loc,
        similar: Vec<Symbol>,
    },

    #[error("Undefined path")]
    UndefinedPath { path: Path, loc: Loc },

    #[error("Already defined")]
    AlreadyDefined {
        name: Symbol,
        first_def: Loc,
        second_def: Loc,
    },

    #[error("Undefined type")]
    UndefinedType { path: Path, loc: Loc },

    #[error("Undefined module")]
    UndefinedModule { path: Path, loc: Loc },

    #[error("Undefined provider")]
    UndefinedProvider { path: Path, loc: Loc },

    #[error("Not a module")]
    NotAModule { path: Path, loc: Loc },

    #[error("Not a function")]
    NotAFunction { name: Symbol, loc: Loc },

    #[error("Not a provider")]
    NotAProvider { path: Path, loc: Loc },
}

impl ResolutionError {
    pub fn undefined_variable(name: Symbol, loc: Loc) -> Self {
        Self::UndefinedVariable {
            name,
            loc,
            similar: Vec::new(),
        }
    }

    pub fn undefined_variable_with_similar(name: Symbol, loc: Loc, similar: Vec<Symbol>) -> Self {
        Self::UndefinedVariable { name, loc, similar }
    }

    pub fn undefined_path(path: Path, loc: Loc) -> Self {
        Self::UndefinedPath { path, loc }
    }

    pub fn already_defined(name: Symbol, first_def: Loc, second_def: Loc) -> Self {
        Self::AlreadyDefined {
            name,
            first_def,
            second_def,
        }
    }

    pub fn undefined_type(path: Path, loc: Loc) -> Self {
        Self::UndefinedType { path, loc }
    }

    pub fn undefined_module(path: Path, loc: Loc) -> Self {
        Self::UndefinedModule { path, loc }
    }
}

impl Reportable for ResolutionError {
    fn report(&self, interner: &Interner) -> Report<'static, Loc> {
        match self {
            ResolutionError::UndefinedVariable { name, loc, similar } => {
                let name_str = format_symbol(*name, interner);
                let mut report = Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Cannot find '{}' in this scope", name_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("not found in this scope")
                            .with_color(self.color()),
                    );

                if let Some(suggestion) = similar.first() {
                    let suggestion_str = format_symbol(*suggestion, interner);
                    report = report.with_help(format!("Did you mean '{}'?", suggestion_str));
                } else {
                    report = report.with_help(
                        "Variables must be defined before use. Check spelling and scope.",
                    );
                }

                report.finish()
            }

            ResolutionError::UndefinedPath { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Undefined path '{}'", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("path not found")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::AlreadyDefined {
                name,
                first_def,
                second_def,
            } => {
                let name_str = format_symbol(*name, interner);
                Report::build(ReportKind::Error, *second_def)
                    .with_config(report_config())
                    .with_message(format!("'{}' is already defined", name_str))
                    .with_label(
                        Label::new(*first_def)
                            .with_message("first defined here")
                            .with_color(Color::Blue),
                    )
                    .with_label(
                        Label::new(*second_def)
                            .with_message("redefined here")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::UndefinedType { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Undefined type '{}'", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("type not found")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::UndefinedModule { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Undefined module '{}'", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("module not found")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::UndefinedProvider { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Undefined provider '{}'", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("provider not found")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::NotAModule { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("'{}' is not a module or provider", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("expected a module")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::NotAFunction { name, loc } => {
                let name_str = format_symbol(*name, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("'{}' is not a function", name_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("expected a function")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            ResolutionError::NotAProvider { path, loc } => {
                let path_str = format_path(path, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("'{}' is not a provider", path_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("expected a type provider")
                            .with_color(self.color()),
                    )
                    .finish()
            }
        }
    }

    fn loc(&self) -> &Loc {
        match self {
            ResolutionError::UndefinedVariable { loc, .. } => loc,
            ResolutionError::UndefinedPath { loc, .. } => loc,
            ResolutionError::AlreadyDefined { second_def, .. } => second_def,
            ResolutionError::UndefinedType { loc, .. } => loc,
            ResolutionError::UndefinedModule { loc, .. } => loc,
            ResolutionError::UndefinedProvider { loc, .. } => loc,
            ResolutionError::NotAModule { loc, .. } => loc,
            ResolutionError::NotAFunction { loc, .. } => loc,
            ResolutionError::NotAProvider { loc, .. } => loc,
        }
    }

    fn color(&self) -> Color {
        Color::Magenta
    }
}

#[derive(Clone, Debug, Error)]
pub enum TypeError {
    #[error("Type mismatch")]
    Mismatch {
        expected: TypeId,
        actual: TypeId,
        loc: Loc,
        context: Option<String>,
    },

    #[error("Arity mismatch")]
    ArityMismatch {
        expected: usize,
        actual: usize,
        loc: Loc,
    },

    #[error("Infinite type")]
    InfiniteType { var: TypeVar, loc: Loc },

    #[error("Not a function")]
    NotAFunction { name: Symbol, loc: Loc },

    #[error("Invalid record field")]
    InvalidRecordField {
        field: Symbol,
        expected_type: TypeId,
        loc: Loc,
    },

    #[error("Record size mismatch")]
    RecordSizeMismatch {
        expected: usize,
        actual: usize,
        loc: Loc,
    },

    #[error("Record field mismatch")]
    RecordFieldMismatch { field: Symbol, loc: Loc },

    #[error("Invalid list element")]
    InvalidListElement { expected: TypeId, loc: Loc },

    #[error("Invalid binding")]
    InvalidBinding { loc: Loc },
}

impl TypeError {
    pub fn mismatch(expected: TypeId, actual: TypeId, loc: Loc) -> Self {
        Self::Mismatch {
            expected,
            actual,
            loc,
            context: None,
        }
    }

    pub fn mismatch_with_context(
        expected: TypeId,
        actual: TypeId,
        loc: Loc,
        context: impl Into<String>,
    ) -> Self {
        Self::Mismatch {
            expected,
            actual,
            loc,
            context: Some(context.into()),
        }
    }

    pub fn arity_mismatch(expected: usize, actual: usize, loc: Loc) -> Self {
        Self::ArityMismatch {
            expected,
            actual,
            loc,
        }
    }

    pub fn infinite_type(var: TypeVar, loc: Loc) -> Self {
        Self::InfiniteType { var, loc }
    }

    pub fn record_size_mismatch(expected: usize, actual: usize, loc: Loc) -> Self {
        Self::RecordSizeMismatch {
            expected,
            actual,
            loc,
        }
    }

    pub fn record_field_mismatch(field: Symbol, loc: Loc) -> Self {
        Self::RecordFieldMismatch { field, loc }
    }
}

impl Reportable for TypeError {
    fn report(&self, interner: &Interner) -> Report<'static, Loc> {
        match self {
            TypeError::Mismatch { loc, context, .. } => {
                let msg = context
                    .clone()
                    .unwrap_or_else(|| "Type mismatch".to_string());

                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(&msg)
                    .with_label(Label::new(*loc).with_message(&msg).with_color(self.color()))
                    .with_help("Type mismatches can be fixed with explicit type annotations.")
                    .finish()
            }

            TypeError::ArityMismatch {
                expected,
                actual,
                loc,
            } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message(format!("Expected {} arguments, got {}", expected, actual))
                .with_label(
                    Label::new(*loc)
                        .with_message(format!("expected {} arguments", expected))
                        .with_color(self.color()),
                )
                .with_help("Check the function definition to see the expected number of arguments.")
                .finish(),

            TypeError::InfiniteType { var, loc } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message(format!("Infinite type detected: {}", var))
                .with_label(
                    Label::new(*loc)
                        .with_message(format!(
                            "type variable {} occurs in the type being unified",
                            var
                        ))
                        .with_color(self.color()),
                )
                .finish(),

            TypeError::NotAFunction { name, loc } => {
                let name_str = format_symbol(*name, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("'{}' is not a function", name_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("cannot be called")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            TypeError::InvalidRecordField { field, loc, .. } => {
                let field_str = format_symbol(*field, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Invalid record field '{}'", field_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("field type does not match")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            TypeError::RecordSizeMismatch {
                expected,
                actual,
                loc,
            } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message(format!(
                    "Record has {} fields but expected {}",
                    actual, expected
                ))
                .with_label(
                    Label::new(*loc)
                        .with_message("wrong number of fields")
                        .with_color(self.color()),
                )
                .finish(),

            TypeError::RecordFieldMismatch { field, loc } => {
                let field_str = format_symbol(*field, interner);
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!("Field '{}' not found in record type", field_str))
                    .with_label(
                        Label::new(*loc)
                            .with_message("field not found")
                            .with_color(self.color()),
                    )
                    .finish()
            }

            TypeError::InvalidListElement { loc, .. } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message("Invalid list element type")
                .with_label(
                    Label::new(*loc)
                        .with_message("element type does not match")
                        .with_color(self.color()),
                )
                .finish(),

            TypeError::InvalidBinding { loc } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message("Invalid binding")
                .with_label(
                    Label::new(*loc)
                        .with_message("invalid binding pattern")
                        .with_color(self.color()),
                )
                .finish(),
        }
    }

    fn loc(&self) -> &Loc {
        match self {
            TypeError::Mismatch { loc, .. } => loc,
            TypeError::ArityMismatch { loc, .. } => loc,
            TypeError::InfiniteType { loc, .. } => loc,
            TypeError::NotAFunction { loc, .. } => loc,
            TypeError::InvalidRecordField { loc, .. } => loc,
            TypeError::RecordSizeMismatch { loc, .. } => loc,
            TypeError::RecordFieldMismatch { loc, .. } => loc,
            TypeError::InvalidListElement { loc, .. } => loc,
            TypeError::InvalidBinding { loc, .. } => loc,
        }
    }

    fn color(&self) -> Color {
        Color::Red
    }
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{message}")]
    Evaluation {
        message: String,
        loc: Loc,
        call_stack: Vec<StackFrame>,
    },

    #[error("Stack overflow")]
    StackOverflow {
        depth: usize,
        loc: Loc,
        call_stack: Vec<StackFrame>,
    },
}

impl RuntimeError {
    pub fn evaluation(message: impl Into<String>, loc: Loc) -> Self {
        Self::Evaluation {
            message: message.into(),
            loc,
            call_stack: Vec::new(),
        }
    }

    pub fn evaluation_with_stack(
        message: impl Into<String>,
        loc: Loc,
        call_stack: Vec<StackFrame>,
    ) -> Self {
        Self::Evaluation {
            message: message.into(),
            loc,
            call_stack,
        }
    }

    pub fn stack_overflow(depth: usize, loc: Loc, call_stack: Vec<StackFrame>) -> Self {
        Self::StackOverflow {
            depth,
            loc,
            call_stack,
        }
    }
}

impl Reportable for RuntimeError {
    fn report(&self, _interner: &Interner) -> Report<'static, Loc> {
        match self {
            RuntimeError::Evaluation {
                message,
                loc,
                call_stack,
            } => {
                let mut report = Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(message)
                    .with_label(
                        Label::new(*loc)
                            .with_message(message)
                            .with_color(self.color()),
                    );

                // Add call stack as notes
                if !call_stack.is_empty() {
                    let stack_str = call_stack
                        .iter()
                        .enumerate()
                        .map(|(i, frame)| format!("  #{}: {}", i, frame.function_name))
                        .collect::<Vec<_>>()
                        .join("\n");
                    report = report.with_note(format!("Call stack:\n{}", stack_str));
                }

                report.finish()
            }

            RuntimeError::StackOverflow { depth, loc, .. } => {
                Report::build(ReportKind::Error, *loc)
                    .with_config(report_config())
                    .with_message(format!(
                        "Stack overflow: maximum recursion depth ({}) exceeded",
                        depth
                    ))
                    .with_label(
                        Label::new(*loc)
                            .with_message("recursive call exceeded limit")
                            .with_color(self.color()),
                    )
                    .finish()
            }
        }
    }

    fn loc(&self) -> &Loc {
        match self {
            RuntimeError::Evaluation { loc, .. } => loc,
            RuntimeError::StackOverflow { loc, .. } => loc,
        }
    }

    fn color(&self) -> Color {
        Color::Red
    }
}

#[derive(Clone, Debug, Error)]
pub enum ProviderErrorKind {
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

    #[error("File not found: {path}")]
    FileNotFound { path: String },

    #[error("Not a file: {path}")]
    NotAFile { path: String },

    #[error("Invalid file extension '{found}', expected: {expected}")]
    InvalidExtension { found: String, expected: String },

    #[error("Failed to read {path}: {cause}")]
    ReadError { path: String, cause: String },

    #[error("Failed to parse {format}: {cause}")]
    ParseError { format: &'static str, cause: String },

    #[error("Schema has no shapes defined")]
    NoShapesDefined,

    #[error("Shape '{name}' not found in schema")]
    ShapeNotFound { name: String },

    #[error("Data error: {0}")]
    DataError(String),

    #[error("{0}")]
    Custom(String),
}

/// Provider error with location information
#[derive(Clone, Debug, Error)]
#[error("{kind}")]
pub struct ProviderError {
    pub kind: ProviderErrorKind,
    pub loc: Loc,
}

impl ProviderError {
    pub fn new(kind: ProviderErrorKind, loc: Loc) -> Self {
        Self { kind, loc }
    }
}

impl Reportable for ProviderError {
    fn report(&self, _interner: &Interner) -> Report<'static, Loc> {
        Report::build(ReportKind::Error, self.loc)
            .with_config(report_config())
            .with_message(self.kind.to_string())
            .with_label(
                Label::new(self.loc)
                    .with_message(self.kind.to_string())
                    .with_color(self.color()),
            )
            .finish()
    }

    fn loc(&self) -> &Loc {
        &self.loc
    }

    fn color(&self) -> Color {
        Color::Cyan
    }
}

#[derive(Clone, Debug)]
pub enum CompileWarning {
    RdfTypeConflict {
        type_name: String,
        attribute_type: String,
        field_name: String,
        loc: Loc,
    },

    ShexRdfTypeIgnored {
        shape_name: String,
        loc: Loc,
    },

    Generic {
        message: String,
        loc: Loc,
    },
}

impl CompileWarning {
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
            loc,
        }
    }

    pub fn shex_rdf_type_ignored(shape_name: impl Into<String>, loc: Loc) -> Self {
        Self::ShexRdfTypeIgnored {
            shape_name: shape_name.into(),
            loc,
        }
    }

    pub fn generic(message: impl Into<String>, loc: Loc) -> Self {
        Self::Generic {
            message: message.into(),
            loc,
        }
    }

    pub fn loc(&self) -> &Loc {
        match self {
            Self::RdfTypeConflict { loc, .. } => loc,
            Self::ShexRdfTypeIgnored { loc, .. } => loc,
            Self::Generic { loc, .. } => loc,
        }
    }

    pub fn report(&self) -> Report<'static, Loc> {
        match self {
            Self::RdfTypeConflict {
                type_name,
                attribute_type,
                field_name,
                loc,
            } => Report::build(ReportKind::Warning, *loc)
                .with_config(report_config())
                .with_message(format!(
                    "Conflicting rdf:type definitions for type '{}'",
                    type_name
                ))
                .with_label(
                    Label::new(*loc)
                        .with_message(format!(
                            "field '{}' has rdf:type predicate, but #[rdf(type = \"{}\")] is also defined",
                            field_name, attribute_type
                        ))
                        .with_color(Color::Yellow),
                )
                .with_help(format!(
                    "The #[rdf(type = \"{}\")] attribute will be used. Remove the rdf:type field or the attribute to avoid this warning.",
                    attribute_type
                ))
                .finish(),

            Self::ShexRdfTypeIgnored { shape_name, loc } => {
                Report::build(ReportKind::Warning, *loc)
                    .with_config(report_config())
                    .with_message(format!(
                        "ShEx shape '{}' contains rdf:type constraint",
                        shape_name
                    ))
                    .with_label(
                        Label::new(*loc)
                            .with_message("rdf:type constraint will be ignored")
                            .with_color(Color::Yellow),
                    )
                    .with_help(
                        "Use #[rdf(type = \"...\")] attribute on the Fossil type instead.",
                    )
                    .finish()
            }

            Self::Generic { message, loc } => Report::build(ReportKind::Warning, *loc)
                .with_config(report_config())
                .with_message(message)
                .with_label(
                    Label::new(*loc)
                        .with_message(message)
                        .with_color(Color::Yellow),
                )
                .finish(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct CompileWarnings(pub Vec<CompileWarning>);

impl CompileWarnings {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, warning: CompileWarning) {
        self.0.push(warning);
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn extend(&mut self, other: CompileWarnings) {
        self.0.extend(other.0);
    }

    pub fn reports(&self) -> Vec<Report<'_, Loc>> {
        self.0.iter().map(|w| w.report()).collect()
    }
}

impl IntoIterator for CompileWarnings {
    type Item = CompileWarning;
    type IntoIter = std::vec::IntoIter<CompileWarning>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Evaluation errors that occur during runtime execution.
///
/// This is separate from CompileError to properly distinguish between
/// compile-time and runtime errors.
#[derive(Debug, Error)]
pub enum EvalError {
    #[error(transparent)]
    Runtime(#[from] RuntimeError),

    #[error(transparent)]
    Provider(#[from] ProviderError),

    #[error("Polars error: {0}")]
    Polars(#[from] polars::error::PolarsError),
}

impl EvalError {
    pub fn runtime(message: impl Into<String>, loc: Loc) -> Self {
        Self::Runtime(RuntimeError::evaluation(message, loc))
    }
}

impl From<EvalError> for CompileError {
    fn from(err: EvalError) -> Self {
        match err {
            EvalError::Runtime(e) => CompileError::Runtime(e),
            EvalError::Provider(e) => CompileError::Provider(e),
            EvalError::Polars(e) => CompileError::Provider(ProviderError::new(
                ProviderErrorKind::DataError(e.to_string()),
                Loc::generated(),
            )),
        }
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Resolution(#[from] ResolutionError),

    #[error(transparent)]
    Type(#[from] TypeError),

    #[error(transparent)]
    Provider(#[from] ProviderError),

    #[error(transparent)]
    Runtime(#[from] RuntimeError),

    #[error("Internal compiler error in {phase}: {message}")]
    Internal {
        phase: &'static str,
        message: String,
        loc: Loc,
    },
}

impl CompileError {
    pub fn internal(phase: &'static str, message: impl Into<String>, loc: Loc) -> Self {
        Self::Internal {
            phase,
            message: message.into(),
            loc,
        }
    }

    pub fn loc(&self) -> &Loc {
        match self {
            CompileError::Parse(e) => e.loc(),
            CompileError::Resolution(e) => e.loc(),
            CompileError::Type(e) => e.loc(),
            CompileError::Provider(e) => e.loc(),
            CompileError::Runtime(e) => e.loc(),
            CompileError::Internal { loc, .. } => loc,
        }
    }

    pub fn report(&self, interner: &Interner) -> Report<'static, Loc> {
        match self {
            CompileError::Parse(e) => e.report(interner),
            CompileError::Resolution(e) => e.report(interner),
            CompileError::Type(e) => e.report(interner),
            CompileError::Provider(e) => e.report(interner),
            CompileError::Runtime(e) => e.report(interner),
            CompileError::Internal {
                phase,
                message,
                loc,
            } => Report::build(ReportKind::Error, *loc)
                .with_config(report_config())
                .with_message(format!("Internal compiler error in {}: {}", phase, message))
                .with_label(
                    Label::new(*loc)
                        .with_message(message)
                        .with_color(Color::Red),
                )
                .with_help(format!(
                    "This is a bug in the {} phase of the compiler. Please file a bug report.",
                    phase
                ))
                .finish(),
        }
    }
}

#[derive(Debug, Default, Error)]
#[error("{} compilation error(s)", .0.len())]
pub struct CompileErrors(pub Vec<CompileError>);

impl CompileErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, error: impl Into<CompileError>) {
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

    pub fn reports(&self, interner: &Interner) -> Vec<Report<'_, Loc>> {
        self.0.iter().map(|e| e.report(interner)).collect()
    }
}

impl<E: Into<CompileError>> From<E> for CompileErrors {
    fn from(err: E) -> Self {
        Self(vec![err.into()])
    }
}

impl IntoIterator for CompileErrors {
    type Item = CompileError;
    type IntoIter = std::vec::IntoIter<CompileError>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl ProviderError {
    pub fn from_polars(err: polars::error::PolarsError, loc: Loc) -> Self {
        ProviderError::new(ProviderErrorKind::DataError(err.to_string()), loc)
    }

    pub fn from_io(err: std::io::Error, loc: Loc) -> Self {
        ProviderError::new(
            ProviderErrorKind::ReadError {
                path: "<unknown>".to_string(),
                cause: err.to_string(),
            },
            loc,
        )
    }

    pub fn from_io_with_path(err: std::io::Error, path: impl Into<String>, loc: Loc) -> Self {
        ProviderError::new(
            ProviderErrorKind::ReadError {
                path: path.into(),
                cause: err.to_string(),
            },
            loc,
        )
    }
}

impl CompileError {
    pub fn from_polars(err: polars::error::PolarsError, loc: Loc) -> Self {
        CompileError::Provider(ProviderError::from_polars(err, loc))
    }

    pub fn from_io(err: std::io::Error, loc: Loc) -> Self {
        CompileError::Provider(ProviderError::from_io(err, loc))
    }
}
