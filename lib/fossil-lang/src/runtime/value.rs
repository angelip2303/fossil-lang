use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::ir::{ExprId, Param};
use crate::traits::source::Source;

/// A transformation to apply to a LazyFrame
///
/// Transformations are accumulated and only applied when the plan is executed.
#[derive(Debug, Clone)]
pub enum Transform {
    /// Select specific columns/expressions
    Select(Vec<Expr>),
    /// Filter rows based on a predicate
    Filter(Expr),
    /// Add or replace a column
    WithColumn(Expr),
}

impl Transform {
    /// Apply this transformation to a LazyFrame
    pub fn apply(&self, lf: LazyFrame) -> LazyFrame {
        match self {
            Transform::Select(exprs) => lf.select(exprs.clone()),
            Transform::Filter(expr) => lf.filter(expr.clone()),
            Transform::WithColumn(expr) => lf.with_column(expr.clone()),
        }
    }
}

/// A lazy plan for data processing
///
/// Contains an optional source and accumulated transformations.
/// Nothing is materialized until a sink executes the plan.
/// This design guarantees constant memory usage regardless of dataset size.
#[derive(Clone)]
pub struct Plan {
    /// Data source (None = empty/pending plan)
    pub source: Option<Box<dyn Source>>,
    /// Transformations to apply (in order)
    pub transforms: Vec<Transform>,
    /// Type definition ID (for obtaining type metadata from attributes)
    pub type_def_id: Option<DefId>,
    /// Schema of the output (known at compile-time)
    pub schema: Arc<Schema>,
    /// Meta-fields from record construction (@name = expr)
    pub meta_fields: MetaFields,
    /// Output specs for multi-output serialization
    /// Empty = single output normal plan, non-empty = multi-output plan
    pub outputs: Vec<OutputSpec>,
    /// Pending select expressions (used during tracing)
    /// When Some, this plan is a template to be applied to a real source later
    pub pending_exprs: Option<Vec<Expr>>,
}

impl Plan {
    /// Create a new plan from a source and schema
    pub fn from_source(source: Box<dyn Source>, schema: Schema) -> Self {
        Self {
            source: Some(source),
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(schema),
            meta_fields: Vec::new(),
            outputs: Vec::new(),
            pending_exprs: None,
        }
    }

    /// Create a plan with source and type information
    pub fn from_source_with_type(
        source: Box<dyn Source>,
        schema: Schema,
        type_def_id: DefId,
    ) -> Self {
        Self {
            source: Some(source),
            transforms: Vec::new(),
            type_def_id: Some(type_def_id),
            schema: Arc::new(schema),
            meta_fields: Vec::new(),
            outputs: Vec::new(),
            pending_exprs: None,
        }
    }

    /// Create a pending transformation plan (used during tracing)
    ///
    /// This creates a "template" that will be applied to a real source later.
    /// The schema represents the output schema after the transformation.
    pub fn pending(select_exprs: Vec<Expr>, type_def_id: DefId, schema: Schema) -> Self {
        Self {
            source: None,
            transforms: Vec::new(),
            type_def_id: Some(type_def_id),
            schema: Arc::new(schema),
            meta_fields: Vec::new(),
            outputs: Vec::new(),
            pending_exprs: Some(select_exprs),
        }
    }

    /// Create an empty plan (for empty lists/tracing context)
    pub fn empty(schema: Schema) -> Self {
        Self {
            source: None,
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(schema),
            meta_fields: Vec::new(),
            outputs: Vec::new(),
            pending_exprs: None,
        }
    }

    /// Add a transformation to the plan
    pub fn add_transform(mut self, transform: Transform) -> Self {
        self.transforms.push(transform);
        self
    }

    /// Add type def id to the plan
    pub fn with_type_def_id(mut self, def_id: DefId) -> Self {
        self.type_def_id = Some(def_id);
        self
    }

    /// Update the schema (after a transformation changes it)
    pub fn with_schema(mut self, schema: Schema) -> Self {
        self.schema = Arc::new(schema);
        self
    }

    /// Check if this is a pending transformation (not backed by a real source)
    pub fn is_pending(&self) -> bool {
        self.pending_exprs.is_some()
    }

    /// Take pending expressions (consumes them)
    pub fn take_pending_exprs(&mut self) -> Option<Vec<Expr>> {
        self.pending_exprs.take()
    }

    /// Apply a pending transformation to this plan
    ///
    /// If `other` is a pending plan (from tracing), extracts its select expressions
    /// and adds them as a Transform::Select to this plan.
    pub fn apply_pending(&self, other: &Plan) -> Option<Self> {
        if let Some(ref select_exprs) = other.pending_exprs {
            let mut result = self.clone();
            result
                .transforms
                .push(Transform::Select(select_exprs.clone()));
            result.type_def_id = other.type_def_id;
            result.schema = other.schema.clone();
            result.outputs = other.outputs.clone();
            Some(result)
        } else {
            None
        }
    }

    /// Check if this plan has multi-output specs
    pub fn has_outputs(&self) -> bool {
        !self.outputs.is_empty()
    }

    /// Add an output spec to the plan (converts to multi-output plan)
    pub fn with_output(mut self, output: OutputSpec) -> Self {
        self.outputs.push(output);
        self
    }

    /// Set output specs for multi-output serialization
    pub fn with_outputs(mut self, outputs: Vec<OutputSpec>) -> Self {
        self.outputs = outputs;
        self
    }
}

/// Output specification for a single type in an OutputPlan
///
/// Contains the type's DefId (for metadata lookup), the transformation
/// expressions to apply to the source data, and projection attributes.
#[derive(Clone)]
pub struct OutputSpec {
    /// DefId of the output type (for obtaining RdfMetadata)
    pub type_def_id: DefId,
    /// Transformation expressions to apply to source data
    /// These are built from tracing a closure against the source schema
    pub select_exprs: Vec<Expr>,
    /// Output schema after transformation
    pub schema: Arc<Schema>,
    /// Meta-fields from record construction (@name = expr)
    pub meta_fields: MetaFields,
}

pub type MetaFields = Vec<(crate::context::Symbol, Expr)>;

/// Runtime values in the Fossil language
///
/// All data values are represented as Polars expressions (`Expr`), enabling
/// lazy evaluation and streaming processing. This unified representation means:
/// - Literals become `lit(...)` expressions
/// - Column references become `col(...)` expressions
/// - Operations build expression trees: `col("age") + lit(1)`
///
/// The type system guarantees correctness at compile-time, so runtime values
/// don't need separate variants for Int/String/Bool.
#[derive(Clone)]
pub enum Value {
    /// Unit value (void/nothing)
    Unit,

    /// A Polars expression - ALL data values are expressions
    ///
    /// This includes:
    /// - Literals: `lit(42)`, `lit("hello")`, `lit(true)`
    /// - Columns: `col("name")`
    /// - Operations: `col("age") + lit(1)`
    /// - String concatenation: `concat_str([...])`
    Expr(polars::prelude::Expr),

    /// A lazy plan for data processing (source + transforms)
    /// Everything stays lazy until a sink executes the plan.
    /// Also used as tracing context in to() (with Empty source).
    /// When `outputs` is non-empty, this is a multi-output plan for serialization.
    Plan(Plan),

    /// User-defined function with captured environment
    Closure {
        params: Vec<Param>,
        body: ExprId,
        env: Rc<Environment>,
        /// Optional attributes on the closure (e.g., #[rdf(id = "...")])
        attrs: Vec<crate::ast::ast::Attribute>,
    },

    /// Builtin function from stdlib
    BuiltinFunction(
        DefId,
        std::sync::Arc<dyn crate::traits::function::FunctionImpl>,
    ),

    /// Record constructor (for types defined with record syntax)
    RecordConstructor(DefId),
}

impl Value {
    /// Try to extract a literal string from an Expr value
    ///
    /// Used by sinks that need concrete paths (e.g., Rdf::serialize)
    pub fn as_literal_string(&self) -> Option<String> {
        match self {
            Value::Expr(expr) => extract_literal_string(expr),
            _ => None,
        }
    }

    /// Try to extract a literal i64 from an Expr value
    pub fn as_literal_int(&self) -> Option<i64> {
        match self {
            Value::Expr(expr) => extract_literal_int(expr),
            _ => None,
        }
    }

    /// Try to extract a literal bool from an Expr value
    pub fn as_literal_bool(&self) -> Option<bool> {
        match self {
            Value::Expr(expr) => extract_literal_bool(expr),
            _ => None,
        }
    }
}

/// Generic helper to extract a value from a Polars literal expression
///
/// Handles both dynamic literals (from `lit(...)`) and scalar values.
fn extract_literal<T>(
    expr: &polars::prelude::Expr,
    extract_from_any: impl Fn(&polars::prelude::AnyValue<'_>) -> Option<T>,
) -> Option<T> {
    use polars::prelude::{Expr, LiteralValue};

    match expr {
        Expr::Literal(lv) => {
            // Try to convert to AnyValue first
            if let Some(av) = lv.to_any_value()
                && let Some(val) = extract_from_any(&av)
            {
                return Some(val);
            }
            // Also handle Scalar directly
            if let LiteralValue::Scalar(scalar) = lv {
                return extract_from_any(scalar.value());
            }
            None
        }
        _ => None,
    }
}

/// Extract a string literal from a Polars expression
fn extract_literal_string(expr: &polars::prelude::Expr) -> Option<String> {
    use polars::prelude::AnyValue;

    extract_literal(expr, |av| match av {
        AnyValue::String(s) => Some(s.to_string()),
        AnyValue::StringOwned(s) => Some(s.to_string()),
        _ => None,
    })
}

/// Extract an integer literal from a Polars expression
fn extract_literal_int(expr: &polars::prelude::Expr) -> Option<i64> {
    use polars::prelude::AnyValue;

    extract_literal(expr, |av| match av {
        AnyValue::Int64(i) => Some(*i),
        AnyValue::Int32(i) => Some(*i as i64),
        AnyValue::Int16(i) => Some(*i as i64),
        AnyValue::Int8(i) => Some(*i as i64),
        AnyValue::UInt64(i) => Some(*i as i64),
        AnyValue::UInt32(i) => Some(*i as i64),
        AnyValue::UInt16(i) => Some(*i as i64),
        AnyValue::UInt8(i) => Some(*i as i64),
        _ => None,
    })
}

/// Extract a boolean literal from a Polars expression
fn extract_literal_bool(expr: &polars::prelude::Expr) -> Option<bool> {
    use polars::prelude::AnyValue;

    extract_literal(expr, |av| match av {
        AnyValue::Boolean(b) => Some(*b),
        _ => None,
    })
}

#[derive(Clone, Default)]
pub struct Environment {
    bindings: HashMap<Symbol, Value>,
}

impl Environment {
    pub fn bind(&mut self, name: Symbol, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn lookup(&self, name: Symbol) -> Option<&Value> {
        self.bindings.get(&name)
    }

    pub fn with_binding(mut self, name: Symbol, value: Value) -> Self {
        self.bind(name, value);
        self
    }
}
