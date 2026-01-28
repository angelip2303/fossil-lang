use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::ir::{ExprId, Param};

/// Describes the source of data for a RecordsPlan
///
/// This is a description, not actual data. The source is only
/// materialized when a sink (like Rdf::serialize) executes the plan.
#[derive(Clone)]
pub enum SourceDescriptor {
    /// CSV file source
    Csv {
        path: String,
        delimiter: u8,
        has_header: bool,
    },
    /// Parquet file source
    Parquet { path: String },
    /// In-memory DataFrame (for tests or small datasets)
    /// This is the only variant that holds actual data
    InMemory(Arc<DataFrame>),
    /// Empty source (for empty lists/records)
    Empty,
    /// Pending transformation (used during List::map tracing)
    /// Contains select expressions that will be applied to a source later.
    /// This is NOT a real source - it's a transformation template.
    Pending { select_exprs: Vec<Expr> },
    /// Concatenation of multiple sources (lazy union)
    /// Used when combining multiple RecordsPlan values.
    Concat(Vec<Box<RecordsPlan>>),
    /// Join of two sources (lazy)
    /// Keeps the join lazy until execution at the sink.
    Join {
        left: Box<RecordsPlan>,
        right: Box<RecordsPlan>,
        left_on: String,
        right_on: String,
    },
}

impl SourceDescriptor {
    /// Check if this is a pending transformation (not a real source)
    pub fn is_pending(&self) -> bool {
        matches!(self, SourceDescriptor::Pending { .. })
    }

    /// Extract select expressions from a Pending source
    pub fn take_select_exprs(&self) -> Option<Vec<Expr>> {
        match self {
            SourceDescriptor::Pending { select_exprs } => Some(select_exprs.clone()),
            _ => None,
        }
    }
}

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

/// A lazy plan for record processing
///
/// Contains a source descriptor and accumulated transformations.
/// Nothing is materialized until a sink executes the plan.
/// This design guarantees constant memory usage regardless of dataset size.
#[derive(Clone)]
pub struct RecordsPlan {
    /// Where the data comes from (file path, format, options)
    pub source: SourceDescriptor,
    /// Transformations to apply (in order)
    pub transforms: Vec<Transform>,
    /// Type definition ID (for obtaining type metadata from attributes)
    pub type_def_id: Option<DefId>,
    /// Schema of the output (known at compile-time)
    pub schema: Arc<Schema>,
    /// Identity expression for entity URIs (used by RDF serialization)
    /// This is a Polars expression that produces the subject URI for each row.
    pub identity_expr: Option<Expr>,
}

impl RecordsPlan {
    /// Create a new plan from a source descriptor and schema
    pub fn new(source: SourceDescriptor, schema: Schema) -> Self {
        Self {
            source,
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(schema),
            identity_expr: None,
        }
    }

    /// Create a plan with type information
    pub fn with_type(source: SourceDescriptor, schema: Schema, type_def_id: DefId) -> Self {
        Self {
            source,
            transforms: Vec::new(),
            type_def_id: Some(type_def_id),
            schema: Arc::new(schema),
            identity_expr: None,
        }
    }

    /// Create a pending transformation plan (used during List::map tracing)
    ///
    /// This creates a "template" that will be applied to a real source later.
    /// The schema represents the output schema after the transformation.
    pub fn from_exprs(select_exprs: Vec<Expr>, type_def_id: DefId, schema: Schema) -> Self {
        Self {
            source: SourceDescriptor::Pending { select_exprs },
            transforms: Vec::new(),
            type_def_id: Some(type_def_id),
            schema: Arc::new(schema),
            identity_expr: None,
        }
    }

    /// Create an empty plan (for empty lists/records)
    pub fn empty() -> Self {
        Self {
            source: SourceDescriptor::Empty,
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(Schema::default()),
            identity_expr: None,
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
        self.source.is_pending()
    }

    /// Apply a pending transformation to this plan
    ///
    /// If `other` is a pending plan (from tracing), extracts its select expressions
    /// and adds them as a Transform::Select to this plan.
    pub fn apply_pending(&self, other: &RecordsPlan) -> Option<Self> {
        if let Some(select_exprs) = other.source.take_select_exprs() {
            let mut result = self.clone();
            result.transforms.push(Transform::Select(select_exprs));
            result.type_def_id = other.type_def_id;
            result.schema = other.schema.clone();
            Some(result)
        } else {
            None
        }
    }

    // NOTE: No to_lazy_frame() method here!
    // Execution is ONLY done through ChunkedExecutor in the sink.
    // This ensures no accidental materialization can happen.
}

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

    /// A lazy plan for record processing (source + transforms)
    /// Everything stays lazy until a sink executes the plan.
    /// Also used as tracing context in List::map (with Empty source).
    Records(RecordsPlan),

    // ===== FUNCTIONS (code, not data) =====
    /// User-defined function with captured environment
    Closure {
        params: Vec<Param>,
        body: ExprId,
        env: Rc<Environment>,
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

/// Extract a string literal from a Polars expression
///
/// Handles both dynamic literals (from `lit("string")`) and scalar values.
fn extract_literal_string(expr: &polars::prelude::Expr) -> Option<String> {
    use polars::prelude::{AnyValue, Expr, LiteralValue};

    match expr {
        Expr::Literal(lv) => {
            // Try to convert to AnyValue and extract string
            if let Some(av) = lv.to_any_value() {
                match av {
                    AnyValue::String(s) => return Some(s.to_string()),
                    AnyValue::StringOwned(s) => return Some(s.to_string()),
                    _ => {}
                }
            }
            // Also handle Scalar directly
            if let LiteralValue::Scalar(scalar) = lv {
                match scalar.value() {
                    AnyValue::String(s) => return Some(s.to_string()),
                    AnyValue::StringOwned(s) => return Some(s.to_string()),
                    _ => {}
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract an integer literal from a Polars expression
fn extract_literal_int(expr: &polars::prelude::Expr) -> Option<i64> {
    use polars::prelude::{AnyValue, Expr, LiteralValue};

    match expr {
        Expr::Literal(lv) => {
            // Try to convert to AnyValue and extract int
            if let Some(av) = lv.to_any_value() {
                match av {
                    AnyValue::Int64(i) => return Some(i),
                    AnyValue::Int32(i) => return Some(i as i64),
                    AnyValue::Int16(i) => return Some(i as i64),
                    AnyValue::Int8(i) => return Some(i as i64),
                    AnyValue::UInt64(i) => return Some(i as i64),
                    AnyValue::UInt32(i) => return Some(i as i64),
                    AnyValue::UInt16(i) => return Some(i as i64),
                    AnyValue::UInt8(i) => return Some(i as i64),
                    _ => {}
                }
            }
            // Also handle Scalar directly
            if let LiteralValue::Scalar(scalar) = lv {
                match scalar.value() {
                    AnyValue::Int64(i) => return Some(*i),
                    AnyValue::Int32(i) => return Some(*i as i64),
                    AnyValue::Int16(i) => return Some(*i as i64),
                    AnyValue::Int8(i) => return Some(*i as i64),
                    AnyValue::UInt64(i) => return Some(*i as i64),
                    AnyValue::UInt32(i) => return Some(*i as i64),
                    AnyValue::UInt16(i) => return Some(*i as i64),
                    AnyValue::UInt8(i) => return Some(*i as i64),
                    _ => {}
                }
            }
            None
        }
        _ => None,
    }
}

/// Extract a boolean literal from a Polars expression
fn extract_literal_bool(expr: &polars::prelude::Expr) -> Option<bool> {
    use polars::prelude::{AnyValue, Expr, LiteralValue};

    match expr {
        Expr::Literal(lv) => {
            // Try to convert to AnyValue and extract bool
            if let Some(av) = lv.to_any_value() {
                if let AnyValue::Boolean(b) = av {
                    return Some(b);
                }
            }
            // Also handle Scalar directly
            if let LiteralValue::Scalar(scalar) = lv {
                if let AnyValue::Boolean(b) = scalar.value() {
                    return Some(*b);
                }
            }
            None
        }
        _ => None,
    }
}

/// Environment for variable bindings during execution
#[derive(Clone, Default)]
pub struct Environment {
    bindings: HashMap<Symbol, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

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
