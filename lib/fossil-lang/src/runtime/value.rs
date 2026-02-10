use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::ir::{ExprId, Param};
use crate::traits::source::Source;

/// A transformation to apply to a LazyFrame
#[derive(Debug, Clone)]
pub enum Transform {
    /// Select specific columns/expressions
    Select(Vec<Expr>),
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
            outputs: Vec::new(),
            pending_exprs: None,
        }
    }

    /// Check if this is a pending transformation (not backed by a real source)
    pub fn is_pending(&self) -> bool {
        self.pending_exprs.is_some()
    }

    /// Check if this plan has multi-output specs
    pub fn has_outputs(&self) -> bool {
        !self.outputs.is_empty()
    }
}

/// Output specification for a single type in an OutputPlan
///
/// Contains the type's DefId (for metadata lookup), the transformation
/// expressions to apply to the source data, and constructor arguments.
#[derive(Clone)]
pub struct OutputSpec {
    /// DefId of the output type (for obtaining RdfMetadata)
    pub type_def_id: DefId,
    /// Transformation expressions to apply to source data
    /// These are built from tracing a closure against the source schema
    pub select_exprs: Vec<Expr>,
    /// Output schema after transformation
    pub schema: Arc<Schema>,
    /// Constructor arguments (positional: subject, graph, ...)
    pub ctor_args: Vec<Expr>,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Interner;
    use polars::prelude::{lit, Schema};

    // ── Value tests ──────────────────────────────────────────────

    #[test]
    fn value_unit() {
        let v = Value::Unit;
        let v2 = v.clone();
        assert!(matches!(v, Value::Unit));
        assert!(matches!(v2, Value::Unit));
    }

    #[test]
    fn value_as_literal_string_from_expr() {
        let v = Value::Expr(lit("hello"));
        assert_eq!(v.as_literal_string(), Some("hello".to_string()));
    }

    #[test]
    fn value_as_literal_string_from_non_string() {
        let v = Value::Expr(lit(42));
        assert_eq!(v.as_literal_string(), None);
    }

    #[test]
    fn value_as_literal_string_from_unit() {
        assert_eq!(Value::Unit.as_literal_string(), None);
    }

    #[test]
    fn value_as_literal_string_from_plan() {
        let v = Value::Plan(Plan::empty(Schema::default()));
        assert_eq!(v.as_literal_string(), None);
    }

    // ── Environment tests ────────────────────────────────────────

    #[test]
    fn env_bind_and_lookup() {
        let mut interner = Interner::default();
        let sym_x = interner.intern("x");

        let mut env = Environment::default();
        env.bind(sym_x, Value::Expr(lit(42)));

        assert!(env.lookup(sym_x).is_some());
    }

    #[test]
    fn env_lookup_missing() {
        let mut interner = Interner::default();
        let sym_x = interner.intern("x");

        let env = Environment::default();
        assert!(env.lookup(sym_x).is_none());
    }

    #[test]
    fn env_overwrite_binding() {
        let mut interner = Interner::default();
        let sym_x = interner.intern("x");

        let mut env = Environment::default();
        env.bind(sym_x, Value::Expr(lit(1)));
        env.bind(sym_x, Value::Expr(lit(2)));

        // Should retrieve the latest binding
        let val = env.lookup(sym_x).unwrap();
        match val {
            Value::Expr(expr) => {
                let s = format!("{:?}", expr);
                assert!(s.contains('2'), "expected lit(2), got {:?}", s);
            }
            _ => panic!("expected Value::Expr"),
        }
    }

    #[test]
    fn env_multiple_bindings() {
        let mut interner = Interner::default();
        let sym_x = interner.intern("x");
        let sym_y = interner.intern("y");
        let sym_z = interner.intern("z");

        let mut env = Environment::default();
        env.bind(sym_x, Value::Expr(lit(1)));
        env.bind(sym_y, Value::Expr(lit("two")));
        env.bind(sym_z, Value::Unit);

        assert!(env.lookup(sym_x).is_some());
        assert!(env.lookup(sym_y).is_some());
        assert!(env.lookup(sym_z).is_some());
    }

    #[test]
    fn env_default_empty() {
        let mut interner = Interner::default();
        let sym_a = interner.intern("a");

        let env = Environment::default();
        assert!(env.lookup(sym_a).is_none());
        assert_eq!(env.bindings.len(), 0);
    }
}
