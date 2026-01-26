use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::ir::{ExprId, Param};

/// Subject URI pattern for RDF serialization
///
/// Defines how to construct subject URIs from record data.
/// Example: prefix="http://example.org/person/", id_column="id"
/// produces URIs like "http://example.org/person/123"
#[derive(Debug, Clone)]
pub struct SubjectPattern {
    /// URI prefix (e.g., "http://example.org/person/")
    pub prefix: String,
    /// Column name to use as ID suffix (e.g., "id")
    pub id_column: String,
}

/// A lazy plan for record processing
///
/// Contains a LazyFrame plus optional metadata for RDF serialization.
/// Everything stays lazy until a sink (like Rdf::serialize) executes the plan.
#[derive(Clone)]
pub struct RecordsPlan {
    /// The underlying lazy computation
    pub lf: LazyFrame,
    /// Type definition ID (for obtaining RDF metadata from type attributes)
    pub type_def_id: Option<DefId>,
    /// Subject URI pattern (set by Entity::with_id)
    pub subject_pattern: Option<SubjectPattern>,
    /// Select expressions for lazy transformation (set during tracing)
    /// When present, these expressions will be applied to a source LazyFrame
    pub select_exprs: Option<Vec<Expr>>,
}

impl RecordsPlan {
    /// Create a new plan from a LazyFrame
    pub fn new(lf: LazyFrame) -> Self {
        Self {
            lf,
            type_def_id: None,
            subject_pattern: None,
            select_exprs: None,
        }
    }

    /// Create a plan with type information
    pub fn with_type(lf: LazyFrame, type_def_id: DefId) -> Self {
        Self {
            lf,
            type_def_id: Some(type_def_id),
            subject_pattern: None,
            select_exprs: None,
        }
    }

    /// Create a plan from select expressions (for lazy transformation)
    pub fn from_exprs(exprs: Vec<Expr>, type_def_id: DefId) -> Self {
        Self {
            lf: LazyFrame::default(),
            type_def_id: Some(type_def_id),
            subject_pattern: None,
            select_exprs: Some(exprs),
        }
    }

    /// Add subject pattern to the plan
    pub fn with_subject_pattern(mut self, pattern: SubjectPattern) -> Self {
        self.subject_pattern = Some(pattern);
        self
    }

    /// Add type def id to the plan
    pub fn with_type_def_id(mut self, def_id: DefId) -> Self {
        self.type_def_id = Some(def_id);
        self
    }

    /// Add select expressions to the plan
    pub fn with_select_exprs(mut self, exprs: Vec<Expr>) -> Self {
        self.select_exprs = Some(exprs);
        self
    }
}

// TODO: Fix BindingId when module system is implemented
pub type BindingId = u32;

#[derive(Clone)]
pub enum Value {
    // Primitive values (concrete/materialized)
    Int(i64),
    String(Arc<str>),
    Bool(bool),
    Unit,

    // ===== LAZY VALUES (Polars expressions) =====

    /// A lazy column expression - NOT materialized
    /// Operations on this build more expressions: col("age") + lit(1)
    Expr(polars::prelude::Expr),

    /// A lazy plan for record processing (LazyFrame + metadata)
    /// Everything stays lazy until a sink executes the plan
    Records(RecordsPlan),

    /// Row context for tracing in List::map
    /// Field access returns Expr, not concrete values
    /// This enables lazy transformation of entire columns
    RowContext {
        /// Source schema for column name validation
        schema: Arc<Schema>,
    },

    // ===== FUNCTIONS =====

    Closure {
        params: Vec<Param>,
        body: ExprId,
        env: Rc<Environment>,
    },

    Function(BindingId),

    /// Builtin function from stdlib
    BuiltinFunction(
        DefId,
        std::sync::Arc<dyn crate::traits::function::FunctionImpl>,
    ),

    /// Field selector - represents a type-safe reference to a field name
    FieldSelector(Arc<str>),

    /// Record constructor (for types defined with record syntax)
    RecordConstructor(DefId),
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
