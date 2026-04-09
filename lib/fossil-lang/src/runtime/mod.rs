//! DEPRECATED: Runtime module — replaced by RQ + FossilPlan.
//!
//! Minimal stubs kept for downstream crate compatibility.
//! Will be fully removed when downstream crates migrate to FunctionDef.

pub mod executor {
    /// Output produced during script execution.
    #[derive(Debug, Clone)]
    pub struct OutputRecord {
        pub kind: String,
        pub path: String,
        pub metadata: serde_json::Value,
    }

    /// Result of executing a fossil script.
    pub struct ExecutionResult {
        pub values: Vec<super::value::Value>,
        pub outputs: Vec<OutputRecord>,
    }

    pub struct IrExecutor;

    impl IrExecutor {
        pub fn execute(_program: crate::passes::IrProgram) -> Result<ExecutionResult, crate::error::FossilError> {
            Err(crate::error::FossilError::evaluation(
                "IrExecutor is deprecated. Use compile_to_plan() instead.".to_string(),
                crate::ast::Loc::generated(),
            ))
        }
    }
}

pub mod value {
    use std::sync::Arc;
    use crate::context::{DefId, Symbol};
    use crate::traits::function::FunctionImpl;

    /// Value type — Polars types replaced with stubs.
    ///
    /// Frame and Emission variants exist for backward compat but contain
    /// deprecated types. New code should use FossilPlan + RQ instead.
    #[derive(Clone)]
    pub enum Value {
        Unit,
        Expr(polars::prelude::Expr),
        Frame(polars::prelude::LazyFrame),
        Emission(Emission),
        Reference { def_id: DefId, args: Vec<Value> },
        Function(DefId, Arc<dyn FunctionImpl>),
        RecordConstructor(DefId),
    }

    impl Value {
        pub fn as_literal_string(&self) -> Option<String> {
            match self {
                Value::Expr(expr) => extract_literal_string(expr),
                _ => None,
            }
        }
        pub fn as_expr(&self) -> Option<&polars::prelude::Expr> {
            match self {
                Value::Expr(e) => Some(e),
                _ => None,
            }
        }
        pub fn into_frame(self) -> Option<polars::prelude::LazyFrame> {
            match self {
                Value::Frame(f) => Some(f),
                Value::Emission(e) => Some(e.frame),
                _ => None,
            }
        }
    }

    fn extract_literal_string(expr: &polars::prelude::Expr) -> Option<String> {
        match expr {
            polars::prelude::Expr::Literal(lv) => {
                if let Some(av) = lv.to_any_value() {
                    match av {
                        polars::prelude::AnyValue::String(s) => return Some(s.to_string()),
                        polars::prelude::AnyValue::StringOwned(s) => return Some(s.to_string()),
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        }
    }

    impl std::fmt::Debug for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Unit => write!(f, "Unit"),
                Value::Expr(_) => write!(f, "Expr(...)"),
                Value::Frame(_) => write!(f, "Frame(...)"),
                Value::Emission(e) => write!(f, "Emission({:?})", e),
                Value::Reference { def_id, .. } => write!(f, "Ref({def_id:?})"),
                Value::Function(id, _) => write!(f, "Fn({id:?})"),
                Value::RecordConstructor(id) => write!(f, "Ctor({id:?})"),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct EmissionDef {
        pub type_def_id: DefId,
        pub select_exprs: Vec<polars::prelude::Expr>,
        pub ctor_args: Vec<polars::prelude::Expr>,
    }

    #[derive(Clone)]
    pub struct Emission {
        pub frame: polars::prelude::LazyFrame,
        pub specs: Vec<EmissionDef>,
    }

    impl std::fmt::Debug for Emission {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Emission").field("specs", &self.specs).finish()
        }
    }

    #[derive(Clone, Default)]
    pub struct Environment {
        bindings: std::collections::HashMap<Symbol, Value>,
    }

    impl Environment {
        pub fn bind(&mut self, name: Symbol, value: Value) { self.bindings.insert(name, value); }
        pub fn lookup(&self, name: Symbol) -> Option<&Value> { self.bindings.get(&name) }
    }
}
