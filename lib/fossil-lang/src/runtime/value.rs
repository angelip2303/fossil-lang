use std::collections::HashMap;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::traits::function::FunctionImpl;
use crate::traits::source::Source;

#[derive(Debug, Clone)]
pub enum Transform {
    Select(Vec<Expr>),
    Filter(Expr),
}

#[derive(Clone, Debug)]
pub struct Plan {
    pub source: Option<Box<dyn Source>>,
    pub transforms: Vec<Transform>,
    pub type_def_id: Option<DefId>,
    pub schema: Arc<Schema>,
    pub outputs: Vec<OutputSpec>,
    pub pending_exprs: Option<Vec<Expr>>,
    pub ctor_exprs: Vec<Expr>,
}

impl Plan {
    pub fn from_source(source: Box<dyn Source>, schema: Schema) -> Self {
        Self {
            source: Some(source),
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(schema),
            outputs: Vec::new(),
            pending_exprs: None,
            ctor_exprs: vec![],
        }
    }

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
            ctor_exprs: vec![],
        }
    }

    pub fn pending(select_exprs: Vec<Expr>, type_def_id: DefId, schema: Schema) -> Self {
        Self {
            source: None,
            transforms: Vec::new(),
            type_def_id: Some(type_def_id),
            schema: Arc::new(schema),
            outputs: Vec::new(),
            pending_exprs: Some(select_exprs),
            ctor_exprs: vec![],
        }
    }

    pub fn empty(schema: Schema) -> Self {
        Self {
            source: None,
            transforms: Vec::new(),
            type_def_id: None,
            schema: Arc::new(schema),
            outputs: Vec::new(),
            pending_exprs: None,
            ctor_exprs: vec![],
        }
    }

    pub fn is_pending(&self) -> bool {
        self.pending_exprs.is_some()
    }

    pub fn has_outputs(&self) -> bool {
        !self.outputs.is_empty()
    }
}

#[derive(Clone, Debug)]
pub struct OutputSpec {
    pub type_def_id: DefId,
    pub select_exprs: Vec<Expr>,
    pub schema: Arc<Schema>,
    pub ctor_args: Vec<Expr>,
}

#[derive(Clone)]
pub enum Value {
    Unit,
    Expr(Expr),
    Plan(Plan),
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
}

fn extract_literal<T>(
    expr: &Expr,
    extract_from_any: impl Fn(&AnyValue<'_>) -> Option<T>,
) -> Option<T> {
    match expr {
        Expr::Literal(lv) => {
            if let Some(av) = lv.to_any_value()
                && let Some(val) = extract_from_any(&av)
            {
                return Some(val);
            }
            if let LiteralValue::Scalar(scalar) = lv {
                return extract_from_any(scalar.value());
            }
            None
        }
        _ => None,
    }
}

fn extract_literal_string(expr: &Expr) -> Option<String> {
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
    use polars::prelude::lit;

    #[test]
    fn value_as_literal_string_from_expr() {
        let v = Value::Expr(lit("hello"));
        assert_eq!(v.as_literal_string(), Some("hello".to_string()));
    }

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
}
