use std::collections::HashMap;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::traits::function::FunctionImpl;
use crate::traits::source::Source;

#[derive(Debug, Clone)]
pub struct JoinTransform {
    pub right_source: Option<Box<dyn Source>>,
    pub right_transforms: Vec<Transform>,
    pub left_on: Vec<Expr>,
    pub right_on: Vec<Expr>,
    pub suffix: String,
}

#[derive(Debug, Clone)]
pub enum Transform {
    Select(Vec<Expr>),
    Filter(Expr),
    Join(JoinTransform),
}

#[derive(Clone, Debug)]
pub struct Plan {
    pub source: Option<Box<dyn Source>>,
    pub transforms: Vec<Transform>,
    pub schema: Arc<Schema>,
    pub outputs: Vec<OutputSpec>,
}

impl Plan {
    pub fn from_source(source: Box<dyn Source>, schema: Schema) -> Self {
        Self {
            source: Some(source),
            transforms: Vec::new(),
            schema: Arc::new(schema),
            outputs: Vec::new(),
        }
    }

    pub fn empty(schema: Schema) -> Self {
        Self {
            source: None,
            transforms: Vec::new(),
            schema: Arc::new(schema),
            outputs: Vec::new(),
        }
    }

    /// Column selection/transformation — like lf.select([...])
    pub fn select(mut self, exprs: Vec<Expr>) -> Self {
        self.schema = Arc::new(schema_from_exprs(&exprs));
        self.transforms.push(Transform::Select(exprs));
        self.outputs.clear();
        self
    }

    /// Row filtering — like lf.filter(...)
    pub fn filter(mut self, expr: Expr) -> Self {
        self.transforms.push(Transform::Filter(expr));
        self.outputs.clear();
        self
    }

    /// Join — like lf.join(...)
    pub fn join(
        mut self,
        right: Plan,
        left_on: Vec<Expr>,
        right_on: Vec<Expr>,
        suffix: String,
    ) -> Self {
        let merged = self.merge_schema(&right.schema, &suffix);
        self.transforms.push(Transform::Join(JoinTransform {
            right_source: right.source,
            right_transforms: right.transforms,
            left_on,
            right_on,
            suffix,
        }));
        self.schema = Arc::new(merged);
        self.outputs.clear();
        self
    }

    /// Projection: single-output with no ctor_args bakes Select into pipeline;
    /// otherwise stores OutputSpecs for downstream consumers (e.g. RDF serializer).
    pub fn project(self, outputs: Vec<OutputSpec>) -> Self {
        let mut plan = if outputs.len() == 1 && outputs[0].ctor_args.is_empty() {
            self.select(outputs[0].select_exprs.clone())
        } else {
            self
        };
        plan.outputs = outputs;
        plan
    }

    pub fn has_outputs(&self) -> bool {
        !self.outputs.is_empty()
    }

    fn merge_schema(&self, right: &Schema, suffix: &str) -> Schema {
        let mut fields: Vec<Field> = self
            .schema
            .iter()
            .map(|(name, dtype)| Field::new(name.clone(), dtype.clone()))
            .collect();

        let left_names: std::collections::HashSet<&str> =
            self.schema.iter_names().map(|n| n.as_str()).collect();

        for (name, dtype) in right.iter() {
            let final_name = if left_names.contains(name.as_str()) {
                format!("{}{}", name, suffix)
            } else {
                name.to_string()
            };
            fields.push(Field::new(final_name.into(), dtype.clone()));
        }

        Schema::from_iter(fields)
    }
}

fn schema_from_exprs(exprs: &[Expr]) -> Schema {
    Schema::from_iter(exprs.iter().filter_map(|expr| {
        if let Expr::Alias(_, name) = expr {
            Some(Field::new(
                name.clone(),
                DataType::Unknown(Default::default()),
            ))
        } else {
            None
        }
    }))
}

/// Output specification not yet attached to a plan.
/// Created by named record constructors inside projections.
#[derive(Clone, Debug)]
pub struct PendingOutput {
    pub type_def_id: DefId,
    pub select_exprs: Vec<Expr>,
    pub ctor_exprs: Vec<Expr>,
    pub schema: Arc<Schema>,
}

impl PendingOutput {
    pub fn into_output_spec(self) -> OutputSpec {
        OutputSpec {
            type_def_id: self.type_def_id,
            select_exprs: self.select_exprs,
            schema: self.schema,
            ctor_args: self.ctor_exprs,
        }
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
    PendingOutput(PendingOutput),
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
