use std::collections::HashMap;
use std::sync::Arc;

use polars::prelude::*;

use crate::context::{DefId, Symbol};
use crate::traits::function::FunctionImpl;

/// A single emission definition produced by record instances in projection context.
#[derive(Clone, Debug)]
pub struct EmissionDef {
    pub type_def_id: DefId,
    pub select_exprs: Vec<Expr>,
    pub ctor_args: Vec<Expr>,
}

/// A frame paired with its emission specifications (produced by projection).
#[derive(Clone)]
pub struct Emission {
    pub frame: LazyFrame,
    pub specs: Vec<EmissionDef>,
}

impl std::fmt::Debug for Emission {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Emission")
            .field("specs", &self.specs)
            .finish()
    }
}

#[derive(Clone)]
pub enum Value {
    Unit,
    Expr(Expr),
    Frame(LazyFrame),
    Emission(Emission),
    /// Ephemeral value produced by `ref Type(args)` or constructor-only calls.
    /// Lives only during each-block evaluation; the serializer builds the
    /// subject IRI via `build_subject_expr`.
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

    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            Value::Expr(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_frame(self) -> Option<LazyFrame> {
        match self {
            Value::Frame(f) => Some(f),
            Value::Emission(e) => Some(e.frame),
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

/// Estimate optimal batch size based on schema.
///
/// Targets approximately 100MB per batch for balanced memory/performance.
pub fn estimate_batch_size(schema: &Schema) -> usize {
    let row_bytes: usize = schema
        .iter()
        .map(|(_, dtype)| estimate_dtype_size(dtype))
        .sum();

    const TARGET_BYTES: usize = 100 * 1024 * 1024;
    (TARGET_BYTES / row_bytes.max(1)).clamp(10_000, 500_000)
}

fn estimate_dtype_size(dtype: &DataType) -> usize {
    match dtype {
        DataType::Boolean => 1,
        DataType::Int8 | DataType::UInt8 => 1,
        DataType::Int16 | DataType::UInt16 => 2,
        DataType::Int32 | DataType::UInt32 | DataType::Float32 => 4,
        DataType::Int64 | DataType::UInt64 | DataType::Float64 => 8,
        DataType::Date => 4,
        DataType::Datetime(_, _) | DataType::Duration(_) | DataType::Time => 8,
        DataType::String => 64,
        DataType::Binary => 128,
        DataType::List(inner) => 8 + estimate_dtype_size(inner) * 10,
        DataType::Struct(fields) => fields.iter().map(|f| estimate_dtype_size(f.dtype())).sum(),
        DataType::Null => 0,
        _ => 32,
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
