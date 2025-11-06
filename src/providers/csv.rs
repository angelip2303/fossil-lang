use indexmap::IndexMap;
use polars::prelude::*;

use crate::const_eval::ConstEvaluator;
use crate::error::Result;
use crate::module::Module;
use crate::providers::{Arg, Ast, TypeProvider};
use crate::runtime::value::ListRepr;
use crate::runtime::{RuntimeFunction, Value};
use crate::solver::Type;

pub struct CsvProvider;

impl TypeProvider for CsvProvider {
    fn param_types(&self) -> Vec<Type> {
        vec![Type::String]
    }

    fn provide_type(&self, ast: &Ast, args: &[Arg]) -> Result<Type> {
        let eval = ConstEvaluator::new(ast);
        let path = eval.eval_to_string(args[0].value)?;
        let path = PlPath::from_str(&path);

        let mut lf = LazyCsvReader::new(path).finish()?;
        let schema = lf.collect_schema()?;

        let fields: IndexMap<String, Box<Type>> = schema
            .iter()
            .map(|(name, dtype)| {
                let ty = match dtype {
                    DataType::Boolean => Type::Bool,
                    DataType::Int64 => Type::Int,
                    DataType::String => Type::String,
                    _ => Type::String, // fallback
                };
                (name.to_string(), Box::new(ty))
            })
            .collect();

        Ok(Type::Record(fields))
    }

    fn generate_module(&self, name: &str, ty: Type) -> Result<Module> {
        let mut module = Module::new(name);

        module.add_function("load", CsvLoadFunction(ty));

        Ok(module)
    }
}

struct CsvLoadFunction(Type);

impl RuntimeFunction for CsvLoadFunction {
    fn ty(&self) -> Type {
        Type::Func(
            Box::new(Type::String),
            Box::new(Type::List(Box::new(self.0.clone()))),
        )
    }

    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let path = match &args[0] {
            Value::String(s) => s.as_ref(),
            _ => unreachable!("Type checker ensures correct types"), // TODO: I don't like this
        };

        let path = PlPath::from_str(path);

        let mut lf = LazyCsvReader::new(path).finish()?;
        let schema = lf.collect_schema()?;

        Ok(Value::List(ListRepr::DataFrame { lf, schema }))
    }
}
