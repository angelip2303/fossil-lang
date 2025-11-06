use polars::prelude::*;

use crate::error::Result;
use crate::solver::Type;

use super::{RuntimeFunction, Value};

pub struct RandomNextFunction;

impl RuntimeFunction for RandomNextFunction {
    fn ty(&self) -> Type {
        Type::Func(
            Box::new(Type::Int),
            Box::new(Type::Func(Box::new(Type::Int), Box::new(Type::Int))),
        )
    }

    fn call(&self, args: Vec<Value>) -> Result<Value> {
        match (&args[0], &args[1]) {
            (Value::Int(min), Value::Int(max)) => {
                use rand::Rng;
                Ok(Value::Int(rand::rng().random_range(*min..=*max)))
            }
            _ => unreachable!("Type checker ensures correct types"), // TODO: I don't like this
        }
    }
}

pub struct CsvWriteFunction;

impl RuntimeFunction for CsvWriteFunction {
    fn ty(&self) -> Type {
        use crate::solver::TypeVar;
        let a = Type::Var(TypeVar(0)); // TODO: maybe we should enforce Record?

        Type::Func(
            Box::new(Type::List(Box::new(a))),
            Box::new(Type::Func(Box::new(Type::String), Box::new(Type::Unit))),
        )
    }

    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let data = &args[0];
        let path = match &args[1] {
            Value::String(s) => s.as_ref(),
            _ => unreachable!(),
        };

        let lf = data.as_lazyframe()?;
        let mut df = lf.collect()?;

        let file = std::fs::File::create(path)?;
        CsvWriter::new(file).finish(&mut df)?;

        Ok(Value::Unit)
    }
}
