use super::{RuntimeFunction, Value};
use crate::error::Result;
use crate::solver::{Type, TypeVar};

pub struct RandomInt;

impl RuntimeFunction for RandomInt {
    fn name(&self) -> &str {
        "Random.int"
    }

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
            _ => unreachable!("Type checker ensures correct types"),
        }
    }
}

pub struct CsvWrite;

impl RuntimeFunction for CsvWrite {
    fn name(&self) -> &str {
        "Data.Csv.write"
    }

    fn ty(&self) -> Type {
        Type::Func(
            Box::new(Type::List(Box::new(Type::Var(TypeVar(0))))),
            Box::new(Type::Func(Box::new(Type::String), Box::new(Type::Unit))),
        )
    }

    fn call(&self, args: Vec<Value>) -> Result<Value> {
        use std::fs::File;

        match (&args[0], &args[1]) {
            (Value::DataFrame(lf), Value::String(path)) => {
                use polars::prelude::*;
                let mut file = File::create(path).map_err(|e| todo!("failed to create file"))?;
                let mut df = lf
                    .clone()
                    .collect()
                    .map_err(|e| todo!("failed to collect DataFrame"))?;

                CsvWriter::new(&mut file)
                    .finish(&mut df)
                    .map_err(|e| todo!("failed to write CSV"))?;

                Ok(Value::Unit)
            }
            _ => unreachable!(),
        }
    }
}
