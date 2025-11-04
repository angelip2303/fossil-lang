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
