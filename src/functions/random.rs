use crate::error::Result;
use crate::solver::Type;

use super::*;

pub struct Int;

impl Function for Int {
    fn name(&self) -> &str {
        "Random.int"
    }

    fn signature(&self) -> Type {
        Type::Func(
            Box::new(Type::Int),
            Box::new(Type::Func(Box::new(Type::Int), Box::new(Type::Int))),
        )
    }

    fn execute(&self, args: Vec<Value>) -> Result<Value> {
        let (Value::Int(a), Value::Int(b)) = (&args[0], &args[1]) else {
            unreachable!("Type checker bug")
        };
        use rand::Rng;
        Ok(Value::Int(rand::rng().random_range(*a..=*b)))
    }
}
