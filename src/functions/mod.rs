use crate::error::Result;
use crate::solver::Type;

pub mod random;
pub mod registry;

#[derive(Clone, Debug)]
pub enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    String(String),
}

pub trait Function {
    fn name(&self) -> &str;
    fn signature(&self) -> Type;
    fn execute(&self, args: Vec<Value>) -> Result<Value>;
}
