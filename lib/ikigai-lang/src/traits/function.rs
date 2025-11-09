use crate::error::Result;
use crate::solver::TypeScheme;
use crate::value::Value;

pub trait RuntimeFunction: Send + Sync {
    fn ty(&self) -> Type;
    fn call(&self, arg: Vec<Value>) -> Result<Value>;
}
