use crate::error::Result;
use crate::solver::TypeScheme;
use crate::value::Value;

pub trait RuntimeFunction: Send + Sync {
    fn type_scheme(&self) -> TypeScheme;
    fn call(&self, arg: Vec<Value>) -> Result<Value>;
}
