pub mod builtin;
pub mod value;

pub use value::Value;

use crate::error::Result;
use crate::solver::Type;

pub trait RuntimeFunction: Send + Sync {
    fn ty(&self) -> Type;
    fn call(&self, args: Vec<Value>) -> Result<Value>;
}
