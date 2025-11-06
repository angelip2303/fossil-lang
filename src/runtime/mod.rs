pub mod builtin;
pub mod value;

pub use value::Value;

use crate::error::Result;
use crate::solver::TypeScheme;

pub trait RuntimeFunction: Send + Sync {
    fn type_scheme(&self) -> TypeScheme;
    fn call(&self, args: Vec<Value>) -> Result<Value>;
}
