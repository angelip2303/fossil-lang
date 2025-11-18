use crate::ast::{Ast, Polytype};
use crate::error::RuntimeError;
use crate::phases::typecheck::TypeVarGen;
use crate::runtime::value::Value;

pub trait FunctionImpl: Send + Sync {
    /// Returns the type signature of the function
    fn signature(&self, ast: &mut Ast, tvg: &mut TypeVarGen) -> Polytype;

    /// Execute the function with the given arguments at runtime
    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError>;
}
