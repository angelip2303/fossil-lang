use crate::ast::thir::{Polytype, TypedHir};
use crate::error::{RuntimeError, TypeVar};

// TODO: Define Value type when runtime module is implemented
pub type Value = ();

pub trait FunctionImpl: Send + Sync {
    /// Returns the type signature of the function
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype;

    /// Execute the function with the given arguments at runtime
    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError>;
}
