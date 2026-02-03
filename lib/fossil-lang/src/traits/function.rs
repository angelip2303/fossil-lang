use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ir, Polytype, TypeVar};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;

pub struct RuntimeContext<'a> {
    pub gcx: &'a GlobalContext,
    pub ir: &'a Ir,
    pub current_type: Option<DefId>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(gcx: &'a GlobalContext, ir: &'a Ir) -> Self {
        Self {
            gcx,
            ir,
            current_type: None,
        }
    }

    pub fn with_type(mut self, type_def_id: DefId) -> Self {
        self.current_type = Some(type_def_id);
        self
    }
}

pub trait FunctionImpl: Send + Sync {
    /// Returns the type signature of the function
    ///
    /// # Arguments
    /// * `ir` - Mutable access to IR for allocating types
    /// * `next_type_var` - Function to generate fresh type variables
    /// * `gcx` - Global context with definitions and type metadata
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype;

    /// Execute the function with the given arguments at runtime
    ///
    /// **Breaking change from previous version**: Now requires RuntimeContext
    /// to provide access to compile-time information (types, metadata) during
    /// runtime execution.
    ///
    /// # Arguments
    /// * `args` - Function arguments as runtime values
    /// * `ctx` - Runtime context with access to GlobalContext and IR
    ///
    /// # Example
    /// ```ignore
    /// fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, CompileError> {
    ///     // Access type metadata from compile-time
    ///     if let Some(type_id) = ctx.current_type {
    ///         let metadata = ctx.gcx.type_metadata.get(&type_id);
    ///         // Use metadata...
    ///     }
    ///     // Process arguments...
    /// }
    /// ```
    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError>;
}
