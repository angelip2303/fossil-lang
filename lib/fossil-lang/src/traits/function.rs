use crate::ast::thir::{Polytype, TypeVar, TypedHir};
use crate::context::DefId;
use crate::error::RuntimeError;
use crate::passes::GlobalContext;
use crate::runtime::value::Value;

/// Runtime context available to functions during execution
///
/// Provides access to compile-time information (types, metadata) during
/// runtime function execution. This allows builtin functions to capture
/// and use type attributes, metadata, and other compile-time information.
pub struct RuntimeContext<'a> {
    /// Global context with interner, definitions, and type metadata
    pub gcx: &'a GlobalContext,

    /// Typed HIR with complete type information
    pub thir: &'a TypedHir,

    /// DefId of the type currently being constructed (if applicable)
    /// Used by constructor functions to access type metadata
    pub current_type: Option<DefId>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(gcx: &'a GlobalContext, thir: &'a TypedHir) -> Self {
        Self {
            gcx,
            thir,
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
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
    ) -> Polytype;

    /// Execute the function with the given arguments at runtime
    ///
    /// **Breaking change from previous version**: Now requires RuntimeContext
    /// to provide access to compile-time information (types, metadata) during
    /// runtime execution.
    ///
    /// # Arguments
    /// * `args` - Function arguments as runtime values
    /// * `ctx` - Runtime context with access to GlobalContext and TypedHir
    ///
    /// # Example
    /// ```ignore
    /// fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
    ///     // Access type metadata from compile-time
    ///     if let Some(type_id) = ctx.current_type {
    ///         let metadata = ctx.gcx.type_metadata.get(&type_id);
    ///         // Use metadata...
    ///     }
    ///     // Process arguments...
    /// }
    /// ```
    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError>;
}
