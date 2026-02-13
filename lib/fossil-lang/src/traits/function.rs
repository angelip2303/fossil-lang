use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ir, Polytype, TypeIndex, TypeVar};
use crate::passes::GlobalContext;
use crate::runtime::value::Value;

pub struct RuntimeContext<'a> {
    pub gcx: &'a GlobalContext,
    pub ir: &'a Ir,
    pub type_index: &'a TypeIndex,
    pub current_type: Option<DefId>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(gcx: &'a GlobalContext, ir: &'a Ir, type_index: &'a TypeIndex) -> Self {
        Self {
            gcx,
            ir,
            type_index,
            current_type: None,
        }
    }

    pub fn with_type(mut self, type_def_id: DefId) -> Self {
        self.current_type = Some(type_def_id);
        self
    }
}

pub trait FunctionImpl: Send + Sync {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype;

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError>;
}
