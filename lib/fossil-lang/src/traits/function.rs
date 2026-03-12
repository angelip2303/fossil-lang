use std::collections::HashMap;
use std::sync::Arc;

use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ir, Polytype, TypeIndex, TypeVar};
use crate::passes::GlobalContext;
use crate::runtime::output::{LocalOutputResolver, OutputResolver};
use crate::runtime::value::Value;

pub struct RuntimeContext<'a> {
    pub gcx: &'a GlobalContext,
    pub ir: &'a Ir,
    pub type_index: &'a TypeIndex,
    pub current_type: Option<DefId>,
    pub output_resolver: Arc<dyn OutputResolver>,
    pub storage: Arc<HashMap<String, String>>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(gcx: &'a GlobalContext, ir: &'a Ir, type_index: &'a TypeIndex) -> Self {
        Self {
            gcx,
            ir,
            type_index,
            current_type: None,
            output_resolver: Arc::new(LocalOutputResolver),
            storage: Arc::new(HashMap::new()),
        }
    }

    pub fn with_type(mut self, type_def_id: DefId) -> Self {
        self.current_type = Some(type_def_id);
        self
    }

    pub fn with_output_resolver(mut self, resolver: Arc<dyn OutputResolver>) -> Self {
        self.output_resolver = resolver;
        self
    }

    pub fn with_storage(mut self, storage: Arc<HashMap<String, String>>) -> Self {
        self.storage = storage;
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
