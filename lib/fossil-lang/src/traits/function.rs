use std::sync::{Arc, Mutex};

use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ir, Polytype, TypeIndex, TypeVar};
use crate::passes::GlobalContext;
use crate::runtime::executor::{OutputKind, OutputRecord};
use crate::runtime::value::Value;

pub struct RuntimeContext<'a> {
    pub gcx: &'a GlobalContext,
    pub ir: &'a Ir,
    pub type_index: &'a TypeIndex,
    pub current_type: Option<DefId>,
    outputs: Arc<Mutex<Vec<OutputRecord>>>,
}

impl<'a> RuntimeContext<'a> {
    pub fn new(gcx: &'a GlobalContext, ir: &'a Ir, type_index: &'a TypeIndex) -> Self {
        Self {
            gcx,
            ir,
            type_index,
            current_type: None,
            outputs: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn with_type(mut self, type_def_id: DefId) -> Self {
        self.current_type = Some(type_def_id);
        self
    }

    pub fn with_outputs(mut self, outputs: Arc<Mutex<Vec<OutputRecord>>>) -> Self {
        self.outputs = outputs;
        self
    }

    /// Register an output produced during execution.
    pub fn register_output(&self, kind: OutputKind, path: String) {
        self.outputs
            .lock()
            .expect("outputs lock poisoned")
            .push(OutputRecord { kind, path });
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
