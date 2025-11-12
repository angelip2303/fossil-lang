use std::collections::HashMap;
use std::sync::Arc;

use crate::context::*;
use crate::error::RegistryError;
use crate::traits::function::FunctionImpl;
use crate::traits::provider::TypeProviderImpl;

pub type BindingId = NodeId<Binding>;

#[derive(Clone)]
pub enum Binding {
    Function(Arc<dyn FunctionImpl>),
    Provider(Arc<dyn TypeProviderImpl>),
}

#[derive(Default, Clone)]
pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
    bindings: Arena<Binding>,
}

impl ModuleRegistry {
    pub fn resolve(
        &self,
        path: &[Symbol],
        interner: &Interner,
    ) -> Result<BindingId, RegistryError> {
        let module_parts: Vec<_> = path
            .iter()
            .take(path.len() - 1)
            .map(|s| interner.resolve(*s))
            .collect();

        let module_name = module_parts.join(".");

        let module = self
            .modules
            .get(&module_name)
            .ok_or_else(|| RegistryError::UndefinedModule(module_name.clone()))?;

        let item = interner.resolve(*path.last().unwrap());

        module
            .bindings
            .get(item)
            .copied()
            .ok_or_else(|| RegistryError::UndefinedVariable(format!("{}.{}", module_name, item)))
    }

    pub fn alloc(&mut self, binding: Binding) -> BindingId {
        self.bindings.alloc(binding)
    }

    pub fn get(&self, id: BindingId) -> &Binding {
        self.bindings.get(id)
    }
}

#[derive(Default, Clone)]
pub struct Module {
    pub name: String,
    pub bindings: HashMap<String, BindingId>,
}
