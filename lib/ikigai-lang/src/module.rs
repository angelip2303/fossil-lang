use std::collections::HashMap;

use thiserror::Error;

use crate::context::*;
use crate::traits::function::FunctionImpl;
use crate::traits::provider::TypeProviderImpl;

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined module: {0}")]
    UndefinedModule(String),
}

pub type BindingId = NodeId<Binding>;

pub enum Binding {
    Function(Box<dyn FunctionImpl>),
    Provider(Box<dyn TypeProviderImpl>),
}

pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
    bindings: Arena<Binding>, // Añadir arena para bindings
}

impl ModuleRegistry {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            bindings: Arena::default(),
        }
    }

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

    pub fn get(&self, id: BindingId) -> &Binding {
        self.bindings.get(id)
    }
}

pub struct Module {
    pub name: String,
    pub bindings: HashMap<String, BindingId>,
}
