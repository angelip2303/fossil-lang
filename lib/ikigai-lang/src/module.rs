use std::collections::HashMap;

use thiserror::Error;

use crate::context::{Interner, Symbol};
use crate::traits::function::FunctionImpl;
use crate::traits::provider::TypeProviderImpl;

#[derive(Error, Debug)]
pub enum RegistryError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Undefined module: {0}")]
    UndefinedModule(String),
}

pub struct Module {
    pub name: String,
    pub bindings: HashMap<String, Binding>,
}

pub enum Binding {
    // Value(Value<'a>),
    // Type(Type<'a>),
    Function(Box<dyn FunctionImpl>),
    Provider(Box<dyn TypeProviderImpl>),
}

pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
}

impl ModuleRegistry {
    pub fn resolve(&self, path: &[Symbol], interner: &Interner) -> Result<&Binding, RegistryError> {
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
            .ok_or_else(|| RegistryError::UndefinedVariable(format!("{}.{}", module_name, item)))
    }
}
