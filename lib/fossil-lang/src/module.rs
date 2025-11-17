use std::collections::HashMap;
use std::sync::Arc;

use crate::context::*;
use crate::error::ResolveError;
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
    /// Create a new module builder for adding bindings
    pub fn module<'a>(&'a mut self, name: impl Into<String>) -> ModuleBuilder<'a> {
        ModuleBuilder {
            registry: self,
            module_name: name.into(),
        }
    }

    /// Get or create a module by name
    fn get_or_create_module(&mut self, name: &str) -> &mut Module {
        self.modules
            .entry(name.to_string())
            .or_insert_with(|| Module {
                name: name.to_string(),
                bindings: HashMap::new(),
            })
    }

    /// Register a binding in a specific module
    pub fn register(&mut self, module_path: &str, item_name: &str, binding: Binding) -> BindingId {
        let binding_id = self.bindings.alloc(binding);
        let module = self.get_or_create_module(module_path);
        module.bindings.insert(item_name.to_string(), binding_id);
        binding_id
    }

    pub fn resolve(&self, path: &[Symbol], interner: &Interner) -> Result<BindingId, ResolveError> {
        let module_parts: Vec<_> = path
            .iter()
            .take(path.len() - 1)
            .map(|s| interner.resolve(*s))
            .collect();

        let module_name = module_parts.join(".");

        let module = self
            .modules
            .get(&module_name)
            .ok_or_else(|| ResolveError::UndefinedModule(module_name.clone()))?;

        let item = interner.resolve(*path.last().unwrap());

        module
            .bindings
            .get(item)
            .copied()
            .ok_or_else(|| ResolveError::UndefinedVariable(format!("{}.{}", module_name, item)))
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

/// Builder pattern for adding bindings to a module
pub struct ModuleBuilder<'a> {
    registry: &'a mut ModuleRegistry,
    module_name: String,
}

impl<'a> ModuleBuilder<'a> {
    pub fn provider(self, name: &str, provider: Arc<dyn TypeProviderImpl>) -> Self {
        self.registry
            .register(&self.module_name, name, Binding::Provider(provider));
        self
    }

    pub fn function(self, name: &str, func: Arc<dyn FunctionImpl>) -> Self {
        self.registry
            .register(&self.module_name, name, Binding::Function(func));
        self
    }

    pub fn done(self) -> &'a mut ModuleRegistry {
        self.registry
    }
}
