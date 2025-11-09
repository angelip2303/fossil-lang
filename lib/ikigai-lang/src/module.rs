use std::collections::HashMap;

use crate::traits::provider::TypeProviderImpl;

#[derive(Default)]
pub struct Module<'a> {
    pub name: &'a str,
    pub bindings: HashMap<&'a str, Binding>,
}

pub enum Binding {
    // Value(Value<'a>),
    // Type(Type<'a>),
    // Function(Function<'a>),
    Provider(Box<dyn TypeProviderImpl>),
}

#[derive(Default)]
pub struct ModuleRegistry<'a> {
    modules: HashMap<&'a str, Module<'a>>,
}

impl<'a> ModuleRegistry<'a> {
    pub fn resolve(&'a self, path: &[&'a str]) -> Option<&Binding> {
        let module = path[..path.len() - 1].join(".");
        let binding = path.last()?;

        self.modules
            .get(module.as_str())
            .and_then(|module| module.bindings.get(binding))
    }
}
