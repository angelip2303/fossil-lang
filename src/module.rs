use std::collections::HashMap;
use std::sync::Arc;

use crate::{providers::TypeProvider, runtime::RuntimeFunction};

#[derive(Clone)]
pub struct Module {
    pub name: String,
    pub functions: HashMap<String, Arc<dyn RuntimeFunction>>,
    pub providers: HashMap<String, Arc<dyn TypeProvider>>,
    pub submodules: HashMap<String, Module>,
}

impl Module {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: HashMap::new(),
            providers: HashMap::new(),
            submodules: HashMap::new(),
        }
    }

    pub fn add_function(&mut self, func: impl RuntimeFunction + 'static) {
        let name = func.name().split('.').last().unwrap().to_string();
        self.functions.insert(name, Arc::new(func));
    }

    pub fn add_provider(&mut self, name: impl Into<String>, provider: impl TypeProvider + 'static) {
        self.providers.insert(name.into(), Arc::new(provider));
    }

    pub fn add_submodule(&mut self, module: Module) {
        self.submodules.insert(module.name.clone(), module);
    }
}

#[derive(Clone, Default)]
pub struct ModuleRegistry {
    modules: HashMap<String, Module>,
}

impl ModuleRegistry {
    pub fn new() -> Self {
        let mut registry = Self::default();

        registry.register(Self::random_module());
        registry.register(Self::data_module());

        registry
    }

    pub fn register(&mut self, module: Module) {
        self.modules.insert(module.name.clone(), module);
    }

    pub fn get(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    pub fn resolve(&self, segments: &[&str]) -> Option<Lookup> {
        match segments {
            [] => None,
            [name] => self.modules.get(*name).map(|m| Lookup::Module(m.clone())),
            [first, rest @ ..] => {
                let module = self.modules.get(*first)?;
                self.resolve_in_module(module, rest)
            }
        }
    }

    fn resolve_in_module(&self, module: &Module, path: &[&str]) -> Option<Lookup> {
        match path {
            [] => Some(Lookup::Module(module.clone())),
            [name] => {
                if let Some(func) = module.functions.get(*name) {
                    return Some(Lookup::Function(func.clone()));
                }
                if let Some(prov) = module.providers.get(*name) {
                    return Some(Lookup::Provider(prov.clone()));
                }
                module
                    .submodules
                    .get(*name)
                    .map(|m| Lookup::Module(m.clone()))
            }
            [first, rest @ ..] => {
                let submod = module.submodules.get(*first)?;
                self.resolve_in_module(submod, rest)
            }
        }
    }

    fn random_module() -> Module {
        use crate::runtime::builtin::*;

        let mut module = Module::new("Random");
        module.add_function(RandomNextFunction);
        module
    }

    fn data_module() -> Module {
        let mut data = Module::new("Data");
        data.add_submodule(Self::csv_module());
        data
    }

    fn csv_module() -> Module {
        use crate::providers::csv::CsvProvider;
        use crate::runtime::builtin::CsvWriteFunction;

        let mut csv = Module::new("Csv");
        csv.add_provider("CsvProvider", CsvProvider);
        csv.add_function(CsvWriteFunction);
        csv
    }
}

#[derive(Clone)]
pub enum Lookup {
    Module(Module),
    Function(Arc<dyn RuntimeFunction>),
    Provider(Arc<dyn TypeProvider>),
}
