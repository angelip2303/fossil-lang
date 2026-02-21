use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::RecordField;
use crate::common::PrimitiveType;
use crate::context::{DefId, DefKind, Definitions, Interner, Symbol, TypeMetadata};
use crate::runtime::storage::StorageConfig;

use crate::traits::provider::{FileReader, LocalFileReader, ModuleSpec, ProviderInfo, TypeProviderImpl};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInFieldType {
    Required(PrimitiveType),
    Optional(PrimitiveType),
}

pub struct TypeInfo<'a> {
    pub name: Symbol,
    pub def_id: DefId,
    pub fields: &'a [RecordField],
    pub interner: &'a Interner,
}

pub type ModuleGeneratorFn = Arc<dyn Fn(&TypeInfo) -> Option<ModuleSpec> + Send + Sync>;

/// Hook for resolving the base URI of a type referenced by `ref Type(args)`.
/// Returns `Some(base_string)` if the type has a base — the evaluator prepends it to the ctor arg.
pub type RefResolverFn = Arc<dyn Fn(DefId, &GlobalContext) -> Option<String> + Send + Sync>;

#[derive(Clone)]
pub struct GlobalContext {
    pub interner: Interner,
    pub definitions: Definitions,
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    pub registered_types: HashMap<DefId, Vec<(Symbol, BuiltInFieldType)>>,
    pub module_generators: Vec<ModuleGeneratorFn>,
    pub ref_resolver: Option<RefResolverFn>,
    pub storage: StorageConfig,
    pub file_reader: Arc<dyn FileReader>,
}

impl GlobalContext {
    pub fn register_provider(&mut self, name: &str, provider: impl TypeProviderImpl + 'static) {
        let symbol = self.interner.intern(name);
        let provider = Arc::new(provider);
        let def_kind = DefKind::Provider(provider);
        self.definitions.insert(None, symbol, def_kind);
    }

    pub fn register_module(&mut self, name: &str, spec: ModuleSpec) {
        let sym = self.interner.intern(name);
        self.register_module_by_symbol(sym, spec);
    }

    pub fn register_module_by_symbol(&mut self, name: Symbol, spec: ModuleSpec) {
        let module_id = self
            .definitions
            .get_by_symbol(name)
            .map(|d| d.id())
            .unwrap_or_else(|| self.definitions.insert(None, name, DefKind::Mod));
        for func_def in spec.functions {
            let func_sym = self.interner.intern(&func_def.name);
            self.definitions.insert(
                Some(module_id),
                func_sym,
                DefKind::Func(func_def.implementation),
            );
        }
    }

    pub fn list_providers(&self) -> Vec<(String, ProviderInfo)> {
        self.definitions.iter()
            .filter_map(|def| match &def.kind {
                DefKind::Provider(p) => Some((self.interner.resolve(def.name).to_string(), p.info())),
                _ => None,
            })
            .collect()
    }

    pub fn register_record_type_with_optionality(
        &mut self,
        name: &str,
        fields: Vec<(&str, BuiltInFieldType)>,
    ) -> DefId {
        let symbol = self.interner.intern(name);
        let def_id = self.definitions.insert(None, symbol, DefKind::Type);

        let interned_fields: Vec<_> = fields
            .into_iter()
            .map(|(fname, ftype)| (self.interner.intern(fname), ftype))
            .collect();

        self.registered_types.insert(def_id, interned_fields);
        def_id
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            interner: Interner::default(),
            definitions: Definitions::default(),
            type_metadata: HashMap::new(),
            registered_types: HashMap::new(),
            module_generators: Vec::new(),
            ref_resolver: None,
            storage: StorageConfig::default(),
            file_reader: Arc::new(LocalFileReader),
        }
    }
}
