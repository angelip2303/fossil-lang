use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::RecordField;
use crate::common::PrimitiveType;
use crate::context::{DefId, DefKind, Definitions, Interner, Symbol, TypeMetadata};
use crate::traits::provider::{ProviderInfo, TypeProviderImpl};
use crate::traits::resolver::{DefaultPathResolver, PathResolver};

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

/// Global compilation context.
#[derive(Clone)]
pub struct GlobalContext {
    pub interner: Interner,
    pub definitions: Definitions,
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    pub registered_types: HashMap<DefId, Vec<(Symbol, BuiltInFieldType)>>,
    pub path_resolver: Arc<dyn PathResolver>,
}

impl GlobalContext {
    pub fn register_provider(&mut self, name: &str, provider: impl TypeProviderImpl + 'static) {
        let symbol = self.interner.intern(name);
        let provider = Arc::new(provider);
        let def_kind = DefKind::Provider(provider);
        self.definitions.insert(None, symbol, def_kind);
    }

    pub fn list_providers(&self) -> Vec<(String, ProviderInfo)> {
        self.definitions.iter()
            .filter_map(|def| match &def.kind {
                DefKind::Provider(p) => Some((self.interner.resolve(def.name).to_string(), p.info())),
                _ => None,
            })
            .collect()
    }

    pub fn provider_for_extension(&self, ext: &str) -> Option<&dyn TypeProviderImpl> {
        self.definitions.iter().find_map(|def| match &def.kind {
            DefKind::Provider(p) if p.info().extensions.contains(&ext) => Some(p.as_ref()),
            _ => None,
        })
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
            path_resolver: Arc::new(DefaultPathResolver),
        }
    }
}
