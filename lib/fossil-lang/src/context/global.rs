use std::collections::HashMap;

use crate::common::PrimitiveType;
use crate::context::{DefId, DefKind, Definitions, Symbol, TypeMetadata};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInFieldType {
    Required(PrimitiveType),
    Optional(PrimitiveType),
}

/// Global compilation context.
///
/// NOTE: This is being decomposed into Salsa queries (DefMap, SchemaMap, etc.).
/// The interner has been moved to a global static (Symbol::intern / Symbol::as_str).
#[derive(Clone, PartialEq)]
pub struct GlobalContext {
    pub definitions: Definitions,
    pub type_metadata: HashMap<DefId, TypeMetadata>,
    pub registered_types: HashMap<DefId, Vec<(Symbol, BuiltInFieldType)>>,
}

impl GlobalContext {
    pub fn register_record_type_with_optionality(
        &mut self,
        name: &str,
        fields: Vec<(&str, BuiltInFieldType)>,
    ) -> DefId {
        let symbol = Symbol::intern(name);
        let def_id = self.definitions.insert(None, symbol, DefKind::Type);
        let interned_fields: Vec<_> = fields
            .into_iter()
            .map(|(fname, ftype)| (Symbol::intern(fname), ftype))
            .collect();
        self.registered_types.insert(def_id, interned_fields);
        def_id
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            definitions: Definitions::default(),
            type_metadata: HashMap::new(),
            registered_types: HashMap::new(),
        }
    }
}
