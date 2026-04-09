use std::collections::HashMap;

use crate::common::{Path, PrimitiveType};
use crate::context::{DefId, DefKindTag, Symbol, TypeMetadata};
use crate::db::Db;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInFieldType {
    Required(PrimitiveType),
    Optional(PrimitiveType),
}

/// Auxiliary index: tracks DefIds by symbol and parent→child relationships.
///
/// DefId creation goes through `DefMap::insert(db, ...)` which calls
/// `DefId::new(db, ...)` (Salsa interning) and updates the local indices.
#[derive(Default, Clone, PartialEq)]
pub struct DefMap {
    by_symbol: HashMap<Symbol, Vec<DefId>>,
    children: HashMap<DefId, HashMap<Symbol, DefId>>,
}

impl DefMap {
    /// Create or look up a definition, updating the auxiliary indices.
    pub fn insert(
        &mut self,
        db: &dyn Db,
        parent: Option<DefId>,
        name: Symbol,
        kind: DefKindTag,
    ) -> DefId {
        let namespace = parent.and_then(|p| Some(p.name(db)));
        let def_id = DefId::new(db, namespace, name, kind);
        self.by_symbol.entry(name).or_default().push(def_id);
        if let Some(parent_id) = parent {
            self.children.entry(parent_id).or_default().insert(name, def_id);
        }
        def_id
    }

    pub fn get_by_symbol(&self, name: Symbol) -> Option<DefId> {
        self.by_symbol
            .get(&name)
            .and_then(|ids| ids.first())
            .copied()
    }

    pub fn find_by_symbol(&self, name: Symbol, db: &dyn Db, pred: impl Fn(DefKindTag) -> bool) -> Option<DefId> {
        self.by_symbol
            .get(&name)?
            .iter()
            .copied()
            .find(|def_id| pred(def_id.kind(db)))
    }

    pub fn resolve(&self, _db: &dyn Db, path: &Path) -> Option<DefId> {
        match path {
            Path::Simple(sym) => self.get_by_symbol(*sym),
            Path::Qualified(parts) if parts.is_empty() => None,
            Path::Qualified(parts) => {
                let current_id = self.get_by_symbol(parts[0])?;
                let mut current = current_id;
                for &part in &parts[1..] {
                    current = *self.children.get(&current)?.get(&part)?;
                }
                Some(current)
            }
        }
    }
}

/// Global compilation context.
///
/// NOTE: This is being decomposed into Salsa queries (DefMap, SchemaMap, etc.).
/// The interner has been moved to a global static (Symbol::intern / Symbol::as_str).
#[derive(Clone, PartialEq)]
pub struct GlobalContext {
    pub definitions: DefMap,
    pub type_metadata: HashMap<DefId, TypeMetadata>,
    pub registered_types: HashMap<DefId, Vec<(Symbol, BuiltInFieldType)>>,
}

impl GlobalContext {
    pub fn register_record_type_with_optionality(
        &mut self,
        db: &dyn Db,
        name: &str,
        fields: Vec<(&str, BuiltInFieldType)>,
    ) -> DefId {
        let symbol = Symbol::intern(name);
        let def_id = self.definitions.insert(db, None, symbol, DefKindTag::Type);
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
            definitions: DefMap::default(),
            type_metadata: HashMap::new(),
            registered_types: HashMap::new(),
        }
    }
}
