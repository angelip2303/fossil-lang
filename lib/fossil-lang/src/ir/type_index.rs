use std::collections::HashMap;

use crate::context::{DefId, Symbol};
use crate::ir::TypeId;

pub struct TypeDeclInfo {
    pub ty: TypeId,
    pub ctor_param_count: usize,
    pub ctor_param_names: Vec<Symbol>,
    pub field_names: Vec<Symbol>,
}

#[derive(Default)]
pub struct TypeIndex {
    entries: HashMap<DefId, TypeDeclInfo>,
}

impl TypeIndex {
    pub fn insert(&mut self, def_id: DefId, info: TypeDeclInfo) {
        self.entries.insert(def_id, info);
    }

    pub fn get(&self, def_id: DefId) -> Option<&TypeDeclInfo> {
        self.entries.get(&def_id)
    }

    pub fn contains(&self, def_id: DefId) -> bool {
        self.entries.contains_key(&def_id)
    }

    pub fn keys(&self) -> impl Iterator<Item = &DefId> {
        self.entries.keys()
    }
}
