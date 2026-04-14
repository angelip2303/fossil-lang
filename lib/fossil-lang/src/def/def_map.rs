use std::collections::HashMap;

use crate::base::common::{Path, PrimitiveType};
use crate::db::{Db, DefId, DefKindTag, Symbol};
use crate::metadata::TypeMetadata;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInFieldType {
    Required(PrimitiveType),
    Optional(PrimitiveType),
}

pub type RegisteredTypes = HashMap<DefId, Vec<(Symbol, BuiltInFieldType)>>;
pub type TypeMetadataMap = HashMap<DefId, TypeMetadata>;

/// Three-namespace separation following rustc's `Namespace` enum
/// (`compiler/rustc_hir/src/def.rs`). `MetaNS` hosts catalog entries
/// (sources/sinks) so that a user `let csv = 1` and a catalog `csv!(…)`
/// can coexist without collision.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Namespace {
    TypeNS,
    ValueNS,
    MetaNS,
}

/// Per-namespace container, mirroring rustc's `PerNS<T>`
/// (`compiler/rustc_resolve/src/lib.rs`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PerNS<T> {
    pub type_ns: T,
    pub value_ns: T,
    pub meta_ns: T,
}

impl<T> PerNS<T> {
    pub fn get(&self, ns: Namespace) -> &T {
        match ns {
            Namespace::TypeNS => &self.type_ns,
            Namespace::ValueNS => &self.value_ns,
            Namespace::MetaNS => &self.meta_ns,
        }
    }

    pub fn get_mut(&mut self, ns: Namespace) -> &mut T {
        match ns {
            Namespace::TypeNS => &mut self.type_ns,
            Namespace::ValueNS => &mut self.value_ns,
            Namespace::MetaNS => &mut self.meta_ns,
        }
    }
}

impl DefKindTag {
    pub fn namespace(self) -> Namespace {
        match self {
            DefKindTag::Type => Namespace::TypeNS,
            DefKindTag::Let | DefKindTag::RecordConstructor => Namespace::ValueNS,
            DefKindTag::Mod => Namespace::TypeNS,
            DefKindTag::Catalog { .. } => Namespace::MetaNS,
        }
    }
}

#[derive(Default, Clone, PartialEq)]
pub struct DefMap {
    by_symbol: PerNS<HashMap<Symbol, Vec<DefId>>>,
    children: HashMap<DefId, PerNS<HashMap<Symbol, DefId>>>,
}

impl DefMap {
    /// Insert a definition into its natural namespace (derived from `kind`).
    pub fn insert(
        &mut self,
        db: &dyn Db,
        parent: Option<DefId>,
        name: Symbol,
        kind: DefKindTag,
    ) -> DefId {
        let ns = kind.namespace();
        let namespace = parent.map(|p| p.name(db));
        let def_id = DefId::new(db, namespace, name, kind);
        self.by_symbol
            .get_mut(ns)
            .entry(name)
            .or_default()
            .push(def_id);
        if let Some(parent_id) = parent {
            self.children
                .entry(parent_id)
                .or_default()
                .get_mut(ns)
                .insert(name, def_id);
        }
        def_id
    }

    /// Look up a symbol in a specific namespace. Returns the first matching DefId.
    pub fn get_in_ns(&self, name: Symbol, ns: Namespace) -> Option<DefId> {
        self.by_symbol
            .get(ns)
            .get(&name)
            .and_then(|ids| ids.first())
            .copied()
    }

    /// Find a symbol in a specific namespace matching a predicate over its kind.
    pub fn find_in_ns(
        &self,
        name: Symbol,
        ns: Namespace,
        db: &dyn Db,
        pred: impl Fn(DefKindTag) -> bool,
    ) -> Option<DefId> {
        self.by_symbol
            .get(ns)
            .get(&name)?
            .iter()
            .copied()
            .find(|def_id| pred(def_id.kind(db)))
    }

    /// Resolve a path in a specific namespace. The first segment of a
    /// qualified path is looked up in `ns` first, falling back to
    /// `TypeNS` for module / namespace tokens (e.g. `Rdf` in
    /// `Rdf.materialize` is a `DefKindTag::Mod` living in `TypeNS`
    /// regardless of whether the caller is resolving a value or a
    /// type). Mirrors rustc's behavior: module path prefixes always
    /// live in the type namespace, and only the tail segment is
    /// resolved in the requested namespace. Subsequent segments
    /// traverse children in `ValueNS` → `TypeNS` → `MetaNS` order, so
    /// `Rdf.materialize` (a sink registered as a `Let` child of the
    /// `Rdf` module) resolves cleanly in `ValueNS` context.
    pub fn resolve(&self, _db: &dyn Db, path: &Path, ns: Namespace) -> Option<DefId> {
        match path {
            Path::Simple(sym) => self.get_in_ns(*sym, ns),
            Path::Qualified(parts) if parts.is_empty() => None,
            Path::Qualified(parts) => {
                let mut current = self
                    .get_in_ns(parts[0], ns)
                    .or_else(|| {
                        if ns != Namespace::TypeNS {
                            self.get_in_ns(parts[0], Namespace::TypeNS)
                        } else {
                            None
                        }
                    })?;
                for &part in &parts[1..] {
                    let children = self.children.get(&current)?;
                    current = children
                        .get(Namespace::ValueNS)
                        .get(&part)
                        .or_else(|| children.get(Namespace::TypeNS).get(&part))
                        .or_else(|| children.get(Namespace::MetaNS).get(&part))
                        .copied()?;
                }
                Some(current)
            }
        }
    }

    /// All symbols across all namespaces — used for "did you mean" suggestion lists.
    pub fn all_symbols_in_ns(&self, ns: Namespace) -> impl Iterator<Item = Symbol> + '_ {
        self.by_symbol.get(ns).keys().copied()
    }

    /// Mutable access to the MetaNS bucket — used by `register_catalog_in_def_map`
    /// to merge catalog DefIds into a per-file DefMap.
    pub fn meta_ns_mut(&mut self) -> &mut HashMap<Symbol, Vec<DefId>> {
        self.by_symbol.get_mut(Namespace::MetaNS)
    }

    pub fn register_record_type(
        &mut self,
        db: &dyn Db,
        registered_types: &mut RegisteredTypes,
        name: &str,
        fields: Vec<(&str, BuiltInFieldType)>,
    ) -> DefId {
        let symbol = Symbol::new(db, name);
        let def_id = self.insert(db, None, symbol, DefKindTag::Type);
        let interned_fields: Vec<_> = fields
            .into_iter()
            .map(|(fname, ftype)| (Symbol::new(db, fname), ftype))
            .collect();
        registered_types.insert(def_id, interned_fields);
        def_id
    }
}
