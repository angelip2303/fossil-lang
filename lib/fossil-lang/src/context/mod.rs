use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::Arc;

use crate::common::Path;
use crate::traits::function::{FunctionEffect, FunctionImpl};
use crate::traits::provider::TypeProviderImpl;

pub mod global;
pub mod metadata;

pub use self::global::*;
pub use self::metadata::*;

// ── Arena + NodeId: thin wrappers over la-arena ──────────────────────
//
// Same public API as before. Internally uses la-arena (rust-analyzer).
// NodeId<T> wraps la_arena::Idx<T>; Arena<T> wraps la_arena::Arena<T>.

pub struct NodeId<T>(la_arena::Idx<T>);

impl<T> NodeId<T> {
    pub fn new(idx: usize) -> Self {
        Self(la_arena::Idx::from_raw(la_arena::RawIdx::from(idx as u32)))
    }
    pub fn idx(&self) -> usize {
        self.0.into_raw().into_u32() as usize
    }
    pub fn raw(self) -> la_arena::Idx<T> {
        self.0
    }
}

impl<T> Clone for NodeId<T> { fn clone(&self) -> Self { *self } }
impl<T> Copy for NodeId<T> {}
impl<T> PartialEq for NodeId<T> { fn eq(&self, other: &Self) -> bool { self.0 == other.0 } }
impl<T> Eq for NodeId<T> {}
impl<T> Hash for NodeId<T> { fn hash<H: Hasher>(&self, state: &mut H) { self.0.hash(state); } }
impl<T> Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeId({})", self.idx())
    }
}

pub struct Arena<T>(la_arena::Arena<T>);

impl<T: Clone> Clone for Arena<T> { fn clone(&self) -> Self { Self(self.0.clone()) } }
impl<T> Default for Arena<T> { fn default() -> Self { Self(la_arena::Arena::new()) } }
impl<T: Debug> Debug for Arena<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Arena").field("len", &self.0.len()).finish()
    }
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, item: T) -> NodeId<T> {
        NodeId(self.0.alloc(item))
    }
    pub fn get(&self, id: NodeId<T>) -> &T {
        &self.0[id.0]
    }
    pub fn get_mut(&mut self, id: NodeId<T>) -> &mut T {
        &mut self.0[id.0]
    }
    pub fn iter(&self) -> impl Iterator<Item = (NodeId<T>, &T)> {
        self.0.iter().map(|(idx, item)| (NodeId(idx), item))
    }
}

impl<T> IntoIterator for Arena<T> {
    type Item = (NodeId<T>, T);
    type IntoIter = std::iter::Map<
        la_arena::IntoIter<T>,
        fn((la_arena::Idx<T>, T)) -> (NodeId<T>, T),
    >;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().map(|(idx, item)| (NodeId(idx), item))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    /// Create a synthetic symbol for error messages
    /// This should only be used in error handling where we don't have access to an interner
    pub const fn synthetic() -> Self {
        Symbol(0)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interner {
    map: HashMap<String, Symbol>,
    strings: Vec<String>,
}

impl Interner {
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.map.get(s) {
            return sym;
        }

        let sym = Symbol(self.strings.len() as u32);
        self.strings.push(s.to_string());
        self.map.insert(s.to_string(), sym);
        sym
    }

    /// Lookup a symbol without creating it if not found
    /// Returns None if the string hasn't been interned
    pub fn lookup(&self, s: &str) -> Option<Symbol> {
        self.map.get(s).copied()
    }

    pub fn resolve(&self, sym: Symbol) -> &str {
        debug_assert!((sym.0 as usize) < self.strings.len(), "Interner::resolve out of bounds: symbol {} but len {}", sym.0, self.strings.len());
        &self.strings[sym.0 as usize]
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(u32);

impl DefId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone)]
pub struct Def {
    id: DefId,
    parent: Option<DefId>,
    pub name: Symbol,
    pub kind: DefKind,
}

#[derive(Clone)]
pub enum DefKind {
    Mod,
    Let,
    Type,
    Func(Arc<dyn FunctionImpl>),
    RecordConstructor,
    Provider(Arc<dyn TypeProviderImpl>),
}

impl Def {
    pub fn new(id: DefId, parent: Option<DefId>, name: Symbol, kind: DefKind) -> Self {
        Self {
            id,
            parent,
            name,
            kind,
        }
    }

    pub fn id(&self) -> DefId {
        self.id
    }

    pub fn parent(&self) -> Option<DefId> {
        self.parent
    }
}

#[derive(Default, Clone)]
pub struct Definitions {
    items: Vec<Def>,
    by_symbol: HashMap<Symbol, Vec<DefId>>,
    children: HashMap<DefId, HashMap<Symbol, DefId>>,
}

impl Definitions {
    pub fn get(&self, id: DefId) -> &Def {
        &self.items[id.index() as usize]
    }

    pub fn get_by_symbol(&self, name: Symbol) -> Option<&Def> {
        self.by_symbol
            .get(&name)
            .and_then(|ids| ids.first())
            .map(|id| self.get(*id))
    }

    pub fn find_by_symbol(&self, name: Symbol, pred: impl Fn(&DefKind) -> bool) -> Option<&Def> {
        self.by_symbol
            .get(&name)?
            .iter()
            .map(|id| self.get(*id))
            .find(|def| pred(&def.kind))
    }

    pub fn insert(&mut self, parent: Option<DefId>, name: Symbol, kind: DefKind) -> DefId {
        let id = DefId::new(self.items.len() as u32);
        self.items.push(Def::new(id, parent, name, kind));
        self.by_symbol.entry(name).or_default().push(id);
        if let Some(parent_id) = parent {
            self.children.entry(parent_id).or_default().insert(name, id);
        }
        id
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn resolve(&self, path: &Path) -> Option<DefId> {
        match path {
            Path::Simple(sym) => self.get_by_symbol(*sym).map(|def| def.id()),
            Path::Qualified(parts) if parts.is_empty() => None,
            Path::Qualified(parts) => {
                let mut current_id = self.get_by_symbol(parts[0]).map(|def| def.id())?;
                for &part in &parts[1..] {
                    current_id = *self.children.get(&current_id)?.get(&part)?;
                }
                Some(current_id)
            }
        }
    }

    pub fn function_effects(&self, id: DefId) -> &[FunctionEffect] {
        match &self.get(id).kind {
            DefKind::Func(f) => f.effects(),
            _ => &[],
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Def> {
        self.items.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::Loc;
    use crate::error::FossilError;
    use crate::ir::{Ir, Polytype, TypeVar};
    use crate::runtime::value::Value;
    use crate::traits::function::{FunctionImpl, RuntimeContext};
    use crate::traits::provider::{
        FunctionDef, ModuleSpec, ProviderArgs, ProviderInfo, ProviderKind, ProviderOutput,
        ProviderSchema, TypeProviderImpl,
    };

    struct MockFunction;

    impl FunctionImpl for MockFunction {
        fn signature(
            &self,
            ir: &mut Ir,
            _next: &mut dyn FnMut() -> TypeVar,
            _gcx: &GlobalContext,
        ) -> Polytype {
            let ty = ir.unit_type();
            Polytype::mono(ty)
        }

        fn call(&self, _args: Vec<Value>, _type_args: &[DefId], _ctx: &RuntimeContext) -> Result<Value, FossilError> {
            Ok(Value::Unit)
        }
    }

    struct MockProvider;

    impl TypeProviderImpl for MockProvider {
        fn info(&self) -> ProviderInfo {
            ProviderInfo { extensions: vec![], kind: ProviderKind::Schema }
        }

        fn provide(
            &self,
            _args: &ProviderArgs,
            _ctx: &mut crate::traits::provider::ProviderContext,
            _type_name: &str,
            _loc: Loc,
        ) -> Result<ProviderOutput, FossilError> {
            Ok(ProviderOutput::new(ProviderSchema { fields: vec![] }))
        }
    }

    #[test]
    fn interner_intern_and_resolve() {
        let mut interner = Interner::default();
        let sym = interner.intern("hello");
        assert_eq!(interner.resolve(sym), "hello");
    }

    #[test]
    fn arena_multiple_items() {
        let mut arena: Arena<i32> = Arena::default();
        let id0 = arena.alloc(10);
        let id1 = arena.alloc(20);
        let id2 = arena.alloc(30);
        assert_eq!(*arena.get(id0), 10);
        assert_eq!(*arena.get(id1), 20);
        assert_eq!(*arena.get(id2), 30);
        assert_eq!(id0.idx(), 0);
        assert_eq!(id1.idx(), 1);
        assert_eq!(id2.idx(), 2);
    }

    #[test]
    fn arena_get_mut() {
        let mut arena: Arena<String> = Arena::default();
        let id = arena.alloc("original".to_string());
        *arena.get_mut(id) = "modified".to_string();
        assert_eq!(arena.get(id), "modified");
    }

    #[test]
    fn definitions_insert_and_get() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();
        let sym = interner.intern("foo");
        let def_id = defs.insert(None, sym, DefKind::Let);
        let def = defs.get(def_id);
        assert_eq!(def.id(), def_id);
        assert_eq!(def.name, sym);
    }

    #[test]
    fn definitions_resolve_simple_path() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();
        let sym = interner.intern("my_var");
        let def_id = defs.insert(None, sym, DefKind::Let);
        let path = Path::Simple(sym);
        assert_eq!(defs.resolve(&path), Some(def_id));
    }

    #[test]
    fn definitions_resolve_qualified_path() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();

        let parent_sym = interner.intern("MyModule");
        let child_sym = interner.intern("my_func");

        let parent_id = defs.insert(None, parent_sym, DefKind::Mod);
        let child_id = defs.insert(Some(parent_id), child_sym, DefKind::Let);

        let path = Path::Qualified(vec![parent_sym, child_sym]);
        assert_eq!(defs.resolve(&path), Some(child_id));
    }

    #[test]
    fn definitions_resolve_missing() {
        let mut interner = Interner::default();
        let defs = Definitions::default();
        let sym = interner.intern("does_not_exist");
        let path = Path::Simple(sym);
        assert_eq!(defs.resolve(&path), None);
    }

    #[test]
    fn definitions_len() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();
        assert_eq!(defs.len(), 0);

        let s1 = interner.intern("a");
        let s2 = interner.intern("b");
        let s3 = interner.intern("c");
        defs.insert(None, s1, DefKind::Let);
        defs.insert(None, s2, DefKind::Let);
        defs.insert(None, s3, DefKind::Let);
        assert_eq!(defs.len(), 3);
    }

    #[test]
    fn definitions_get_by_symbol() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();
        let sym = interner.intern("target");
        let def_id = defs.insert(None, sym, DefKind::Type);
        let found = defs.get_by_symbol(sym);
        assert!(found.is_some());
        assert_eq!(found.unwrap().id(), def_id);
    }

    #[test]
    fn definitions_iter() {
        let mut interner = Interner::default();
        let mut defs = Definitions::default();
        let s1 = interner.intern("x");
        let s2 = interner.intern("y");
        defs.insert(None, s1, DefKind::Let);
        defs.insert(None, s2, DefKind::Let);

        let names: Vec<Symbol> = defs.iter().map(|def| def.name).collect();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&s1));
        assert!(names.contains(&s2));
    }

    #[test]
    fn global_context_register_provider() {
        let mut gcx = GlobalContext::default();
        gcx.register_provider("csv", MockProvider);

        let sym = gcx.interner.lookup("csv").expect("csv should be interned");
        let def = gcx.definitions.get_by_symbol(sym);
        assert!(def.is_some());
        assert!(matches!(def.unwrap().kind, DefKind::Provider(_)));
    }

    #[test]
    fn global_context_register_module() {
        let mut gcx = GlobalContext::default();
        gcx.register_module(
            "Rdf",
            ModuleSpec {
                functions: vec![FunctionDef::new("serialize", MockFunction)],
            },
        );

        let rdf_sym = gcx.interner.intern("Rdf");
        let ser_sym = gcx.interner.intern("serialize");
        let path = Path::Qualified(vec![rdf_sym, ser_sym]);
        let resolved = gcx.definitions.resolve(&path);
        assert!(resolved.is_some());
        let def = gcx.definitions.get(resolved.unwrap());
        assert!(matches!(def.kind, DefKind::Func(_)));
    }
}
