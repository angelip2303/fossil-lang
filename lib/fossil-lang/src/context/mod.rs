use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, OnceLock};

pub mod global;
pub mod metadata;

pub use self::global::*;
pub use self::metadata::*;
// Re-export DefId and DefKindTag from db.rs so `use crate::context::DefId` still works.
// DefKindTag replaces the old DefKind enum (same variants).
pub use crate::db::{DefId, DefKindTag, InternedDef};
/// Compat alias: old code imports `DefKind`; new name is `DefKindTag`.
pub type DefKind = DefKindTag;

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
impl<T: PartialEq> PartialEq for Arena<T> { fn eq(&self, other: &Self) -> bool { self.0 == other.0 } }
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

// ── Global interner (r-a pattern) ──────────────────────────────────
//
// Single process-wide Interner behind a Mutex.  Symbol::intern() and
// Symbol::as_str() are free functions that go through this global,
// eliminating the need to thread an `&mut Interner` everywhere.

fn global_interner() -> &'static Mutex<Interner> {
    static INTERNER: OnceLock<Mutex<Interner>> = OnceLock::new();
    INTERNER.get_or_init(|| Mutex::new(Interner::default()))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    /// Create a synthetic symbol for error messages
    /// This should only be used in error handling where we don't have access to an interner
    pub const fn synthetic() -> Self {
        Symbol(0)
    }

    /// Intern a string into the global interner, returning a stable Symbol.
    pub fn intern(s: &str) -> Self {
        global_interner().lock().unwrap().intern(s)
    }

    /// Resolve this symbol to its string representation (allocates).
    pub fn as_str(self) -> String {
        global_interner().lock().unwrap().resolve(self).to_string()
    }

    /// Lookup a string in the global interner without creating it.
    pub fn lookup(s: &str) -> Option<Self> {
        global_interner().lock().unwrap().lookup(s)
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
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

// DefId, DefKindTag → moved to db.rs as #[salsa::interned]
// Old DefId(u32), Def, DefKind, Definitions are removed.
// Use db::DefId<'db> and db::DefKindTag instead.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn global_interner_intern_and_resolve() {
        let sym = Symbol::intern("hello_global");
        assert_eq!(sym.as_str(), "hello_global");
    }

    #[test]
    fn local_interner_intern_and_resolve() {
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
    }
}
