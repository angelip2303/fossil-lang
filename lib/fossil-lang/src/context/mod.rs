use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::sync::Arc;

use crate::ast::ast::Path;
use crate::traits::function::FunctionImpl;
use crate::traits::provider::TypeProviderImpl;

pub struct NodeId<T> {
    idx: u32,
    _marker: PhantomData<T>,
}

impl<T> NodeId<T> {
    pub fn new(idx: usize) -> Self {
        Self {
            idx: idx as u32,
            _marker: PhantomData,
        }
    }

    pub fn idx(&self) -> usize {
        self.idx as usize
    }

    pub fn into<U>(self) -> NodeId<U> {
        NodeId::new(self.idx as usize)
    }
}

impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for NodeId<T> {}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for NodeId<T> where NodeId<T>: Clone {}

impl<T> Hash for NodeId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}

impl<T> Debug for NodeId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeId({})", self.idx)
    }
}

pub struct Arena<T> {
    items: Vec<T>,
}

impl<T: Clone> Clone for Arena<T> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
        }
    }
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, item: T) -> NodeId<T> {
        let id = NodeId::new(self.items.len());
        self.items.push(item);
        id
    }

    pub fn get(&self, id: NodeId<T>) -> &T {
        &self.items[id.idx()]
    }

    pub fn get_mut(&mut self, id: NodeId<T>) -> &mut T {
        &mut self.items[id.idx()]
    }

    pub fn iter(&self) -> impl Iterator<Item = (NodeId<T>, &T)> {
        self.items
            .iter()
            .enumerate()
            .map(|(idx, item)| (NodeId::new(idx), item))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (NodeId<T>, T)> {
        self.items
            .into_iter()
            .enumerate()
            .map(|(idx, item)| (NodeId::new(idx), item))
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { items: Vec::new() }
    }
}

impl<T: Debug> Debug for Arena<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Arena").field("items", &self.items).finish()
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
        &self.strings[sym.0 as usize]
    }

    /// Try to resolve a symbol, returning None if the symbol index is out of bounds.
    /// This is useful when the symbol may have been interned in a different interner
    /// (e.g., during error recovery in compilation).
    pub fn try_resolve(&self, sym: Symbol) -> Option<&str> {
        self.strings.get(sym.0 as usize).map(|s| s.as_str())
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
    #[allow(dead_code)]
    parent: Option<DefId>,
    pub name: Symbol,
    pub kind: DefKind,
}

#[derive(Clone)]
pub enum DefKind {
    /// A module definition
    /// - file_path: The source file for this module (None for inline/builtin modules)
    /// - is_inline: Whether this is an inline module definition (vs file-based)
    Mod {
        file_path: Option<PathBuf>,
        is_inline: bool,
    },
    Let,
    Type,
    Func(Option<Arc<dyn FunctionImpl>>),
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
}

#[derive(Default, Clone)]
pub struct Definitions {
    items: Vec<Def>,
}

impl Definitions {
    pub fn get(&self, id: DefId) -> &Def {
        &self.items[id.index() as usize]
    }

    pub fn get_by_symbol(&self, name: Symbol) -> Option<&Def> {
        self.items.iter().find(|def| def.name == name)
    }

    pub fn insert(&mut self, parent: Option<DefId>, name: Symbol, kind: DefKind) -> DefId {
        let id = DefId::new(self.items.len() as u32);
        self.items.push(Def::new(id, parent, name, kind));
        id
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn resolve(&self, path: &Path) -> Option<DefId> {
        // if the path is empty, return None
        if path.is_empty() {
            return None;
        }

        // For simple paths, just look up the item symbol
        match path {
            Path::Simple(sym) => self.get_by_symbol(*sym).map(|def| def.id()),
            Path::Qualified(parts) => {
                // For qualified paths, resolve step by step through the hierarchy
                // Example: Entity::with_id resolves as: Entity (module) -> with_id (function)
                if parts.is_empty() {
                    return None;
                }

                // Start with the first part (module name)
                let mut current_id = self.get_by_symbol(parts[0]).map(|def| def.id())?;

                // Navigate through the rest of the path (supports arbitrary depth now)
                for &part in &parts[1..] {
                    // Find item with this symbol that has current_id as parent
                    current_id = self
                        .items
                        .iter()
                        .find(|def| def.name == part && def.parent == Some(current_id))
                        .map(|def| def.id())?;
                }

                Some(current_id)
            }
            Path::Relative { .. } => {
                // Relative path resolution requires context (current module)
                // This will be implemented in the resolver pass (Step 1.4)
                // For now, return None
                None
            }
        }
    }

    pub fn get_children(&self, parent_id: DefId) -> Vec<&Def> {
        self.items
            .iter()
            .filter(|def| def.parent == Some(parent_id))
            .collect()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Def> {
        self.items.iter()
    }

    /// Update a function definition's implementation
    ///
    /// This is used to install runtime implementations for auto-generated functions
    /// like record constructors.
    ///
    /// # Panics
    /// Panics if the DefId is not a function or doesn't exist
    pub fn update_function(&mut self, id: DefId, func: Option<Arc<dyn FunctionImpl>>) {
        let def = &mut self.items[id.index() as usize];
        match &mut def.kind {
            DefKind::Func(impl_slot) => {
                *impl_slot = func;
            }
            _ => panic!("Attempted to update non-function definition as function"),
        }
    }
}

/// Metadata about a module for tracking file-based organization
#[derive(Clone, Debug)]
pub struct ModuleInfo {
    /// The DefId of this module
    pub def_id: DefId,
    /// Parent module (None for root modules)
    pub parent: Option<DefId>,
    /// Source file path for this module
    pub file_path: PathBuf,
    /// Child module DefIds
    pub children: Vec<DefId>,
}

/// Kind of a type (for kind checking)
///
/// In type theory, kinds classify types. The simplest kind is `Type` (denoted `*`),
/// which represents concrete types like `Int`, `String`, `Person`, etc.
///
/// Future extensions will support higher-kinded types like `* -> *` for type
/// constructors that take other types as arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    /// The kind of concrete types: `*`
    ///
    /// Examples: `Int`, `String`, `Person`, `Entity<Person>`, `List<Int>`
    Type,
}

/// Metadata about a type constructor (generic type)
///
/// A type constructor is a type that takes type parameters. For example:
/// - `List` has arity 1 and kind `* -> *` (takes one type, returns a type)
/// - `Entity` has arity 1 and kind `* -> *`
/// - `Map` would have arity 2 and kind `* -> * -> *`
///
/// This information is used during type checking to:
/// 1. Validate that type applications have the correct number of arguments
/// 2. Perform kind checking (future)
/// 3. Generate meaningful error messages
#[derive(Debug, Clone)]
pub struct TypeConstructorInfo {
    /// The DefId of this type constructor
    pub def_id: DefId,
    /// Number of type parameters this constructor expects
    ///
    /// Examples:
    /// - `List` has arity 1: `List<T>`
    /// - `Entity` has arity 1: `Entity<T>`
    /// - `Map` would have arity 2: `Map<K, V>`
    pub arity: usize,
    /// Name of the type constructor (for error messages)
    pub name: Symbol,
    /// Kinds of the type parameters
    ///
    /// Currently all parameters have kind `Type`, but this will be extended
    /// to support higher-kinded types in the future.
    pub param_kinds: Vec<Kind>,
}

pub mod metadata;
pub use self::metadata::*;
