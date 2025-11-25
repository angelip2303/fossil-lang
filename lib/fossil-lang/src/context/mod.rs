use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
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
        Self {
            idx: self.idx,
            _marker: PhantomData,
        }
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

#[derive(Debug)]
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

    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.strings[sym.0 as usize]
    }
}

impl Default for Interner {
    fn default() -> Self {
        Interner {
            map: HashMap::new(),
            strings: Vec::new(),
        }
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
}

#[derive(Default)]
pub struct Definitions {
    items: Vec<Def>,
}

impl Definitions {
    pub fn insert(&mut self, parent: Option<DefId>, name: Symbol, kind: DefKind) -> DefId {
        let id = DefId::new(self.items.len() as u32);
        self.items.push(Def::new(id, parent, name, kind));
        id
    }

    pub fn get(&self, id: DefId) -> &Def {
        &self.items[id.index() as usize]
    }

    pub fn get_by_name(&self, name: Symbol) -> Option<&Def> {
        self.items.iter().find(|def| def.name == name)
    }

    pub fn resolve(&self, path: impl Into<Path>) -> Option<DefId> {
        let path = path.into();

        // if the path is empty, return None
        if path.is_empty() {
            return None;
        }

        // try to find the item by name, return None otherwise
        let mut item = match self.get_by_name(path.item()) {
            Some(item) => item,
            None => return None,
        };

        // if the path has a parent, resolve it recursively, return None otherwise
        let parent = match path.parent() {
            Some(parent) => parent,
            None => {
                return match item.parent {
                    Some(parent) => Some(parent),
                    None => Some(item.id),
                };
            }
        };

        Into::<Vec<Symbol>>::into(parent.rev())
            .iter()
            .all(|i: &Symbol| match item.parent {
                Some(parent) => {
                    item = self.get(parent);
                    i == &item.name
                }
                None => false,
            })
            .then(|| item.id)
    }
}
