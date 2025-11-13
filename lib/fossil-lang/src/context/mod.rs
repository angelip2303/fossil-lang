use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

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
