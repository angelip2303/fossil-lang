use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId<T> {
    idx: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> NodeId<T> {
    fn new(idx: usize) -> Self {
        Self {
            idx: idx as u32,
            _marker: PhantomData,
        }
    }

    pub fn idx(&self) -> usize {
        self.idx as usize
    }
}

pub struct Arena<T> {
    items: Vec<T>,
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

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { items: Vec::new() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

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

    pub fn len(&self) -> usize {
        self.strings.len()
    }

    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
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
