use std::collections::HashMap;

pub trait Pool<T> {
    fn add(&mut self, t: T) -> NodeId;
    fn get(&self, idx: NodeId) -> &T;
}

pub struct NodeId(u32);

pub struct GenericPool<T>(Vec<T>);

impl<T> Pool<T> for GenericPool<T> {
    fn add(&mut self, t: T) -> NodeId {
        let idx = self.0.len();
        self.0.push(t);
        NodeId(idx as u32)
    }

    fn get(&self, idx: NodeId) -> &T {
        &self.0[idx.0 as usize]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

pub struct Interner {
    map: HashMap<String, Symbol>,
    strings: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

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
