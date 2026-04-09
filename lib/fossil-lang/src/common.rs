//! Types shared between AST and IR (identical in both representations).

use crate::context::{Interner, Symbol};

/// A path to an identifier (either simple, qualified, or relative)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    Simple(Symbol),
    Qualified(Vec<Symbol>),
}

impl Path {
    pub fn simple(sym: Symbol) -> Self {
        Path::Simple(sym)
    }

    pub fn qualified(parts: Vec<Symbol>) -> Self {
        match parts.as_slice() {
            [sym] => Path::Simple(*sym),
            _ => Path::Qualified(parts),
        }
    }

    pub fn display(&self, interner: &Interner) -> String {
        match self {
            Path::Simple(sym) => interner.resolve(*sym).to_string(),
            Path::Qualified(parts) => parts
                .iter()
                .map(|sym| interner.resolve(*sym))
                .collect::<Vec<_>>()
                .join("."),
        }
    }
}

impl From<Path> for Vec<Symbol> {
    fn from(path: Path) -> Self {
        match path {
            Path::Simple(sym) => vec![sym],
            Path::Qualified(parts) => parts,
        }
    }
}

impl From<Vec<Symbol>> for Path {
    fn from(parts: Vec<Symbol>) -> Self {
        Path::qualified(parts)
    }
}

impl From<Symbol> for Path {
    fn from(sym: Symbol) -> Self {
        Path::Simple(sym)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(i64),
    String(Symbol),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Float,
    String,
    Bool,
}

/// A provider argument (literal-based or positional)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProviderArgument {
    Positional(Literal),
    Named { name: Symbol, value: Literal },
}

impl ProviderArgument {
    pub fn value(&self) -> &Literal {
        match self {
            ProviderArgument::Positional(lit) => lit,
            ProviderArgument::Named { value, .. } => value,
        }
    }
}
