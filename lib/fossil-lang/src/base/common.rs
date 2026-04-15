//! Types shared between AST and IR (identical in both representations).

use crate::db::Symbol;

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

    /// Display this path as a dot-separated string.
    pub fn display(&self, db: &dyn crate::db::Db) -> String {
        match self {
            Path::Simple(sym) => sym.text(db).to_string(),
            Path::Qualified(parts) => parts
                .iter()
                .map(|sym| sym.text(db))
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

/// Argument to a metaprogramming call (`csv!(path="…")`). Restricted to
/// literals because expansion happens at compile time and needs constant
/// values. Mirrors rustc's `MacCall` — args are tokens, not arbitrary exprs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetaArg {
    Positional(Literal),
    Named { name: Symbol, value: Literal },
}

impl MetaArg {
    pub fn value(&self) -> &Literal {
        match self {
            MetaArg::Positional(lit) => lit,
            MetaArg::Named { value, .. } => value,
        }
    }
}
