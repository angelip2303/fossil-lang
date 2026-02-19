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

impl PrimitiveType {
    /// Convert to the canonical Polars DataType.
    pub fn to_polars_dtype(self) -> polars::prelude::DataType {
        use polars::prelude::DataType;
        match self {
            PrimitiveType::Int => DataType::Int64,
            PrimitiveType::Float => DataType::Float64,
            PrimitiveType::Bool => DataType::Boolean,
            PrimitiveType::String => DataType::String,
        }
    }
}

impl From<polars::prelude::DataType> for PrimitiveType {
    fn from(value: polars::prelude::DataType) -> Self {
        use polars::prelude::DataType;
        match value {
            DataType::Boolean => PrimitiveType::Bool,
            DataType::Int8
            | DataType::Int16
            | DataType::Int32
            | DataType::Int64
            | DataType::Int128
            | DataType::UInt8
            | DataType::UInt16
            | DataType::UInt32
            | DataType::UInt64 => PrimitiveType::Int,
            DataType::Float32 | DataType::Float64 => PrimitiveType::Float,
            DataType::String => PrimitiveType::String,
            _ => todo!("Unsupported data type: {:?}", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JoinHow {
    Inner,
    Left,
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
