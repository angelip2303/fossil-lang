//! Layer 1: database foundations + common types.
//!
//! Mirror of rust-analyzer's `base-db` crate. Exposes the `Db` trait,
//! `FossilDb`, `FossilDbBuilder`, `HasRegistry`, `HasSchemaResolver`,
//! and primitive types (Symbol, DefId, Loc, SourceFile) used by all higher layers.

pub mod common;
pub mod db;

pub use common::{Literal, Path, PrimitiveType, MetaArg};
pub use db::{
    Db, DefId, DefKindTag, Diagnostic, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver,
    InternedDef, InternedSymbol, Severity, SourceFile, Symbol,
};
