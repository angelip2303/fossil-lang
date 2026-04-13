//! Layer 1: database foundations + common types.
//!
//! Facade module mirroring rust-analyzer's `base-db` crate. Exposes the
//! `Db` trait, `FossilDb`, `FossilDbBuilder`, `HasRegistry`, `HasSchemaResolver`,
//! and primitive types (Symbol, DefId, Loc, SourceFile) used by all higher layers.
//!
//! Future refactor: physically move `db.rs` and primitives into this module
//! (non-breaking if re-exports remain stable).

pub use crate::common::{Literal, Path, PrimitiveType, ProviderArgument};
pub use crate::db::{
    Db, DefId, DefKindTag, Diagnostic, FossilDb, FossilDbBuilder, HasRegistry, HasSchemaResolver,
    InternedDef, InternedSymbol, Severity, SourceFile, Symbol,
};
