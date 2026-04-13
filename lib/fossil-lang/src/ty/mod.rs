//! Layer 4: type system (type inference, type checker, metadata).
//!
//! Facade module mirroring rust-analyzer's `hir-ty` crate. Exposes the
//! type checker and metadata extraction.
//!
//! Future refactor: physically move `passes/typecheck/` and `metadata.rs`
//! into this module.

pub use crate::metadata::{extract_type_metadata, AttributeData, FieldMetadata, TypeMetadata};
pub use crate::passes::typecheck::TypeChecker;
