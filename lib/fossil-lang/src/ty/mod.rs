//! Layer 4: type system (type inference, type checker, metadata).
//!
//! Mirror of rust-analyzer's `hir-ty` crate. Owns the type checker and
//! metadata extraction. Reads from `def/` (DefMap, ItemTree, RegisteredTypes)
//! and is consumed by `codegen/` to lower typed IR to relational queries.

pub mod metadata;
pub mod typecheck;
pub mod types;

pub use metadata::{extract_type_metadata, AttributeData, FieldMetadata, TypeMetadata};
pub use typecheck::TypeChecker;
pub use types::{Polytype, RecordFields, Ty, TyKind, TypeVar};
