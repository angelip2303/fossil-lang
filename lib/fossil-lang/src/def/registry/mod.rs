//! Compiler registry: sources (readers) + sinks (writers) + attribute operations.
//!
//! Mirror of DataFusion's SessionState catalog/function split.
//! The host registers data sources via the builder; language-level features
//! (Rdf.materialize sink, clean/anon ops) are registered via with_default_features().

pub mod attributes;
pub mod sinks;
pub mod sources;

pub use attributes::{
    default_anon_ops, default_clean_ops, AnonOp, AttributeOp, AttributeOpKind,
    AttributeRegistry, CleanOp,
};
pub use sinks::{SinkDef, SinkFormat, SinkRegistry};
pub use sources::{ParamDef, SourceDef, SourceRegistry};

/// Compiler registry — sources, sinks, attribute operations.
/// Stored as a field of `FossilDb`. Immutable post-build.
#[derive(Debug, Clone, Default)]
pub struct FossilRegistry {
    pub sources: SourceRegistry,
    pub sinks: SinkRegistry,
    pub attributes: AttributeRegistry,
}

impl FossilRegistry {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Register language defaults (Rdf.materialize sink, clean/anon attribute ops).
/// Mirror of DataFusion's `with_default_features()`.
pub fn register_defaults(reg: &mut FossilRegistry) {
    reg.sinks.register(SinkDef::new("Rdf", "materialize", SinkFormat::Graphar));

    for op in default_clean_ops() {
        reg.attributes.register(op);
    }
    for op in default_anon_ops() {
        reg.attributes.register(op);
    }
}
