//! OutputPlugin — output format handlers (graphar, turtle, nquads, ...).

use std::collections::HashMap;

use super::source::ParamInfo;

/// Trait for output format plugins.
pub trait OutputPlugin: Send + Sync {
    /// Format name used in scripts (e.g., "graphar" in `Rdf.materialize`).
    fn name(&self) -> &'static str;

    /// Parameters accepted (e.g., chunk_size for graphar).
    fn params(&self) -> Vec<ParamInfo>;
}
