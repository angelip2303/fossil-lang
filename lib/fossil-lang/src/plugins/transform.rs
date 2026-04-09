//! TransformPlugin — declarative attribute transforms (#[clean], #[anon], ...).
//!
//! Registers an attribute namespace. The compiler parses attributes and
//! includes transform ops in the plan. Applied automatically when mapping.

/// A single transform operation parsed from an attribute.
#[derive(Debug, Clone)]
pub enum TransformOp {
    /// Named operation with optional parameters.
    Op {
        name: String,
        params: Vec<(String, String)>,
    },
}

/// Trait for transform attribute plugins.
pub trait TransformPlugin: Send + Sync {
    /// Attribute namespace (e.g., "clean", "anon").
    fn namespace(&self) -> &'static str;

    /// Generate SQL expression for a transform op applied to a column.
    fn to_sql(&self, op: &TransformOp, column: &str) -> String;
}
