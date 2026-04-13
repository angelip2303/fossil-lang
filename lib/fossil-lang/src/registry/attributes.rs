//! Attribute operations: logical operations applied to columns.
//!
//! `#[clean(trim)]`, `#[anon(hash)]`, etc. The language knows the operations
//! (Trim, Lower, Slug, Hash, ...) but NOT how they execute. Each backend
//! (e.g., DuckDB) translates `CleanOp::Trim` to its specific SQL.

/// Registry of attribute operations.
#[derive(Debug, Clone, Default)]
pub struct AttributeRegistry {
    entries: Vec<AttributeOp>,
}

impl AttributeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, op: AttributeOp) {
        self.entries.push(op);
    }

    pub fn find(&self, namespace: &str, name: &str) -> Option<&AttributeOp> {
        self.entries
            .iter()
            .find(|op| op.namespace == namespace && op.name == name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &AttributeOp> {
        self.entries.iter()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// A registered attribute operation.
#[derive(Debug, Clone)]
pub struct AttributeOp {
    pub namespace: String,
    pub name: String,
    pub kind: AttributeOpKind,
}

impl AttributeOp {
    pub fn new(
        namespace: impl Into<String>,
        name: impl Into<String>,
        kind: AttributeOpKind,
    ) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            kind,
        }
    }
}

/// Logical kind of an attribute operation. Two namespaces today: clean + anon.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttributeOpKind {
    Clean(CleanOp),
    Anon(AnonOp),
}

/// Clean operations — column normalization. Logical, no SQL knowledge.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CleanOp {
    Trim,
    Lower,
    Upper,
    Slug,
    /// `#[clean(default = "x")]` — coalesce with replacement value.
    Default,
    /// `#[clean(to_null = "")]` — convert sentinel value to NULL.
    ToNull,
    /// `#[clean(min = 159)]` — floor.
    Min,
    /// `#[clean(max = 100)]` — ceiling.
    Max,
    /// `#[clean(replace = ...)]` — string replace.
    Replace,
}

/// Anonymization operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnonOp {
    Hash,
    Redact,
    /// `#[anon(mask = N)]` — keep first N chars, mask the rest.
    Mask,
    Suppress,
}

/// Default clean operations registered by `with_default_features`.
pub fn default_clean_ops() -> Vec<AttributeOp> {
    vec![
        AttributeOp::new("clean", "trim", AttributeOpKind::Clean(CleanOp::Trim)),
        AttributeOp::new("clean", "lower", AttributeOpKind::Clean(CleanOp::Lower)),
        AttributeOp::new("clean", "upper", AttributeOpKind::Clean(CleanOp::Upper)),
        AttributeOp::new("clean", "slug", AttributeOpKind::Clean(CleanOp::Slug)),
        AttributeOp::new("clean", "default", AttributeOpKind::Clean(CleanOp::Default)),
        AttributeOp::new("clean", "to_null", AttributeOpKind::Clean(CleanOp::ToNull)),
        AttributeOp::new("clean", "min", AttributeOpKind::Clean(CleanOp::Min)),
        AttributeOp::new("clean", "max", AttributeOpKind::Clean(CleanOp::Max)),
        AttributeOp::new("clean", "replace", AttributeOpKind::Clean(CleanOp::Replace)),
    ]
}

/// Default anon operations registered by `with_default_features`.
pub fn default_anon_ops() -> Vec<AttributeOp> {
    vec![
        AttributeOp::new("anon", "hash", AttributeOpKind::Anon(AnonOp::Hash)),
        AttributeOp::new("anon", "redact", AttributeOpKind::Anon(AnonOp::Redact)),
        AttributeOp::new("anon", "mask", AttributeOpKind::Anon(AnonOp::Mask)),
        AttributeOp::new("anon", "suppress", AttributeOpKind::Anon(AnonOp::Suppress)),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_finds_trim() {
        let mut reg = AttributeRegistry::new();
        for op in default_clean_ops() {
            reg.register(op);
        }
        let trim = reg.find("clean", "trim").expect("trim not found");
        assert_eq!(trim.kind, AttributeOpKind::Clean(CleanOp::Trim));
    }

    #[test]
    fn all_clean_ops_registered() {
        let ops = default_clean_ops();
        let mut reg = AttributeRegistry::new();
        for op in ops {
            reg.register(op);
        }
        for name in &[
            "trim", "lower", "upper", "slug", "default", "to_null", "min", "max", "replace",
        ] {
            assert!(reg.find("clean", name).is_some(), "missing: {}", name);
        }
    }

    #[test]
    fn all_anon_ops_registered() {
        let ops = default_anon_ops();
        let mut reg = AttributeRegistry::new();
        for op in ops {
            reg.register(op);
        }
        for name in &["hash", "redact", "mask", "suppress"] {
            assert!(reg.find("anon", name).is_some(), "missing: {}", name);
        }
    }
}
