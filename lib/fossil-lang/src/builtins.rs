//! Built-in language features: output functions and attribute operations.
//!
//! Data sources are NOT registered here — the host registers them.
//! Only language-level features (outputs, pipelines, attributes) live here.

use crate::registry::*;

// ── Output functions ─────────────────────────────────────────────────

pub const RDF_MATERIALIZE: FunctionDef = FunctionDef::output("Rdf", "materialize", "graphar");

// ── Pipeline functions ───────────────────────────────────────────────

pub const TEXT_EXTRACT: FunctionDef = FunctionDef::pipeline("Text", "extract", "text_extract");

// ── Attribute ops: #[clean(...)] ─────────────────────────────────────

pub const CLEAN_OPS: &[AttributeOp] = &[
    AttributeOp::new("clean", "trim",    "TRIM({col})"),
    AttributeOp::new("clean", "lower",   "LOWER({col})"),
    AttributeOp::new("clean", "upper",   "UPPER({col})"),
    AttributeOp::new("clean", "slug",    "REGEXP_REPLACE(LOWER(TRIM({col})), '[^a-z0-9]+', '-', 'g')"),
    AttributeOp::new("clean", "default", "COALESCE({col}, {value})"),
    AttributeOp::new("clean", "to_null", "NULLIF({col}, {value})"),
    AttributeOp::new("clean", "min",     "GREATEST({col}, {value})"),
    AttributeOp::new("clean", "max",     "LEAST({col}, {value})"),
    AttributeOp::new("clean", "replace", "REPLACE({col}, {from}, {to})"),
];

// ── Attribute ops: #[anon(...)] ──────────────────────────────────────

pub const ANON_OPS: &[AttributeOp] = &[
    AttributeOp::new("anon", "hash",     "SHA256(CAST({col} AS VARCHAR))"),
    AttributeOp::new("anon", "redact",   "'[REDACTED]'"),
    AttributeOp::new("anon", "mask",     "CONCAT(LEFT(CAST({col} AS VARCHAR), {keep}), '***')"),
    AttributeOp::new("anon", "suppress", "NULL"),
];

// ── Register all ─────────────────────────────────────────────────────

/// Register language-level builtins. Sources are registered by the host.
pub fn register(r: &mut Registry) {
    r.add_function(RDF_MATERIALIZE);
    r.add_function(TEXT_EXTRACT);
    r.add_attributes(CLEAN_OPS);
    r.add_attributes(ANON_OPS);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_finds_trim() {
        let mut r = Registry::new();
        register(&mut r);
        let trim = r.find_attribute("clean", "trim").expect("trim not found");
        assert_eq!(trim.sql_template, "TRIM({col})");
    }

    #[test]
    fn registry_finds_rdf_materialize() {
        let mut r = Registry::new();
        register(&mut r);
        let mat = r.find_function("Rdf", "materialize").expect("materialize not found");
        assert!(matches!(mat.impl_, OpImpl::Output { format: "graphar" }));
    }

    #[test]
    fn all_clean_ops_registered() {
        let mut r = Registry::new();
        register(&mut r);
        for op in CLEAN_OPS {
            assert!(r.find_attribute("clean", op.name).is_some(), "missing: {}", op.name);
        }
    }

    #[test]
    fn all_anon_ops_registered() {
        let mut r = Registry::new();
        register(&mut r);
        for op in ANON_OPS {
            assert!(r.find_attribute("anon", op.name).is_some(), "missing: {}", op.name);
        }
    }
}
