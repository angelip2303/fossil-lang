//! Built-in functions and attribute operations.
//!
//! All builtins are const data — no traits, no dynamic dispatch.
//! Each is individually testable and composable.

use crate::registry::*;

// ── Source functions ─────────────────────────────────────────────────

pub const CSV: FunctionDef = FunctionDef::source(
    "csv",
    "SELECT * FROM read_csv('{path}', delim='{delimiter}', header={header})",
    &[
        ParamDef::required("path"),
        ParamDef::optional("delimiter"),
        ParamDef::optional("header"),
    ],
);

pub const EXCEL: FunctionDef = FunctionDef::source(
    "excel",
    "SELECT * FROM st_read('{path}', layer='{sheet}')",
    &[
        ParamDef::required("path"),
        ParamDef::optional("sheet"),
    ],
);

pub const PARQUET: FunctionDef = FunctionDef::source(
    "parquet",
    "SELECT * FROM read_parquet('{path}')",
    &[ParamDef::required("path")],
);

pub const PDF: FunctionDef = FunctionDef::preprocess(
    "pdf",
    "pdf_extract",
    &[ParamDef::required("path")],
);

pub const DOCX: FunctionDef = FunctionDef::preprocess(
    "docx",
    "docx_extract",
    &[ParamDef::required("path")],
);

pub const SOURCES: &[FunctionDef] = &[CSV, EXCEL, PARQUET, PDF, DOCX];

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

/// Register all built-in functions and attributes into a Registry.
pub fn register(r: &mut Registry) {
    r.add_functions(SOURCES);
    r.add_function(RDF_MATERIALIZE);
    r.add_function(TEXT_EXTRACT);
    r.add_attributes(CLEAN_OPS);
    r.add_attributes(ANON_OPS);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_finds_csv() {
        let mut r = Registry::new();
        register(&mut r);
        let csv = r.find_source("csv").expect("csv not found");
        assert_eq!(csv.name, "csv");
        assert!(matches!(csv.impl_, OpImpl::SourceSql(_)));
    }

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
