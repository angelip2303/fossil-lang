//! Fossil standard library.
//!
//! Dual API: legacy init() + new register() for FunctionDef/AttributeOp.

pub mod ops;
pub mod rdf;
pub mod string;
pub mod text;

use fossil_lang::passes::GlobalContext;
use fossil_lang::registry::{AttributeOp, FunctionDef, Registry};
use fossil_lang::traits::provider::{FunctionDef as LegacyFunctionDef, ModuleSpec};

use ops::make_module_generator;
use rdf::RdfMaterializeFunction;
use text::TextExtractFunction;

// ── Legacy init ──────────────────────────────────────────────────────

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_module("Rdf", ModuleSpec {
        functions: vec![
            LegacyFunctionDef::new("materialize", RdfMaterializeFunction),
        ],
    });

    gcx.register_module("Text", ModuleSpec {
        functions: vec![
            LegacyFunctionDef::new("extract", TextExtractFunction),
        ],
    });

    gcx.module_generators.push(make_module_generator("clean", ops::clean::clean_functions));
    gcx.module_generators.push(make_module_generator("anon", ops::anon::anon_functions));
}

// ── New: const registrations ─────────────────────────────────────────

pub const RDF_MATERIALIZE: FunctionDef = FunctionDef::output("Rdf", "materialize", "graphar");
pub const TEXT_EXTRACT: FunctionDef = FunctionDef::pipeline("Text", "extract", "text_extract");

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

pub const ANON_OPS: &[AttributeOp] = &[
    AttributeOp::new("anon", "hash",     "SHA256(CAST({col} AS VARCHAR))"),
    AttributeOp::new("anon", "redact",   "'[REDACTED]'"),
    AttributeOp::new("anon", "mask",     "CONCAT(LEFT(CAST({col} AS VARCHAR), {keep}), '***')"),
    AttributeOp::new("anon", "suppress", "NULL"),
];

pub fn register(r: &mut Registry) {
    r.add_function(RDF_MATERIALIZE);
    r.add_function(TEXT_EXTRACT);
    r.add_attributes(CLEAN_OPS);
    r.add_attributes(ANON_OPS);
}
