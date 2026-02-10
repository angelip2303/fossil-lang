//! Validation metadata extraction from type attributes
//!
//! Mirrors the `rdf/metadata.rs` pattern — uses `#[derive(FromAttrs)]` for
//! type-safe attribute extraction from `@validate(...)` annotations.
//!
//! `required` is inferred from the field's IR type: `T` = required, `T?` = optional.
//!
//! # Example
//!
//! ```fossil
//! type ValidWall do
//!     @validate(min_length = 1)
//!     name: string          // required
//!     description: string?  // optional
//! end
//! ```

use fossil_lang::context::{Interner, TypeMetadata};
use fossil_macros::FromAttrs;

use super::constraints::field_checks;
use super::collect_required_fields;

/// Field-level validation constraints, extracted via FromAttrs derive macro.
/// Same pattern as `RdfFieldAttrs` in `rdf/metadata.rs`.
///
/// Note: `required` is no longer an attribute — it's inferred from the IR type.
/// `T` = required, `T?` = optional.
#[derive(Debug, Clone, Default, FromAttrs)]
pub struct ValidateFieldAttrs {
    #[attr("validate.min_length", field)]
    pub min_length: Option<i64>,
    #[attr("validate.max_length", field)]
    pub max_length: Option<i64>,
    #[attr("validate.length", field)]
    pub length: Option<i64>,
    #[attr("validate.pattern", field)]
    pub pattern: Option<String>,
    #[attr("validate.min_inclusive", field)]
    pub min_inclusive: Option<String>,
    #[attr("validate.max_inclusive", field)]
    pub max_inclusive: Option<String>,
    #[attr("validate.min_exclusive", field)]
    pub min_exclusive: Option<String>,
    #[attr("validate.max_exclusive", field)]
    pub max_exclusive: Option<String>,
    #[attr("validate.one_of", field)]
    pub one_of: Option<String>,
}

/// Rule metadata for a single validation constraint.
///
/// Lives in fossil-stdlib (not fossil-lang). Used by both `ValidWall.check`
/// and Report sinks — both derive rules from the same `TypeMetadata` source.
///
/// Fields align with SHACL validation vocabulary where applicable.
#[derive(Debug, Clone)]
pub struct ValidateRule {
    /// Column name for the boolean result (e.g., `_check_name_required`)
    pub column_name: String,
    /// The field being validated
    pub field_name: String,
    /// The constraint type (e.g., "required", "min_length")
    pub constraint: String,
    /// Human-readable message (e.g., "Field 'name' is required")
    pub message: String,
    /// SHACL result path — the RDF predicate URI (from `@rdf(uri)`), if available
    pub result_path: Option<String>,
    /// SHACL source constraint component IRI
    pub source_constraint_component: String,
    /// SHACL result severity
    pub severity: Severity,
}

/// SHACL-aligned severity levels for validation results
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Violation,
}

impl Severity {
    pub fn as_shacl_iri(&self) -> &'static str {
        match self {
            Severity::Violation => "sh:Violation",
        }
    }
}

/// Map a Fossil constraint name to its SHACL sourceConstraintComponent IRI
fn shacl_constraint_component(constraint: &str) -> String {
    let component = match constraint {
        "required" => "sh:MinCountConstraintComponent",
        "min_length" => "sh:MinLengthConstraintComponent",
        "max_length" => "sh:MaxLengthConstraintComponent",
        "length" => "sh:MinLengthConstraintComponent",
        "pattern" => "sh:PatternConstraintComponent",
        "min_inclusive" => "sh:MinInclusiveConstraintComponent",
        "max_inclusive" => "sh:MaxInclusiveConstraintComponent",
        "min_exclusive" => "sh:MinExclusiveConstraintComponent",
        "max_exclusive" => "sh:MaxExclusiveConstraintComponent",
        "one_of" => "sh:InConstraintComponent",
        other => return format!("fossil:{}", other),
    };
    component.to_string()
}

/// Information needed to infer required from IR types
pub struct RequiredContext<'a> {
    pub ir: &'a fossil_lang::ir::Ir,
    pub type_name: fossil_lang::context::Symbol,
}

/// Extract all validation rules from TypeMetadata.
///
/// Used by `ValidWall.check` AND Report sinks — same source of truth.
/// For each field with `@validate(...)` attributes, extracts constraints
/// and builds `ValidateRule` entries with column names, messages, and SHACL metadata.
///
/// When `required_ctx` is provided, also generates required rules by examining
/// field IR types: `T` = required, `T?` = optional.
///
/// Also looks up `@rdf(uri)` on each field to populate `result_path`.
pub fn extract_validate_rules(
    type_metadata: &TypeMetadata,
    interner: &Interner,
    required_ctx: Option<&RequiredContext<'_>>,
) -> Vec<ValidateRule> {
    use crate::rdf::metadata::RdfFieldAttrs;

    let mut rules = Vec::new();

    // Generate required rules from IR types (if context available)
    if let Some(rctx) = required_ctx {
        for rf in collect_required_fields(rctx.ir, rctx.type_name, interner) {
            let result_path = type_metadata
                .field_metadata
                .get(&rf.field_sym)
                .map(|fm| RdfFieldAttrs::from_field_metadata(fm, interner))
                .and_then(|attrs| attrs.uri);

            rules.push(ValidateRule {
                column_name: rf.column_name,
                field_name: rf.field_name.clone(),
                result_path,
                source_constraint_component: shacl_constraint_component("required"),
                severity: Severity::Violation,
                constraint: "required".to_string(),
                message: format!("Field '{}' is required", rf.field_name),
            });
        }
    }

    // Generate attribute-based rules (min_length, pattern, etc.)
    for (field_sym, field_meta) in &type_metadata.field_metadata {
        let field_name = interner.resolve(*field_sym);
        let validate_attrs = ValidateFieldAttrs::from_field_metadata(field_meta, interner);
        let rdf_attrs = RdfFieldAttrs::from_field_metadata(field_meta, interner);

        for (_expr, constraint, message) in field_checks(field_name, &validate_attrs) {
            let column_name = format!("_check_{}_{}", field_name, constraint);
            let source_constraint_component = shacl_constraint_component(&constraint);
            rules.push(ValidateRule {
                column_name,
                field_name: field_name.to_string(),
                result_path: rdf_attrs.uri.clone(),
                source_constraint_component,
                severity: Severity::Violation,
                constraint,
                message,
            });
        }
    }

    rules
}

