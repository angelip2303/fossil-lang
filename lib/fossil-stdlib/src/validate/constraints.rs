//! Constraint → Polars expression translation
//!
//! Converts `ValidateFieldAttrs` into Polars boolean expressions
//! that can be applied to DataFrames for validation.

use polars::prelude::{col, lit, Expr};

use super::metadata::ValidateFieldAttrs;

/// Build all check expressions for one field from its ValidateFieldAttrs.
///
/// Returns a vec of (polars_expr, constraint_name, message) tuples.
/// The caller is responsible for aliasing the expr with the column name.
pub fn field_checks(
    field_name: &str,
    attrs: &ValidateFieldAttrs,
) -> Vec<(Expr, String, String)> {
    let mut checks = Vec::new();

    // Note: `required` checks are generated in validate_plan() based on IR types,
    // not from attributes. A field typed `T` (not `T?`) is required.

    if let Some(n) = attrs.min_length {
        let col_name = format!("_check_{}_{}", field_name, "min_length");
        checks.push((
            col(field_name)
                .str()
                .len_chars()
                .gt_eq(lit(n as u32))
                .alias(&col_name),
            "min_length".to_string(),
            format!("Field '{}' must have at least {} character(s)", field_name, n),
        ));
    }

    if let Some(n) = attrs.max_length {
        let col_name = format!("_check_{}_{}", field_name, "max_length");
        checks.push((
            col(field_name)
                .str()
                .len_chars()
                .lt_eq(lit(n as u32))
                .alias(&col_name),
            "max_length".to_string(),
            format!("Field '{}' must have at most {} character(s)", field_name, n),
        ));
    }

    if let Some(n) = attrs.length {
        let col_name = format!("_check_{}_{}", field_name, "length");
        checks.push((
            col(field_name)
                .str()
                .len_chars()
                .eq(lit(n as u32))
                .alias(&col_name),
            "length".to_string(),
            format!("Field '{}' must have exactly {} character(s)", field_name, n),
        ));
    }

    if let Some(ref pat) = attrs.pattern {
        let col_name = format!("_check_{}_{}", field_name, "pattern");
        checks.push((
            col(field_name)
                .str()
                .contains(lit(pat.as_str()), false)
                .alias(&col_name),
            "pattern".to_string(),
            format!("Field '{}' must match pattern: {}", field_name, pat),
        ));
    }

    if let Some(ref s) = attrs.min_inclusive
        && let Ok(n) = s.parse::<f64>()
    {
        let col_name = format!("_check_{}_{}", field_name, "min_inclusive");
        checks.push((
            col(field_name).gt_eq(lit(n)).alias(&col_name),
            "min_inclusive".to_string(),
            format!("Field '{}' must be >= {}", field_name, s),
        ));
    }

    if let Some(ref s) = attrs.max_inclusive
        && let Ok(n) = s.parse::<f64>()
    {
        let col_name = format!("_check_{}_{}", field_name, "max_inclusive");
        checks.push((
            col(field_name).lt_eq(lit(n)).alias(&col_name),
            "max_inclusive".to_string(),
            format!("Field '{}' must be <= {}", field_name, s),
        ));
    }

    if let Some(ref s) = attrs.min_exclusive
        && let Ok(n) = s.parse::<f64>()
    {
        let col_name = format!("_check_{}_{}", field_name, "min_exclusive");
        checks.push((
            col(field_name).gt(lit(n)).alias(&col_name),
            "min_exclusive".to_string(),
            format!("Field '{}' must be > {}", field_name, s),
        ));
    }

    if let Some(ref s) = attrs.max_exclusive
        && let Ok(n) = s.parse::<f64>()
    {
        let col_name = format!("_check_{}_{}", field_name, "max_exclusive");
        checks.push((
            col(field_name).lt(lit(n)).alias(&col_name),
            "max_exclusive".to_string(),
            format!("Field '{}' must be < {}", field_name, s),
        ));
    }

    if let Some(ref s) = attrs.one_of {
        let col_name = format!("_check_{}_{}", field_name, "one_of");
        let values: Vec<&str> = s.split(',').collect();
        let expr = values
            .iter()
            .map(|v| col(field_name).eq(lit(*v)))
            .reduce(|a, b| a.or(b))
            .unwrap_or(lit(false));
        checks.push((
            expr.alias(&col_name),
            "one_of".to_string(),
            format!("Field '{}' must be one of: {}", field_name, s),
        ));
    }

    checks
}
