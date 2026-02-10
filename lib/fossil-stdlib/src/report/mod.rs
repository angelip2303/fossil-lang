//! Report sinks for validation results
//!
//! Generates JSON or CSV reports from validated Plans by deriving rules from TypeMetadata.
//! After reporting, the plan is filtered to only conforming rows for downstream consumers.

pub mod json;
pub mod csv;

pub use json::ReportJsonFunction;
pub use csv::ReportCsvFunction;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{Plan, Transform, Value};
use fossil_lang::traits::function::RuntimeContext;
use polars::prelude::*;

use crate::validate::metadata::{RequiredContext, ValidateRule, extract_validate_rules};

/// Shared signature for Report sinks: `forall T. (Validated<T>, String) -> Validated<T>`
pub(crate) fn report_sink_signature(
    ir: &mut Ir,
    next_type_var: &mut dyn FnMut() -> TypeVar,
    gcx: &GlobalContext,
) -> Polytype {
    let t_var = next_type_var();
    let t_ty = ir.var_type(t_var);

    let validated_def_id = gcx
        .interner
        .lookup("Validated")
        .and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()))
        .expect("Validated type constructor must be registered");
    let validated_t = ir.app_type(validated_def_id, vec![t_ty]);

    let filename_ty = ir.string_type();
    Polytype::poly(
        vec![t_var],
        ir.fn_type(vec![validated_t, filename_ty], validated_t),
    )
}

/// Shared call() extraction for Report sinks: extract Plan + filename, dispatch to writer
pub(crate) fn report_sink_call(
    args: Vec<Value>,
    ctx: &RuntimeContext,
    name: &str,
    writer_fn: fn(Plan, &str, &RuntimeContext) -> Result<Value, FossilError>,
) -> Result<Value, FossilError> {
    let mut args_iter = args.into_iter();

    let input = args_iter.next().ok_or_else(|| {
        FossilError::evaluation(
            format!("{} requires a Plan argument", name),
            fossil_lang::ast::Loc::generated(),
        )
    })?;

    let filename = args_iter
        .next()
        .and_then(|v| v.as_literal_string())
        .ok_or_else(|| {
            FossilError::evaluation(
                format!("{} requires a filename string", name),
                fossil_lang::ast::Loc::generated(),
            )
        })?;

    match input {
        Value::Plan(plan) => writer_fn(plan, &filename, ctx),
        _ => Err(FossilError::evaluation(
            format!("{} expects a validated Plan", name),
            fossil_lang::ast::Loc::generated(),
        )),
    }
}

/// Derive validation rules from TypeMetadata.
///
/// Checks plan output specs first, then falls back to plan.type_def_id.
/// Includes type-inferred required rules (T = required, T? = optional).
pub(crate) fn get_rules(plan: &Plan, ctx: &RuntimeContext, context: &str) -> Result<Vec<ValidateRule>, FossilError> {
    let type_def_ids = plan.type_def_ids();
    if type_def_ids.is_empty() {
        return Err(FossilError::evaluation(
            format!("{}: plan has no type info. Use Type.check first.", context),
            fossil_lang::ast::Loc::generated(),
        ));
    }

    let mut rules = Vec::new();
    for def_id in type_def_ids {
        if let Some(tm) = ctx.gcx.type_metadata.get(&def_id) {
            let def = ctx.gcx.definitions.get(def_id);
            let required_ctx = RequiredContext {
                ir: ctx.ir,
                type_name: def.name,
            };
            rules.extend(extract_validate_rules(tm, &ctx.gcx.interner, Some(&required_ctx)));
        }
    }

    if rules.is_empty() {
        return Err(FossilError::evaluation(
            format!("{}: no validation rules found. Use Type.check first.", context),
            fossil_lang::ast::Loc::generated(),
        ));
    }
    Ok(rules)
}

/// Try to get an entity identifier from the first column or a _subject column
pub(crate) fn get_entity_id(batch: &DataFrame, row_idx: usize) -> String {
    // Try _subject column first (from projection output)
    if let Ok(col) = batch.column("_subject")
        && let Ok(val) = col.get(row_idx)
    {
        let s = format!("{}", val);
        if s != "null" {
            return s;
        }
    }
    // Fall back to first column
    if let Some(col) = batch.get_columns().first()
        && let Ok(val) = col.get(row_idx)
    {
        return format!("{}", val);
    }
    format!("row_{}", row_idx)
}

/// Filter a plan to only conforming rows.
///
/// Called after Report has processed all rows (including violations).
/// Downstream consumers (e.g., `Rdf.serialize`) only see conforming data.
/// The _check_* columns remain but are harmless — RDF serialization only
/// uses columns with @rdf(uri) mappings.
pub(crate) fn filter_to_conforming(plan: &mut Plan, rules: &[ValidateRule]) {
    if rules.is_empty() {
        return;
    }

    // Filter: keep rows where ALL _check_* columns are true
    let filter_expr = rules
        .iter()
        .map(|r| col(r.column_name.as_str()))
        .reduce(|a, b| a.and(b));

    if let Some(filter) = filter_expr {
        plan.transforms.push(Transform::Filter(filter));
    }
}
