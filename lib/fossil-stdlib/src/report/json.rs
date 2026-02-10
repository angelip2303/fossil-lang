//! JSON report sink for validation results
//!
//! Generates a JSON file with validation summary, per-rule statistics, and findings.

use std::cell::RefCell;
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{Value, Plan};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

use crate::validate::metadata::ValidateRule;
use super::{filter_to_conforming, get_entity_id, get_rules, report_sink_signature, report_sink_call};

pub struct ReportJsonFunction;

impl FunctionImpl for ReportJsonFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        report_sink_signature(ir, next_type_var, gcx)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        report_sink_call(args, ctx, "Report.json", write_json_report)
    }
}

fn write_json_report(mut plan: Plan, destination: &str, ctx: &RuntimeContext) -> Result<Value, FossilError> {
    let rules = get_rules(&plan, ctx, "Report.json")?;
    let batch_size = estimate_batch_size_from_plan(&plan);
    let executor = ChunkedExecutor::new(batch_size);

    // Accumulate statistics across batches (sees ALL rows, including non-conforming)
    let stats = RefCell::new(RuleStats::new(&rules));

    executor
        .execute_plan_batched(&plan, |batch| {
            stats.borrow_mut().process_batch(&batch, &rules)?;
            Ok(())
        })
        .map_err(|e| {
            FossilError::evaluation(
                format!("Report.json execution error: {}", e),
                fossil_lang::ast::Loc::generated(),
            )
        })?;

    let stats = stats.into_inner();

    // Build JSON output
    let total_entities = stats.total_rows;
    let total_rules = rules.len();
    let total_violations: u64 = stats.violation_counts.iter().sum();
    let pass_rate = if total_entities > 0 {
        1.0 - (total_violations as f64 / (total_entities as f64 * total_rules as f64))
    } else {
        1.0
    };

    let mut rule_reports = Vec::new();
    for (i, rule) in rules.iter().enumerate() {
        let mut rule_json = serde_json::json!({
            "field": rule.field_name,
            "constraint": rule.constraint,
            "sourceConstraintComponent": rule.source_constraint_component,
            "message": rule.message,
            "violations": stats.violation_counts[i],
        });
        if let Some(ref path) = rule.result_path {
            rule_json["resultPath"] = serde_json::Value::String(path.clone());
        }
        rule_reports.push(rule_json);
    }

    let report = serde_json::json!({
        "@type": "sh:ValidationReport",
        "conforms": total_violations == 0,
        "summary": {
            "total_entities": total_entities,
            "total_rules": total_rules,
            "violations": total_violations,
            "pass_rate": (pass_rate * 1000.0).round() / 1000.0,
        },
        "rules": rule_reports,
        "results": stats.findings,
    });

    // Write to file
    let path = PathBuf::from(destination);
    let file = File::create(&path).map_err(|e| {
        FossilError::evaluation(
            format!("Failed to create report file '{}': {}", destination, e),
            fossil_lang::ast::Loc::generated(),
        )
    })?;
    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, &report).map_err(|e| {
        FossilError::evaluation(
            format!("Failed to write JSON report: {}", e),
            fossil_lang::ast::Loc::generated(),
        )
    })?;

    // Filter plan for downstream: only conforming rows, drop _check_* columns
    filter_to_conforming(&mut plan, &rules);

    Ok(Value::Plan(plan))
}

/// Accumulates validation statistics across batches
struct RuleStats {
    total_rows: u64,
    violation_counts: Vec<u64>,
    findings: Vec<serde_json::Value>,
}

impl RuleStats {
    fn new(rules: &[ValidateRule]) -> Self {
        Self {
            total_rows: 0,
            violation_counts: vec![0; rules.len()],
            findings: Vec::new(),
        }
    }

    fn process_batch(
        &mut self,
        batch: &DataFrame,
        rules: &[ValidateRule],
    ) -> PolarsResult<()> {
        self.total_rows += batch.height() as u64;

        for (i, rule) in rules.iter().enumerate() {
            if let Ok(col) = batch.column(&rule.column_name) {
                let empty = BooleanChunked::full("".into(), true, 0);
                let bool_col = col.bool().unwrap_or(&empty);

                // Columnar: count violations without row iteration
                let pass_count = bool_col.sum().unwrap_or(0) as u64;
                let fail_count = bool_col.len() as u64 - pass_count;
                self.violation_counts[i] += fail_count;

                // Row iteration only for individual finding details (capped)
                if fail_count > 0 && self.findings.len() < 1000 {
                    for (row_idx, val) in bool_col.into_iter().enumerate() {
                        if !val.unwrap_or(true) && self.findings.len() < 1000 {
                            let entity_id = get_entity_id(batch, row_idx);
                            let field_value = get_field_value(batch, &rule.field_name, row_idx);

                            let mut finding = serde_json::json!({
                                "@type": "sh:ValidationResult",
                                "focusNode": entity_id,
                                "resultPath": rule.result_path.as_deref().unwrap_or(&rule.field_name),
                                "resultSeverity": rule.severity.as_shacl_iri(),
                                "sourceConstraintComponent": rule.source_constraint_component,
                                "resultMessage": rule.message,
                                "value": field_value,
                            });

                            finding["field"] = serde_json::Value::String(rule.field_name.clone());
                            self.findings.push(finding);
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

/// Get the value of a field at a given row as a JSON value
fn get_field_value(batch: &DataFrame, field_name: &str, row_idx: usize) -> serde_json::Value {
    if let Ok(col) = batch.column(field_name)
        && let Ok(val) = col.get(row_idx)
    {
        return match val {
            AnyValue::Null => serde_json::Value::Null,
            AnyValue::String(s) => serde_json::Value::String(s.to_string()),
            AnyValue::StringOwned(s) => serde_json::Value::String(s.to_string()),
            other => serde_json::Value::String(format!("{}", other)),
        };
    }
    serde_json::Value::Null
}
