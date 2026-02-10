//! CSV report sink for validation results
//!
//! Generates a CSV file with one row per violation.

use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{Value, Plan};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

use super::{filter_to_conforming, get_entity_id, get_rules, report_sink_signature, report_sink_call};

pub struct ReportCsvFunction;

impl FunctionImpl for ReportCsvFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        report_sink_signature(ir, next_type_var, gcx)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        report_sink_call(args, ctx, "Report.csv", write_csv_report)
    }
}

fn write_csv_report(mut plan: Plan, destination: &str, ctx: &RuntimeContext) -> Result<Value, FossilError> {
    let rules = get_rules(&plan, ctx, "Report.csv")?;
    let batch_size = estimate_batch_size_from_plan(&plan);
    let executor = ChunkedExecutor::new(batch_size);

    let path = PathBuf::from(destination);
    let file = File::create(&path).map_err(|e| {
        FossilError::evaluation(
            format!("Failed to create CSV report file '{}': {}", destination, e),
            fossil_lang::ast::Loc::generated(),
        )
    })?;

    let writer = std::cell::RefCell::new(CsvReportWriter::new(BufWriter::new(file)));

    executor
        .execute_plan_batched(&plan, |batch| {
            let mut w = writer.borrow_mut();

            // Write header on first batch
            if !w.header_written {
                w.write_header()?;
            }

            for rule in rules.iter() {
                if let Ok(col) = batch.column(&rule.column_name) {
                    let empty = BooleanChunked::full("".into(), true, 0);
                    let bool_col = col.bool().unwrap_or(&empty);
                    for (row_idx, val) in bool_col.into_iter().enumerate() {
                        if !val.unwrap_or(true) {
                            let entity = get_entity_id(&batch, row_idx);
                            let field_value = get_field_value(&batch, &rule.field_name, row_idx);
                            w.write_row(&entity, &rule.field_name, &rule.constraint, &rule.message, &field_value)?;
                        }
                    }
                }
            }

            Ok(())
        })
        .map_err(|e| {
            FossilError::evaluation(
                format!("Report.csv execution error: {}", e),
                fossil_lang::ast::Loc::generated(),
            )
        })?;

    // Filter plan for downstream: only conforming rows, drop _check_* columns
    filter_to_conforming(&mut plan, &rules);

    Ok(Value::Plan(plan))
}

struct CsvReportWriter<W: Write> {
    writer: W,
    header_written: bool,
}

impl<W: Write> CsvReportWriter<W> {
    fn new(writer: W) -> Self {
        Self {
            writer,
            header_written: false,
        }
    }

    fn write_header(&mut self) -> PolarsResult<()> {
        writeln!(self.writer, "entity,field,constraint,message,value")
            .map_err(|e| PolarsError::ComputeError(format!("CSV write error: {}", e).into()))?;
        self.header_written = true;
        Ok(())
    }

    fn write_row(
        &mut self,
        entity: &str,
        field: &str,
        constraint: &str,
        message: &str,
        value: &str,
    ) -> PolarsResult<()> {
        writeln!(
            self.writer,
            "{},{},{},{},{}",
            escape_csv(entity),
            escape_csv(field),
            escape_csv(constraint),
            escape_csv(message),
            escape_csv(value),
        )
        .map_err(|e| PolarsError::ComputeError(format!("CSV write error: {}", e).into()))
    }
}

/// Escape a CSV field value (quote if it contains comma, quote, or newline)
fn escape_csv(s: &str) -> String {
    if s.contains(',') || s.contains('"') || s.contains('\n') {
        format!("\"{}\"", s.replace('"', "\"\""))
    } else {
        s.to_string()
    }
}

/// Get the string representation of a field value at a given row
fn get_field_value(batch: &DataFrame, field_name: &str, row_idx: usize) -> String {
    if let Ok(col) = batch.column(field_name)
        && let Ok(val) = col.get(row_idx)
    {
        return match val {
            AnyValue::Null => "null".to_string(),
            other => format!("{}", other),
        };
    }
    "null".to_string()
}
