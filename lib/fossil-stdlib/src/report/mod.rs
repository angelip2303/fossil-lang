use std::cell::RefCell;
use std::io::Write;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{Plan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

pub struct ReportCsvFunction;

impl FunctionImpl for ReportCsvFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, String) -> Unit
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        let filename_ty = ir.string_type();
        let output_ty = ir.unit_type();
        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty, filename_ty], output_ty))
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let loc = fossil_lang::ast::Loc::generated();
        let mut args_iter = args.into_iter();

        let input = args_iter
            .next()
            .ok_or_else(|| FossilError::evaluation(String::from("Report.csv requires input"), loc))?;

        let filename = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                FossilError::evaluation(String::from("Report.csv filename must be a string"), loc)
            })?;

        let plan = match input {
            Value::Plan(p) => p,
            _ => return Err(FossilError::evaluation(String::from("Report.csv expects a Plan"), loc)),
        };

        let dest = ctx.output_resolver.resolve_output(&filename)?;
        write_csv(&plan, dest.writer)
    }
}

fn write_csv(plan: &Plan, writer: Box<dyn Write + Send>) -> Result<Value, FossilError> {
    let loc = fossil_lang::ast::Loc::generated();
    let batch_size = estimate_batch_size_from_plan(plan);

    let writer = RefCell::new(writer);
    let header_written = RefCell::new(false);

    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_batched(plan, |mut batch| {
            let include_header = !*header_written.borrow();
            CsvWriter::new(&mut *writer.borrow_mut())
                .include_header(include_header)
                .finish(&mut batch)?;
            *header_written.borrow_mut() = true;
            Ok(())
        })
        .map_err(|e| FossilError::evaluation(format!("Failed to write CSV: {}", e), loc))?;

    Ok(Value::Unit)
}
