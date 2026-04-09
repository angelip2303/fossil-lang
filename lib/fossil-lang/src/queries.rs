//! Top-level compilation queries.
//!
//! These wrap the existing compiler pipeline (parse → expand → lower →
//! typecheck) and add the new RQ lowering + SQL emission layer on top.
//!
//! Eventually these will become Salsa tracked queries. For now they're
//! plain functions that call through the existing pipeline.

use crate::compiler::{CompilerInput, TolerantResult};
use crate::error::FossilErrors;
use crate::passes::GlobalContext;
use crate::plan::FossilPlan;
use crate::registry::Registry;
use crate::rq::emit_sql::rq_to_sql;
use crate::rq::lower::RqLowering;

/// Compile a Fossil script to a FossilPlan (SQL + metadata).
///
/// This is the main entry point for Fossil v2: source → plan.
/// The plan is serializable and can be executed by any host.
pub fn compile_to_plan(
    source: &str,
    name: &str,
    gcx: GlobalContext,
    registry: &Registry,
) -> Result<FossilPlan, FossilErrors> {
    // Phase 1: Compile (parse → expand → lower → typecheck)
    let compiler = crate::compiler::Compiler::with_context(gcx);
    let result = compiler.compile(CompilerInput::Source {
        name: name.to_string(),
        content: source.to_string(),
    })?;

    // Phase 2: Lower IR → RQ
    let rq = RqLowering::new(
        &result.program.ir,
        &result.program.gcx,
        &result.program.type_index,
        &result.program.resolutions,
        &result.program.typeck_results,
        registry,
    )
    .lower()
    .map_err(|e| FossilErrors::from(e))?;

    // Phase 3: Emit RQ → SQL
    let sql = rq_to_sql(&rq);

    // Phase 4: Wrap in FossilPlan
    Ok(FossilPlan::from_rq(rq, sql))
}

/// Compile tolerantly (for LSP) — returns partial results even on type errors.
pub fn compile_tolerant_to_plan(
    source: &str,
    name: &str,
    gcx: GlobalContext,
    registry: &Registry,
) -> Result<(FossilPlan, TolerantResult), FossilErrors> {
    let compiler = crate::compiler::Compiler::with_context(gcx);
    let result = compiler.compile_tolerant(CompilerInput::Source {
        name: name.to_string(),
        content: source.to_string(),
    })?;

    // Try RQ lowering even if there were type errors
    let rq_result = RqLowering::new(
        &result.program.ir,
        &result.program.gcx,
        &result.program.type_index,
        &result.program.resolutions,
        &result.program.typeck_results,
        registry,
    )
    .lower();

    let plan = match rq_result {
        Ok(rq) => {
            let sql = rq_to_sql(&rq);
            FossilPlan::from_rq(rq, sql)
        }
        Err(_) => {
            // RQ lowering failed — return empty plan
            let rq = crate::rq::RelationalQuery::new();
            FossilPlan::from_rq(rq, String::new())
        }
    };

    Ok((plan, result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins;

    fn default_registry() -> Registry {
        let mut r = Registry::new();
        builtins::register(&mut r);
        r
    }

    #[test]
    fn compile_literal_to_plan() {
        let plan = compile_to_plan("let x = 42", "test", GlobalContext::default(), &default_registry())
            .expect("compilation failed");
        assert!(plan.sql.contains("SELECT 1")); // no transforms → trivial SQL
        assert!(plan.sources.is_empty());
        assert!(plan.outputs.is_empty());
    }

    #[test]
    fn plan_is_serializable() {
        let plan = compile_to_plan("let x = 42", "test", GlobalContext::default(), &default_registry())
            .expect("compilation failed");
        let json = serde_json::to_string_pretty(&plan).expect("serialization failed");
        assert!(json.contains("\"sql\""));
        let _: FossilPlan = serde_json::from_str(&json).expect("deserialization failed");
    }
}
