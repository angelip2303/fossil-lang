pub mod metadata;

pub use metadata::RdfMetadata;

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::*;


/// Rdf::serialize function implementation
///
/// Signature: (Records, string) -> Unit
///
/// Serializes a RecordsPlan with identity annotations to an RDF file.
/// Uses ChunkedExecutor for memory-efficient batch processing.
pub struct RdfSerializeFunction;

impl FunctionImpl for RdfSerializeFunction {
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

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let mut args_iter = args.into_iter();

        let records_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "Rdf::serialize requires records and filename".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        let filename = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime(
                        "Rdf::serialize filename must be a string literal".to_string(),
                    ),
                    Loc::generated(),
                )
            })?;

        // Extract RecordsPlan
        let plan = match records_value {
            Value::Records(plan) => plan,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Rdf::serialize expects records".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Verify we have identity expression (from Entity::with_id)
        let _identity_expr = plan.identity_expr.as_ref().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "Rdf::serialize requires records with Entity::with_id applied".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        // Get RDF metadata from type
        let rdf_metadata = plan
            .type_def_id
            .and_then(|def_id| {
                ctx.gcx
                    .type_metadata
                    .get(&def_id)
                    .and_then(|tm| RdfMetadata::from_type_metadata(tm, &ctx.gcx.interner))
            })
            .ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime(
                        "Rdf::serialize requires a typed record with #[rdf(...)] attributes"
                            .to_string(),
                    ),
                    Loc::generated(),
                )
            })?;

        serialize_streaming(&plan, &rdf_metadata, &filename, ctx)
    }
}

/// RDF serialization using chunked execution
///
/// Uses ChunkedExecutor to process data in fixed-size batches, ensuring constant
/// memory usage regardless of dataset size.
fn serialize_streaming(
    plan: &fossil_lang::runtime::value::RecordsPlan,
    rdf_metadata: &RdfMetadata,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};

    let interner = ctx.gcx.interner.clone();

    // Build selection expressions from RDF metadata
    let selection: Vec<Expr> = rdf_metadata
        .predicates
        .iter()
        .map(|(key, value)| col(interner.resolve(*key)).alias(value))
        .collect();

    // Estimate batch size from plan schema
    let batch_size = estimate_batch_size_from_plan(plan);

    // Execute plan with selection - ChunkedExecutor is the ONLY place materialization happens
    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_with_select_to_parquet(plan, selection, destination)
        .map_err(|e| {
            fossil_lang::error::CompileError::new(
                fossil_lang::error::CompileErrorKind::Runtime(format!(
                    "Failed to write parquet: {}",
                    e
                )),
                Loc::generated(),
            )
        })?;

    Ok(Value::Unit)
}
