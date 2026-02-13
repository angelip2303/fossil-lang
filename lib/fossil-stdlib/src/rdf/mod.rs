pub mod metadata;
pub mod serializer;

use std::cell::RefCell;
use std::path::PathBuf;
pub use metadata::{RdfFieldInfo, RdfMetadata, RdfMetadataResult};
pub use serializer::RdfBatchWriter;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{Plan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use oxrdfio::RdfFormat;
use polars::prelude::*;
use thiserror::Error;

/// RDF-specific errors
#[derive(Debug, Error)]
pub enum RdfError {
    // Serialize errors
    #[error("Rdf::serialize requires input and filename")]
    SerializeMissingArgs,
    #[error("Rdf::serialize filename must be a string literal")]
    SerializeInvalidFilename,
    #[error("Rdf::serialize expects an OutputPlan")]
    SerializeInvalidInput,
    #[error("Unsupported RDF format extension: {0}")]
    UnsupportedFormat(String),

    // I/O errors
    #[error("Failed to create RDF writer: {0}")]
    CreateWriter(String),
    #[error("Failed to write RDF: {0}")]
    Write(String),
    #[error("Failed to finalize RDF file: {0}")]
    Finalize(String),
}

impl From<RdfError> for FossilError {
    fn from(err: RdfError) -> Self {
        FossilError::evaluation(err.to_string(), fossil_lang::ast::Loc::generated())
    }
}

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

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args_iter = args.into_iter();

        let input_value = args_iter.next().ok_or(RdfError::SerializeMissingArgs)?;

        let filename = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or(RdfError::SerializeInvalidFilename)?;

        match input_value {
            Value::Plan(plan) if plan.has_outputs() => serialize_rdf(&plan, &filename, ctx),
            _ => Err(RdfError::SerializeInvalidInput.into()),
        }
    }
}

fn serialize_rdf(
    plan: &Plan,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, FossilError> {
    let interner = ctx.gcx.interner.clone();

    let path = PathBuf::from(destination);

    let ext = path
        .extension()
        .map(|e| e.to_string_lossy().to_string())
        .unwrap_or_else(|| "ttl".to_string()); // TODO: here we should raise error?

    let format =
        RdfFormat::from_extension(&ext).ok_or_else(|| RdfError::UnsupportedFormat(ext.clone()))?;

    let batch_size = estimate_batch_size_from_plan(plan);

    // Prepare combined selections for each output
    // Each selection combines: @id meta-field + rdf:type + predicate columns
    let output_configs: Vec<_> = plan
        .outputs
        .iter()
        .map(|output_spec| {
            let type_name = ctx.gcx.definitions.get(output_spec.type_def_id).name;
            let type_name_str = interner.resolve(type_name);

            let rdf_result = ctx
                .gcx
                .type_metadata
                .get(&output_spec.type_def_id)
                .map(|tm| {
                    RdfMetadata::from_type_metadata_with_warnings(
                        tm,
                        &interner,
                        Some(type_name_str),
                    )
                });

            // Log any warnings (at runtime, we can't use ariadne)
            if let Some(ref result) = rdf_result {
                for warning in &result.warnings.0 {
                    eprintln!("warning: {:?}", warning);
                }
            }

            let rdf_metadata = rdf_result
                .map(|r| r.metadata)
                .filter(|m| m.has_metadata())
                .unwrap_or_default();

            let mut combined_selection: Vec<Expr> = Vec::new();

            // 1. Generate _subject and _graph columns from constructor arguments (positional)
            // Position 0 = subject, Position 1 = graph (optional)
            if let Some(subject_expr) = output_spec.ctor_args.first() {
                let subject_expr = if let Some(ref base) = rdf_metadata.base {
                    concat_str([lit(base.as_str()), subject_expr.clone()], "", true)
                } else {
                    subject_expr.clone()
                };
                combined_selection.push(subject_expr.alias("_subject"));
            }

            if let Some(graph_expr) = output_spec.ctor_args.get(1) {
                combined_selection.push(graph_expr.clone().alias("_graph"));
            }

            if let Some(ref rdf_type) = rdf_metadata.rdf_type {
                combined_selection.push(lit(rdf_type.as_str()).alias("_type"));
            }

            for transform_expr in &output_spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(field_info) = rdf_metadata.fields.get(&field_sym)
                {
                    combined_selection.push(inner.as_ref().clone().alias(&field_info.uri));
                }
            }

            (combined_selection, rdf_metadata)
        })
        .collect();

    serialize_oxigraph(path, format, plan, &output_configs, batch_size)
}

fn serialize_oxigraph(
    path: PathBuf,
    format: RdfFormat,
    plan: &Plan,
    output_configs: &[(Vec<Expr>, RdfMetadata)],
    batch_size: usize,
) -> Result<Value, FossilError> {
    let writer =
        RdfBatchWriter::new(path, format).map_err(|e| RdfError::CreateWriter(e.to_string()))?;

    let writer = RefCell::new(writer);

    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_batched(plan, |batch| {
            let lazy_batch = batch.clone().lazy();

            for (combined_selection, _rdf_metadata) in output_configs {
                if combined_selection.is_empty() {
                    continue;
                }

                let rdf_batch = lazy_batch
                    .clone()
                    .select(combined_selection.clone())
                    .collect()
                    .map_err(|e| {
                        PolarsError::ComputeError(
                            format!("Failed to apply RDF selection: {}", e).into(),
                        )
                    })?;

                writer.borrow_mut().write_batch(&rdf_batch)?;
            }
            Ok(())
        })
        .map_err(|e| RdfError::Write(e.to_string()))?;

    writer
        .into_inner()
        .finish()
        .map_err(|e| RdfError::Finalize(e.to_string()))?;

    Ok(Value::Unit)
}
