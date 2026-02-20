pub mod metadata;
pub mod serializer;

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
pub use metadata::{RdfFieldInfo, RdfMetadata, RdfMetadataResult};
pub use serializer::RdfBatchWriter;

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::context::Symbol;
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

/// Extract the `PrimitiveType` from a `BuiltInFieldType`, discarding optionality.
fn primitive_of(ft: &BuiltInFieldType) -> PrimitiveType {
    match ft {
        BuiltInFieldType::Required(p) | BuiltInFieldType::Optional(p) => *p,
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

    let dest = ctx.output_resolver.resolve_output(destination)?;

    let ext = dest
        .extension()
        .unwrap_or_else(|| "ttl".to_string());

    let format =
        RdfFormat::from_extension(&ext).ok_or_else(|| RdfError::UnsupportedFormat(ext.clone()))?;

    let batch_size = estimate_batch_size_from_plan(plan);

    // Phase 1: Build combined selections and XSD type maps for each output type.
    //
    // For each output we:
    //   1. Extract RDF metadata (predicates, rdf:type, base) from type attributes
    //   2. Resolve XSD datatypes from Fossil's type registry (the plan schema
    //      only contains DataType::Unknown, so it can't be used)
    //   3. Build the Polars selection: _subject, _graph, _type, predicate columns
    //   4. Pre-compute the XSD type map (predicate URI → XSD IRI) once
    let output_configs: Vec<_> = plan
        .outputs
        .iter()
        .map(|output_spec| {
            let type_name = ctx.gcx.definitions.get(output_spec.type_def_id).name;
            let type_name_str = interner.resolve(type_name);

            // 1. Extract RDF metadata from #[rdf] attributes
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

            if let Some(ref result) = rdf_result {
                for warning in &result.warnings.0 {
                    eprintln!("warning: {:?}", warning);
                }
            }

            let mut rdf_metadata = rdf_result
                .map(|r| r.metadata)
                .filter(|m| m.has_metadata())
                .unwrap_or_default();

            // 2. Resolve XSD datatypes from Fossil's type registry
            if let Some(registered) = ctx.gcx.registered_types.get(&output_spec.type_def_id) {
                let field_types: HashMap<Symbol, PrimitiveType> = registered
                    .iter()
                    .map(|(sym, ft)| (*sym, primitive_of(ft)))
                    .collect();
                rdf_metadata.resolve_xsd_types(&field_types);
            }

            // 3. Build Polars selection
            let mut selection: Vec<Expr> = Vec::new();

            if let Some(subject_expr) = output_spec.ctor_args.first() {
                let subject_expr = if let Some(ref base) = rdf_metadata.base {
                    concat_str([lit(base.as_str()), subject_expr.clone()], "", true)
                } else {
                    subject_expr.clone()
                };
                selection.push(subject_expr.alias("_subject"));
            }

            if let Some(graph_expr) = output_spec.ctor_args.get(1) {
                selection.push(graph_expr.clone().alias("_graph"));
            }

            if let Some(ref rdf_type) = rdf_metadata.rdf_type {
                selection.push(lit(rdf_type.as_str()).alias("_type"));
            }

            for transform_expr in &output_spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(field_info) = rdf_metadata.fields.get(&field_sym)
                {
                    let expr = match field_info.primitive_type {
                        Some(prim) if prim != PrimitiveType::String => {
                            inner.as_ref().clone().cast(prim.to_polars_dtype())
                        }
                        _ => inner.as_ref().clone(),
                    };
                    selection.push(expr.alias(&field_info.uri));
                }
            }

            // 4. Pre-compute XSD type map (done once, reused across all batches)
            let xsd_types = rdf_metadata.xsd_type_map();

            (selection, xsd_types)
        })
        .collect();

    // Phase 2: Stream batches through the RDF serializer
    serialize_oxigraph(dest.writer, format, plan, &output_configs, batch_size)
}

fn serialize_oxigraph(
    writer: Box<dyn Write + Send>,
    format: RdfFormat,
    plan: &Plan,
    output_configs: &[(Vec<Expr>, HashMap<String, String>)],
    batch_size: usize,
) -> Result<Value, FossilError> {
    let rdf_writer = RefCell::new(RdfBatchWriter::new(writer, format));

    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_batched(plan, |batch| {
            let lazy_batch = batch.clone().lazy();

            for (selection, xsd_types) in output_configs {
                if selection.is_empty() {
                    continue;
                }

                let rdf_batch = lazy_batch
                    .clone()
                    .select(selection.clone())
                    .collect()
                    .map_err(|e| {
                        PolarsError::ComputeError(
                            format!("Failed to apply RDF selection: {}", e).into(),
                        )
                    })?;

                rdf_writer.borrow_mut().write_batch(&rdf_batch, xsd_types)?;
            }
            Ok(())
        })
        .map_err(|e| RdfError::Write(e.to_string()))?;

    let mut writer = rdf_writer
        .into_inner()
        .finish()
        .map_err(|e| RdfError::Finalize(e.to_string()))?;

    writer
        .flush()
        .map_err(|e| RdfError::Finalize(e.to_string()))?;

    Ok(Value::Unit)
}
