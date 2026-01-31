pub mod metadata;
pub mod serializer;

use std::cell::RefCell;
use std::path::PathBuf;
use std::str::FromStr;

pub use metadata::{RdfFieldInfo, RdfMetadata, RdfTermType};
pub use serializer::{ColumnarNTriplesWriter, PredicateColumn, RdfBatchWriter};

use fossil_lang::error::{CompileError, RuntimeError};
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{OutputPlan, OutputSpec, RecordsPlan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use oxrdf::{NamedNode, Term, TermParseError};
use oxrdfio::RdfFormat;
use polars::prelude::*;
use thiserror::Error;

/// RDF-specific errors
#[derive(Debug, Error)]
pub enum RdfError {
    // Validation errors
    #[error("'{0}' is not a valid subject")]
    InvalidSubject(String),
    #[error("Invalid predicate '{0}'")]
    InvalidPredicate(String),
    #[error(transparent)]
    TermParse(#[from] TermParseError),

    // Function argument errors
    #[error("map() requires at least 1 argument: source")]
    MapMissingSource,
    #[error("map() first argument must be Records (from a type provider)")]
    MapInvalidSource,
    #[error("map() requires at least one transform closure")]
    MapMissingTransform,
    #[error("map() transform must return a typed record (e.g., Type {{ field = value }})")]
    MapTransformNotTyped,
    #[error("map() transform must return a record type")]
    MapTransformNotRecord,
    #[error("map() transform requires at least one parameter")]
    MapTransformNoParams,
    #[error("map() expects a function as argument")]
    MapNotAFunction,

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

impl From<RdfError> for CompileError {
    fn from(err: RdfError) -> Self {
        CompileError::from(RuntimeError::evaluation(err.to_string(), fossil_lang::ast::Loc::generated()))
    }
}

pub fn is_subject(s: &str) -> Result<(), RdfError> {
    match Term::from_str(s)? {
        Term::BlankNode(_) | Term::NamedNode(_) | Term::Triple(_) => Ok(()),
        Term::Literal(_) => Err(RdfError::InvalidSubject(s.into())),
    }
}

pub fn is_predicate(s: &str) -> Result<(), RdfError> {
    NamedNode::from_str(s).map_err(|_| RdfError::InvalidPredicate(s.into()))?;
    Ok(())
}

pub struct MapFunction;

impl FunctionImpl for MapFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall S O R. (List<S>, fn(S) -> O) -> R
        // Where:
        // - S is the source row type
        // - O is the output type (inferred from transform return)
        // - R is the result type (opaque OutputPlan)
        // Note: Additional closures are accepted at runtime (variadic)
        let s_var = next_type_var();
        let o_var = next_type_var();
        let r_var = next_type_var();

        let s_ty = ir.var_type(s_var);
        let o_ty = ir.var_type(o_var);
        let r_ty = ir.var_type(r_var);

        let list_s = ir.list_type(s_ty);
        let transform_fn = ir.fn_type(vec![s_ty], o_ty);

        Polytype::poly(
            vec![s_var, o_var, r_var],
            ir.fn_type(vec![list_s, transform_fn], r_ty),
        )
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, CompileError> {
        use fossil_lang::runtime::value::SourceDescriptor;

        if args.is_empty() {
            return Err(RdfError::MapMissingSource.into());
        }

        let source_plan = match &args[0] {
            Value::Records(plan) => plan.clone(),
            _ => return Err(RdfError::MapInvalidSource.into()),
        };

        let closures = &args[1..];
        if closures.is_empty() {
            return Err(RdfError::MapMissingTransform.into());
        }

        let mut outputs = Vec::with_capacity(closures.len());

        for transform_fn in closures {
            let row_context = Value::Records(RecordsPlan::new(
                SourceDescriptor::Empty,
                source_plan.schema.as_ref().clone(),
            ));

            let traced_result = trace_transform(transform_fn, row_context, ctx)?;

            let (select_exprs, output_schema, type_def_id, meta_fields) = match traced_result {
                Value::Records(traced_plan) => {
                    if let Some(exprs) = traced_plan.source.take_select_exprs() {
                        let type_def_id = traced_plan
                            .type_def_id
                            .ok_or(RdfError::MapTransformNotTyped)?;
                        (exprs, traced_plan.schema, type_def_id, traced_plan.meta_fields.clone())
                    } else {
                        return Err(RdfError::MapTransformNotRecord.into());
                    }
                }
                _ => return Err(RdfError::MapTransformNotRecord.into()),
            };

            outputs.push(OutputSpec {
                type_def_id,
                select_exprs,
                schema: output_schema,
                meta_fields,
            });
        }

        Ok(Value::OutputPlan(OutputPlan {
            source: source_plan,
            outputs,
        }))
    }
}

/// Trace a transformation function to discover the output structure
fn trace_transform(
    transform_fn: &Value,
    row_context: Value,
    ctx: &RuntimeContext,
) -> Result<Value, CompileError> {
    use fossil_lang::runtime::evaluator::IrEvaluator;

    match transform_fn {
        Value::Closure {
            params, body, env, ..
        } => {
            let mut closure_env = (**env).clone();

            if let Some(first_param) = params.first() {
                closure_env.bind(first_param.name, row_context);
            } else {
                return Err(RdfError::MapTransformNoParams.into());
            }

            let mut evaluator = IrEvaluator::new(ctx.ir, ctx.gcx, closure_env);
            evaluator.eval(*body)
        }

        Value::BuiltinFunction(_, func) => func.call(vec![row_context], ctx),

        _ => Err(RdfError::MapNotAFunction.into()),
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

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, CompileError> {
        let mut args_iter = args.into_iter();

        let input_value = args_iter.next().ok_or(RdfError::SerializeMissingArgs)?;

        let filename = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or(RdfError::SerializeInvalidFilename)?;

        match input_value {
            Value::OutputPlan(output_plan) => serialize_rdf(&output_plan, &filename, ctx),
            _ => Err(RdfError::SerializeInvalidInput.into()),
        }
    }
}

/// Multi-output RDF serialization
///
/// Iterates the source data ONCE and applies all output transforms,
/// writing to a single RDF file. This is the streaming implementation
/// for OutputPlan created by chaining `to()` calls.
///
/// Uses columnar serialization for N-Triples/N-Quads (faster),
/// falls back to oxigraph for other formats (Turtle, RDF/XML, etc).
fn serialize_rdf(
    output_plan: &OutputPlan,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, CompileError> {
    let interner = ctx.gcx.interner.clone();

    let path = PathBuf::from(destination);

    let ext = path
        .extension()
        .map(|e| e.to_string_lossy().to_string())
        .unwrap_or_else(|| "ttl".to_string());

    let format = RdfFormat::from_extension(&ext)
        .ok_or_else(|| RdfError::UnsupportedFormat(ext.clone()))?;

    // Use columnar serializer for N-Triples/N-Quads (much faster)
    let use_columnar = matches!(format, RdfFormat::NTriples | RdfFormat::NQuads);

    // Estimate batch size from source plan schema
    let batch_size = estimate_batch_size_from_plan(&output_plan.source);

    // Prepare combined selections for each output
    // Each selection combines: @id meta-field + rdf:type + predicate columns
    let output_configs: Vec<_> = output_plan
        .outputs
        .iter()
        .map(|output_spec| {
            // Get RDF metadata for this output type
            let rdf_metadata = ctx
                .gcx
                .type_metadata
                .get(&output_spec.type_def_id)
                .and_then(|tm| RdfMetadata::from_type_metadata(tm, &interner))
                .unwrap_or_default();

            // Build a combined selection that operates on source columns directly
            let mut combined_selection: Vec<Expr> = Vec::new();

            // 1. Generate _subject column from @id meta-field
            // Look for @id by comparing symbol names
            for (name, expr) in &output_spec.meta_fields {
                let name_str = interner.resolve(*name);
                if name_str == "id" {
                    combined_selection.push(expr.clone().alias("_subject"));
                }
                // TODO: Handle @graph for named graphs
            }

            // 2. Add _type column if rdf_type is specified
            if let Some(ref rdf_type) = rdf_metadata.rdf_type {
                combined_selection.push(lit(rdf_type.as_str()).alias("_type"));
            }

            // 3. Add predicate columns by applying transform expressions
            // and renaming to their URIs
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

    if use_columnar {
        serialize_columnar(path, output_plan, &output_configs, batch_size)
    } else {
        serialize_oxigraph(path, format, output_plan, &output_configs, batch_size)
    }
}

/// Columnar N-Triples serialization (fast path)
///
/// Uses Polars vectorized string operations - no per-row function calls.
fn serialize_columnar(
    path: PathBuf,
    output_plan: &OutputPlan,
    output_configs: &[(Vec<Expr>, RdfMetadata)],
    batch_size: usize,
) -> Result<Value, CompileError> {
    let writer = ColumnarNTriplesWriter::new(path)
        .map_err(|e| RdfError::CreateWriter(e.to_string()))?;

    let writer = RefCell::new(Some(writer));

    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_batched(&output_plan.source, |batch| {
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
                        PolarsError::ComputeError(format!("RDF selection failed: {}", e).into())
                    })?;

                if let Some(ref mut w) = *writer.borrow_mut() {
                    w.write_batch(&rdf_batch)?;
                }
            }
            Ok(())
        })
        .map_err(|e| RdfError::Write(e.to_string()))?;

    if let Some(w) = writer.borrow_mut().take() {
        w.finish().map_err(|e| RdfError::Finalize(e.to_string()))?;
    }

    Ok(Value::Unit)
}

/// Oxigraph-based serialization (supports all RDF formats)
///
/// Used for Turtle, RDF/XML, and other formats that need special handling.
fn serialize_oxigraph(
    path: PathBuf,
    format: RdfFormat,
    output_plan: &OutputPlan,
    output_configs: &[(Vec<Expr>, RdfMetadata)],
    batch_size: usize,
) -> Result<Value, CompileError> {
    let writer = RdfBatchWriter::new(path, format)
        .map_err(|e| RdfError::CreateWriter(e.to_string()))?;

    let writer = RefCell::new(writer);

    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_batched(&output_plan.source, |batch| {
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
