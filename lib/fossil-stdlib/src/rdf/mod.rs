pub mod metadata;
pub mod serializer;

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
pub use serializer::RdfBatchWriter;

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{DefId, Symbol};
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeIndex, TypeKind as IrTypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
use fossil_lang::runtime::value::{Plan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use crate::string::template::parse_template;
use metadata::{RdfFieldAttrs, RdfTypeAttrs, build_xsd_type_map, field_primitive_type};

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

/// Build a blank node expression: `_:{TypeName}_{arg0}_{arg1}_...`
fn build_bnode_expr(def_id: DefId, args: &[Expr], gcx: &GlobalContext) -> Expr {
    let type_name = gcx.interner.resolve(gcx.definitions.get(def_id).name);
    let mut parts = vec![lit(format!("_:{type_name}"))];
    parts.extend(args.iter().map(|a| a.clone().cast(DataType::String)));
    concat_str(parts, "_", true)
}

/// Resolve a subject identity expression for a type.
///
/// With `#[rdf(subject = "...")]`: template expansion using param names.
/// Without: blank node `_:{TypeName}_{arg0}_{arg1}_...`
fn resolve_identity_expr(
    def_id: DefId,
    args: &[Expr],
    gcx: &GlobalContext,
    type_index: &TypeIndex,
) -> Expr {
    let template = RdfTypeAttrs::from_def_id(def_id, gcx)
        .and_then(|a| a.subject);

    match template {
        Some(template) => {
            let param_names = type_index.get(def_id)
                .map(|info| &info.ctor_param_names[..])
                .unwrap_or(&[]);
            let parts = parse_template(&template, param_names, args, &gcx.interner);
            concat_str(parts, "", true)
        }
        None => build_bnode_expr(def_id, args, gcx),
    }
}

/// Build the subject expression for a type's constructor args.
/// Returns `None` when there are no ctor args.
fn build_subject_expr(
    def_id: DefId,
    ctor_args: &[Expr],
    gcx: &GlobalContext,
    type_index: &TypeIndex,
) -> Option<Expr> {
    if ctor_args.is_empty() {
        return None;
    }
    Some(resolve_identity_expr(def_id, ctor_args, gcx, type_index))
}

/// Check if a field's type is a reference to another record type.
/// Returns the DefId of the referenced type if so.
fn field_ref_type(
    def_id: DefId,
    field: Symbol,
    ir: &Ir,
    type_index: &TypeIndex,
) -> Option<DefId> {
    let info = type_index.get(def_id)?;
    let IrTypeKind::Record(fields) = &ir.types.get(info.ty).kind else { return None };
    let field_ty = ir.types.get(fields.lookup(field)?);
    match &field_ty.kind {
        IrTypeKind::Named(ref_id) => Some(*ref_id),
        IrTypeKind::Optional(inner) => match &ir.types.get(*inner).kind {
            IrTypeKind::Named(ref_id) => Some(*ref_id),
            _ => None,
        },
        _ => None,
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
        .ok_or_else(|| RdfError::UnsupportedFormat(
            "no file extension; use .ttl, .nt, .nq, or .jsonld".to_string()
        ))?;

    let format =
        RdfFormat::from_extension(&ext).ok_or_else(|| RdfError::UnsupportedFormat(ext.clone()))?;

    let batch_size = estimate_batch_size_from_plan(plan);

    // Phase 1: Build combined selections and XSD type maps for each output type.
    //
    // For each output we:
    //   1. Read type-level attrs (#[rdf(subject, type)]) via RdfTypeAttrs
    //   2. Read field-level attrs (#[rdf(uri)]) via RdfFieldAttrs
    //   3. Build the Polars selection: _subject, _type, predicate columns
    //   4. Pre-compute the XSD type map (predicate URI → XSD IRI) once
    let output_configs: Vec<_> = plan
        .outputs
        .iter()
        .map(|output_spec| {
            let def_id = output_spec.type_def_id;

            // 1. Type-level: rdf_type + subject template
            let type_attrs = RdfTypeAttrs::from_def_id(def_id, ctx.gcx);

            // 2. Field-level: build field_sym → predicate URI map
            let mut field_uris: HashMap<Symbol, String> = HashMap::new();
            if let Some(tm) = ctx.gcx.type_metadata.get(&def_id) {
                for (&field_sym, field_meta) in &tm.field_metadata {
                    let attrs = RdfFieldAttrs::from_field_metadata(field_meta, &interner);
                    if let Some(uri) = attrs.uri {
                        field_uris.insert(field_sym, uri);
                    }
                }
            }

            // 3. Build Polars selection
            let mut selection: Vec<Expr> = Vec::new();

            if let Some(subject_expr) = build_subject_expr(
                def_id, &output_spec.ctor_args, ctx.gcx, ctx.type_index,
            ) {
                selection.push(subject_expr.alias("_subject"));
            }

            if let Some(ref attrs) = type_attrs {
                if let Some(ref rdf_type) = attrs.rdf_type {
                    selection.push(lit(rdf_type.as_str()).alias("_type"));
                }
            }

            for transform_expr in &output_spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(uri) = field_uris.get(&field_sym)
                {
                    // Reference field → build subject IRI from base + identity
                    if let Some(ref_def_id) = field_ref_type(
                        def_id, field_sym, ctx.ir, ctx.type_index,
                    ) {
                        let ref_expr = resolve_identity_expr(
                            ref_def_id, &[inner.as_ref().clone()], ctx.gcx, ctx.type_index,
                        );
                        selection.push(ref_expr.alias(uri));
                        continue;
                    }
                    // Normal field — cast to registered type + alias to predicate URI
                    let prim = field_primitive_type(def_id, field_sym, ctx.gcx);
                    let expr = match prim {
                        Some(p) if p != PrimitiveType::String => {
                            inner.as_ref().clone().cast(p.to_polars_dtype())
                        }
                        _ => inner.as_ref().clone(),
                    };
                    selection.push(expr.alias(uri));
                }
            }

            // 4. XSD type map (predicate URI → XSD IRI)
            let xsd_types = build_xsd_type_map(def_id, &field_uris, ctx.gcx);

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
    output_configs: &[(Vec<Expr>, HashMap<String, &'static str>)],
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
