pub mod metadata;
pub mod parquet_writer;

use std::collections::{HashMap, HashSet};

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{DefId, Symbol};
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeIndex, TypeKind as IrTypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::OutputKind;
use fossil_lang::runtime::value::{Emission, Value};
use fossil_lang::traits::function::{FunctionEffect, FunctionImpl, RuntimeContext};

use crate::string::template::parse_template;
use metadata::{RdfFieldAttrs, RdfTypeAttrs, build_xsd_type_map, field_primitive_type};

use polars::prelude::*;
use thiserror::Error;

/// RDF-specific errors
#[derive(Debug, Error)]
pub enum RdfError {
    #[error("Rdf function requires input and path")]
    SerializeMissingArgs,
    #[error("Rdf function path must be a string literal")]
    SerializeInvalidFilename,
    #[error("Rdf function expects an Emission")]
    SerializeInvalidInput,

    #[error("Failed to write RDF: {0}")]
    Write(String),
}

impl From<RdfError> for FossilError {
    fn from(err: RdfError) -> Self {
        FossilError::evaluation(err.to_string(), fossil_lang::ast::Loc::generated())
    }
}

/// Output configuration for a single RDF type.
pub struct OutputConfig {
    /// Polars selection expressions producing `_subject`, `_type`, and predicate columns.
    pub selection: Vec<Expr>,
    /// Predicate URI → XSD datatype IRI for typed literals.
    pub xsd_types: HashMap<String, &'static str>,
    /// Predicate URIs whose values are references (URIs/blank nodes, not literals).
    pub ref_predicates: HashSet<String>,
    /// Directory name for this type (e.g., `"wall"`).
    pub type_dir: String,
    /// Full RDF type IRI (e.g., `"http://example.com/bim#Wall"`).
    pub type_iri: String,
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
    let template = RdfTypeAttrs::from_def_id(def_id, gcx).and_then(|a| a.subject);

    match template {
        Some(template) => {
            let param_names = type_index
                .get(def_id)
                .map(|info| &info.ctor_param_names[..])
                .unwrap_or(&[]);
            let parts = parse_template(&template, param_names, args, &gcx.interner);
            concat_str(parts, "", true)
        }
        None if args.len() == 1 => {
            // Single ctor arg without template: use the value directly as subject
            args[0].clone().cast(DataType::String)
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
    let IrTypeKind::Record(fields) = &ir.types.get(info.ty).kind else {
        return None;
    };
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

// ---------------------------------------------------------------------------
// Parquet materialization (HDT-style indices)
// ---------------------------------------------------------------------------

pub struct RdfMaterializeFunction;

impl FunctionImpl for RdfMaterializeFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, String) -> Unit
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        let path_ty = ir.string_type();
        let output_ty = ir.unit_type();
        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty, path_ty], output_ty))
    }

    fn effects(&self) -> &[FunctionEffect] {
        &[FunctionEffect::Sink]
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args_iter = args.into_iter();

        let input_value = args_iter.next().ok_or(RdfError::SerializeMissingArgs)?;

        let base_path = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or(RdfError::SerializeInvalidFilename)?;

        match input_value {
            Value::Emission(emission) if !emission.specs.is_empty() => {
                let configs = build_output_configs(&emission, ctx);
                let resolved = ctx.gcx.path_resolver.resolve(&base_path)
                    .map_err(|e| FossilError::evaluation(e, fossil_lang::ast::Loc::generated()))?;
                parquet_writer::materialize(&emission.frame, &configs, &resolved.url, resolved.cloud_options)?;
                ctx.register_output(OutputKind::RdfParquet, resolved.url);
                Ok(Value::Unit)
            }
            _ => Err(RdfError::SerializeInvalidInput.into()),
        }
    }
}

/// Build output configs from an Emission's specs.
fn build_output_configs(emission: &Emission, ctx: &RuntimeContext) -> Vec<OutputConfig> {
    let interner = ctx.gcx.interner.clone();

    emission.specs
        .iter()
        .map(|spec| {
            let def_id = spec.type_def_id;

            // Type-level attrs
            let type_attrs = RdfTypeAttrs::from_def_id(def_id, ctx.gcx);

            // Field-level: field_sym → predicate URI
            let mut field_uris: HashMap<Symbol, String> = HashMap::new();
            if let Some(tm) = ctx.gcx.type_metadata.get(&def_id) {
                for (&field_sym, field_meta) in &tm.field_metadata {
                    let attrs = RdfFieldAttrs::from_field_metadata(field_meta, &interner);
                    if let Some(uri) = attrs.uri {
                        field_uris.insert(field_sym, uri);
                    }
                }
            }

            // Build selection + track reference predicates
            let mut selection: Vec<Expr> = Vec::new();
            let mut ref_predicates: HashSet<String> = HashSet::new();

            if let Some(subject_expr) =
                build_subject_expr(def_id, &spec.ctor_args, ctx.gcx, ctx.type_index)
            {
                selection.push(subject_expr.alias("_subject"));
            }

            if let Some(ref attrs) = type_attrs {
                if let Some(ref rdf_type) = attrs.rdf_type {
                    selection.push(lit(rdf_type.as_str()).alias("_type"));
                }
            }

            for transform_expr in &spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(uri) = field_uris.get(&field_sym)
                {
                    // Reference field → URI/blank node object
                    if let Some(ref_def_id) =
                        field_ref_type(def_id, field_sym, ctx.ir, ctx.type_index)
                    {
                        ref_predicates.insert(uri.clone());
                        let ref_expr = resolve_identity_expr(
                            ref_def_id,
                            &[inner.as_ref().clone()],
                            ctx.gcx,
                            ctx.type_index,
                        );
                        selection.push(ref_expr.alias(uri));
                        continue;
                    }
                    // Normal field
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

            let xsd_types = build_xsd_type_map(def_id, &field_uris, ctx.gcx);

            // Derive type directory and IRI
            let type_name = interner.resolve(ctx.gcx.definitions.get(def_id).name);
            let type_dir = type_name.to_lowercase();
            let type_iri = type_attrs
                .as_ref()
                .and_then(|a| a.rdf_type.clone())
                .unwrap_or_else(|| type_name.to_string());

            OutputConfig {
                selection,
                xsd_types,
                ref_predicates,
                type_dir,
                type_iri,
            }
        })
        .collect()
}
