pub mod from_turtle;
pub mod metadata;
pub mod parquet_writer;

// Re-export generic materializer types for external callers (e.g. keasy catalog).
pub use parquet_writer::{EdgeSpec, VertexSpec, materialize_frames};

pub use from_turtle::RdfFromTurtleFunction;

use std::collections::HashMap;

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{DefId, Symbol};
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeIndex, TypeKind as IrTypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::OutputKind;
use fossil_lang::runtime::value::{Emission, Value};
use fossil_lang::traits::function::{FunctionEffect, FunctionImpl, RuntimeContext};

use crate::string::template::parse_template;
use metadata::{RdfFieldAttrs, RdfTypeAttrs, field_primitive_type};

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

    #[error("vertex dedup failed for type '{type_dir}': {reason}")]
    VertexDedupFailed { type_dir: String, reason: String },

    #[error("edge join failed for '{edge}': {reason}")]
    EdgeJoinFailed { edge: String, reason: String },

    #[error("duckdb: {0}")]
    DuckDb(String),
}

impl From<duckdb::Error> for RdfError {
    fn from(e: duckdb::Error) -> Self {
        RdfError::DuckDb(e.to_string())
    }
}

impl From<RdfError> for FossilError {
    fn from(err: RdfError) -> Self {
        FossilError::evaluation(err.to_string(), fossil_lang::ast::Loc::generated())
    }
}

/// A reference (edge) from this type to another type.
pub struct RefEdge {
    /// Predicate URI (e.g. "http://schema.org/knows").
    pub predicate_uri: String,
    /// Short label for the edge (local name of predicate IRI).
    pub label: String,
    /// Target type directory name (e.g. "person").
    pub target_type_dir: String,
    /// The Polars expression that produces the ref value (target IRI).
    pub expr: Expr,
}

/// Output configuration for a single vertex type.
pub struct OutputConfig {
    /// Polars selection expressions producing `_subject` and scalar property columns (no refs).
    pub selection: Vec<Expr>,
    /// The expression that produces the subject IRI (for edge generation).
    pub subject_expr: Expr,
    /// Short label → full predicate IRI for scalar columns.
    pub label_to_iri: HashMap<String, String>,
    /// Reference edges from this type to other types.
    pub ref_edges: Vec<RefEdge>,
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
                let manifest = parquet_writer::materialize(&emission.frame, &configs, &resolved)?;
                ctx.register_output(OutputKind::RdfParquet, resolved.to_str().to_string(), Some(manifest));
                Ok(Value::Unit)
            }
            _ => Err(RdfError::SerializeInvalidInput.into()),
        }
    }
}

/// Extract the local name from a URI (part after last `#` or `/`).
fn short_label(uri: &str) -> String {
    uri.rsplit_once('#')
        .or_else(|| uri.rsplit_once('/'))
        .map(|(_, name)| name)
        .unwrap_or(uri)
        .to_string()
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

            // Build selection (scalar only) + ref edges (separate)
            let mut selection: Vec<Expr> = Vec::new();
            let mut ref_edges: Vec<RefEdge> = Vec::new();
            let mut label_to_iri: HashMap<String, String> = HashMap::new();

            let subject_expr = build_subject_expr(def_id, &spec.ctor_args, ctx.gcx, ctx.type_index)
                .unwrap_or_else(|| lit("").alias("_subject"));
            selection.push(subject_expr.clone().alias("_subject"));

            for transform_expr in &spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(uri) = field_uris.get(&field_sym)
                {
                    // Reference field → goes to ref_edges, NOT selection
                    if let Some(ref_def_id) =
                        field_ref_type(def_id, field_sym, ctx.ir, ctx.type_index)
                    {
                        let ref_expr = resolve_identity_expr(
                            ref_def_id,
                            &[inner.as_ref().clone()],
                            ctx.gcx,
                            ctx.type_index,
                        );
                        let ref_type_name = interner.resolve(
                            ctx.gcx.definitions.get(ref_def_id).name,
                        );
                        ref_edges.push(RefEdge {
                            predicate_uri: uri.clone(),
                            label: short_label(uri),
                            target_type_dir: ref_type_name.to_lowercase(),
                            expr: ref_expr,
                        });
                        continue;
                    }
                    // Scalar field → selection with short label alias
                    let label = short_label(uri);
                    label_to_iri.insert(label.clone(), uri.clone());
                    let prim = field_primitive_type(def_id, field_sym, ctx.gcx);
                    let expr = match prim {
                        Some(p) if p != PrimitiveType::String => {
                            inner.as_ref().clone().cast(p.to_polars_dtype())
                        }
                        _ => inner.as_ref().clone(),
                    };
                    selection.push(expr.alias(PlSmallStr::from(label.as_str())));
                }
            }

            // Derive type directory and IRI
            let type_name = interner.resolve(ctx.gcx.definitions.get(def_id).name);
            let type_dir = type_name.to_lowercase();
            let type_iri = type_attrs
                .as_ref()
                .and_then(|a| a.rdf_type.clone())
                .unwrap_or_else(|| type_name.to_string());

            OutputConfig {
                selection,
                subject_expr,
                label_to_iri,
                ref_edges,
                type_dir,
                type_iri,
            }
        })
        .collect()
}
