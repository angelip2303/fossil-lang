pub mod attrs;
pub mod template;

use std::collections::HashMap;

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::{DefId, Symbol};
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeIndex, TypeKind as IrTypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{Emission, Value};
use fossil_lang::traits::function::{FunctionEffect, FunctionImpl, RuntimeContext};

use fossil_rdf::dest::GraphDest;
use fossil_rdf::error::RdfError;
use fossil_rdf::mapping::{EdgeMapping, VertexMapping};

use attrs::{RdfFieldAttrs, RdfTypeAttrs, field_primitive_type};
use template::parse_template;

use polars::prelude::*;

// ── RdfMaterializeFunction ──────────────────────────────────────────────

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

    fn call(&self, args: Vec<Value>, _type_args: &[DefId], ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args_iter = args.into_iter();

        let input_value = args_iter.next()
            .ok_or_else(|| rdf_err(RdfError::MissingArguments))?;

        let base_path = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| rdf_err(RdfError::InvalidPath))?;

        match input_value {
            Value::Emission(emission) if !emission.specs.is_empty() => {
                let mappings = build_mappings(&emission, ctx);
                let resolved = ctx.gcx.path_resolver.resolve(&base_path)
                    .map_err(|e| FossilError::evaluation(e, fossil_lang::ast::Loc::generated()))?;
                let dest = to_graph_dest(&resolved);
                let manifest = fossil_rdf::materialize(&emission.frame, &mappings, &dest)
                    .map_err(rdf_err)?;
                ctx.register_output(
                    fossil_rdf::OUTPUT_KIND,
                    resolved.to_str().to_string(),
                    serde_json::to_value(&manifest).unwrap_or_default(),
                );
                Ok(Value::Unit)
            }
            _ => Err(rdf_err(RdfError::ExpectedEmission)),
        }
    }
}

/// Convert a fossil-lang ResolvedPath to a fossil-rdf GraphDest.
fn to_graph_dest(resolved: &fossil_lang::traits::resolver::ResolvedPath) -> GraphDest {
    match resolved.cloud_options() {
        Some(opts) => GraphDest::cloud(resolved.to_str(), opts.clone()),
        None => GraphDest::local(resolved.to_str()),
    }
}

// ── Mapping builders ────────────────────────────────────────────────────

/// Build a blank node expression: `_:{TypeName}_{arg0}_{arg1}_...`
fn build_bnode_expr(def_id: DefId, args: &[Expr], gcx: &GlobalContext) -> Expr {
    let type_name = gcx.interner.resolve(gcx.definitions.get(def_id).name);
    let mut parts = vec![lit(format!("_:{type_name}"))];
    parts.extend(args.iter().map(|a| a.clone().cast(DataType::String)));
    concat_str(parts, "_", true)
}

/// Resolve a subject identity expression for a type.
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
            args[0].clone().cast(DataType::String)
        }
        None => build_bnode_expr(def_id, args, gcx),
    }
}

/// Build the subject expression for a type's constructor args.
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

/// Extract the local name from a URI (part after last `#` or `/`).
fn short_label(uri: &str) -> String {
    uri.rsplit_once('#')
        .or_else(|| uri.rsplit_once('/'))
        .map(|(_, name)| name)
        .unwrap_or(uri)
        .to_string()
}

/// Build VertexMappings from an Emission's specs.
fn build_mappings(emission: &Emission, ctx: &RuntimeContext) -> Vec<VertexMapping> {
    let interner = ctx.gcx.interner.clone();

    emission.specs
        .iter()
        .map(|spec| {
            let def_id = spec.type_def_id;

            let type_attrs = RdfTypeAttrs::from_def_id(def_id, ctx.gcx);

            let mut field_uris: HashMap<Symbol, String> = HashMap::new();
            if let Some(tm) = ctx.gcx.type_metadata.get(&def_id) {
                for (&field_sym, field_meta) in &tm.field_metadata {
                    let attrs = RdfFieldAttrs::from_field_metadata(field_meta, &interner);
                    if let Some(uri) = attrs.uri {
                        field_uris.insert(field_sym, uri);
                    }
                }
            }

            let mut selection: Vec<Expr> = Vec::new();
            let mut edges: Vec<EdgeMapping> = Vec::new();
            let mut label_to_iri: HashMap<String, String> = HashMap::new();

            let subject_expr = build_subject_expr(def_id, &spec.ctor_args, ctx.gcx, ctx.type_index)
                .unwrap_or_else(|| lit("").alias("_subject"));
            selection.push(subject_expr.clone().alias("_subject"));

            for transform_expr in &spec.select_exprs {
                if let Expr::Alias(inner, field_name) = transform_expr
                    && let Some(field_sym) = interner.lookup(field_name)
                    && let Some(uri) = field_uris.get(&field_sym)
                {
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
                        edges.push(EdgeMapping {
                            predicate_uri: uri.clone(),
                            label: short_label(uri),
                            target_type_dir: ref_type_name.to_lowercase(),
                            expr: ref_expr,
                        });
                        continue;
                    }
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

            let type_name = interner.resolve(ctx.gcx.definitions.get(def_id).name);
            let type_dir = type_name.to_lowercase();
            let type_iri = type_attrs
                .as_ref()
                .and_then(|a| a.rdf_type.clone())
                .unwrap_or_else(|| type_name.to_string());

            VertexMapping {
                selection,
                subject_expr,
                label_to_iri,
                edges,
                type_dir,
                type_iri,
            }
        })
        .collect()
}

fn rdf_err(err: RdfError) -> FossilError {
    FossilError::evaluation(err.to_string(), fossil_lang::ast::Loc::generated())
}
