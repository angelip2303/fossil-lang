//! `Rdf.from_turtle(input, output)` — direct turtle → GraphAr pivot.
//!
//! Bypasses the typed-projection pipeline: parses RDF triples, indexes
//! subjects by their `rdf:type`, and emits one vertex parquet per type
//! with literal-valued predicates pivoted into property columns and
//! IRI-valued predicates pointing to other typed subjects pivoted into
//! edge parquets. The host gets a property-graph view of the source
//! turtle file with zero user-side mapping.
//!
//! Loss-y in v1: takes the first value per (subject, predicate),
//! casts all literals to string, and ignores triples whose subject
//! has no `rdf:type` declaration (subject is untyped).

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::io::{BufReader, Read};

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::OutputKind;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionEffect, FunctionImpl, RuntimeContext};
use fossil_lang::traits::resolver::ResolvedPath;

use oxrdf::{NamedOrBlankNode, Term};
use oxttl::TurtleParser;
use polars::prelude::*;

use super::parquet_writer::{self, EdgeSpec, VertexSpec};
use super::RdfError;

const RDF_TYPE_IRI: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

pub struct RdfFromTurtleFunction;

impl FunctionImpl for RdfFromTurtleFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // (String, String) -> Unit
        let path_ty = ir.string_type();
        let output_ty = ir.unit_type();
        Polytype::mono(ir.fn_type(vec![path_ty, path_ty], output_ty))
    }

    fn effects(&self) -> &[FunctionEffect] {
        &[FunctionEffect::Sink]
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args_iter = args.into_iter();
        let input_path = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                FossilError::evaluation(
                    "Rdf.from_turtle expects (input_path: string, output_path: string)"
                        .to_string(),
                    fossil_lang::ast::Loc::generated(),
                )
            })?;
        let output_path = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                FossilError::evaluation(
                    "Rdf.from_turtle: output_path must be a string literal".to_string(),
                    fossil_lang::ast::Loc::generated(),
                )
            })?;

        let resolved_in = ctx
            .gcx
            .path_resolver
            .resolve(&input_path)
            .map_err(|e| FossilError::evaluation(e, fossil_lang::ast::Loc::generated()))?;
        let resolved_out = ctx
            .gcx
            .path_resolver
            .resolve(&output_path)
            .map_err(|e| FossilError::evaluation(e, fossil_lang::ast::Loc::generated()))?;

        let reader = open_for_read(&resolved_in)?;
        let (vertex_specs, edge_specs) = parse_and_pivot(reader)
            .map_err(|e| RdfError::Write(format!("turtle pivot: {e}")))?;

        let manifest =
            parquet_writer::materialize_frames(vertex_specs, edge_specs, &resolved_out)?;

        ctx.register_output(
            OutputKind::RdfParquet,
            resolved_out.to_str().to_string(),
            Some(manifest),
        );
        Ok(Value::Unit)
    }
}

/// Open a resolved path for streaming reads — local files via `File`,
/// cloud objects downloaded eagerly into memory via Polars' object_store
/// adapter so credentials/auth in `CloudOptions` apply.
fn open_for_read(resolved: &ResolvedPath) -> Result<Box<dyn Read + Send>, FossilError> {
    match resolved.pl_path().as_ref() {
        PlPathRef::Local(p) => {
            let f = std::fs::File::open(p).map_err(|e| {
                FossilError::evaluation(
                    format!("open {}: {e}", p.display()),
                    fossil_lang::ast::Loc::generated(),
                )
            })?;
            Ok(Box::new(f))
        }
        _ => {
            let bytes = download_cloud(resolved)?;
            Ok(Box::new(std::io::Cursor::new(bytes)))
        }
    }
}

/// Pull a cloud object's bytes via Polars' built-in object_store wiring.
/// Reuses the same `CloudOptions` that travel with `ResolvedPath`, so AWS /
/// Azure / GCS credentials configured by the host (keasy cloud-accounts)
/// apply transparently.
fn download_cloud(resolved: &ResolvedPath) -> Result<Vec<u8>, FossilError> {
    use polars::io::cloud::{build_object_store, object_path_from_str};
    use polars::io::pl_async::get_runtime;

    let plpath = resolved.pl_path().clone();
    let options = resolved.cloud_options().cloned();

    let runtime = get_runtime();
    runtime
        .block_on(async move {
            let (loc, store) =
                build_object_store(plpath.as_ref(), options.as_ref(), false).await?;
            let path = object_path_from_str(&loc.prefix)?;
            let meta = store.head(&path).await?;
            let bytes = store
                .get_range(&path, 0..meta.size as usize)
                .await?;
            PolarsResult::Ok(bytes.to_vec())
        })
        .map_err(|e: PolarsError| {
            FossilError::evaluation(
                format!("cloud download: {e}"),
                fossil_lang::ast::Loc::generated(),
            )
        })
}

/// Parse a turtle stream and produce VertexSpec/EdgeSpec frames partitioned
/// by each subject's `rdf:type`.
fn parse_and_pivot<R: Read>(reader: R) -> Result<(Vec<VertexSpec>, Vec<EdgeSpec>), String> {
    let parser = TurtleParser::new().for_reader(BufReader::new(reader));

    // Pass 1: collect everything in memory, separate type declarations.
    let mut subject_type: HashMap<String, String> = HashMap::new();
    let mut other_triples: Vec<(String, String, Term)> = Vec::new();

    for triple in parser {
        let triple = triple.map_err(|e| format!("turtle parse: {e}"))?;
        let Some(subj) = subject_id(&triple.subject) else {
            continue;
        };
        let pred = triple.predicate.as_str().to_string();
        if pred == RDF_TYPE_IRI {
            if let Term::NamedNode(n) = &triple.object {
                subject_type.insert(subj, n.as_str().to_string());
            }
            // ignore non-NamedNode rdf:type values
        } else {
            other_triples.push((subj, pred, triple.object));
        }
    }

    // Index subjects per type, sorted for deterministic IDs.
    let mut subjects_by_type: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (s, t) in &subject_type {
        subjects_by_type
            .entry(t.clone())
            .or_default()
            .insert(s.clone());
    }

    // Per-type id maps (subject_iri → row index in sorted order).
    let mut id_by_type: HashMap<String, HashMap<String, u64>> = HashMap::new();
    for (t, subs) in &subjects_by_type {
        let map: HashMap<String, u64> = subs
            .iter()
            .enumerate()
            .map(|(i, s)| (s.clone(), i as u64))
            .collect();
        id_by_type.insert(t.clone(), map);
    }

    // Pass 2: bucket properties (literal/iri-untyped → property column) and
    // edges (iri-typed → edge spec). First value wins per (subject, predicate).
    type PropMap = HashMap<String, HashMap<String, HashMap<String, String>>>;
    // type_iri → subject_iri → pred_local → first value
    let mut props: PropMap = HashMap::new();
    // type_iri → ordered set of pred_iris seen (drives column order)
    let mut props_pred_iris: HashMap<String, BTreeSet<String>> = HashMap::new();

    type EdgeMap = HashMap<(String, String, String), Vec<(String, String)>>;
    // (src_type, pred_iri, dst_type) → [(src_iri, dst_iri)]
    let mut edges: EdgeMap = HashMap::new();

    for (subj, pred, obj) in other_triples {
        let Some(s_type) = subject_type.get(&subj).cloned() else {
            continue;
        };

        match &obj {
            Term::NamedNode(n) => {
                let obj_iri = n.as_str().to_string();
                if let Some(o_type) = subject_type.get(&obj_iri).cloned() {
                    edges
                        .entry((s_type, pred, o_type))
                        .or_default()
                        .push((subj, obj_iri));
                } else {
                    let local = local_name(&pred);
                    props_pred_iris.entry(s_type.clone()).or_default().insert(pred);
                    props
                        .entry(s_type)
                        .or_default()
                        .entry(subj)
                        .or_default()
                        .entry(local)
                        .or_insert(obj_iri);
                }
            }
            Term::BlankNode(b) => {
                let local = local_name(&pred);
                props_pred_iris.entry(s_type.clone()).or_default().insert(pred);
                props
                    .entry(s_type)
                    .or_default()
                    .entry(subj)
                    .or_default()
                    .entry(local)
                    .or_insert_with(|| format!("_:{}", b.as_str()));
            }
            Term::Literal(l) => {
                let local = local_name(&pred);
                props_pred_iris.entry(s_type.clone()).or_default().insert(pred);
                props
                    .entry(s_type)
                    .or_default()
                    .entry(subj)
                    .or_default()
                    .entry(local)
                    .or_insert_with(|| l.value().to_string());
            }
        }
    }

    // Build VertexSpec per type.
    let mut vertex_specs = Vec::with_capacity(subjects_by_type.len());
    for (type_iri, subs) in &subjects_by_type {
        let type_local = local_name(type_iri);
        let pred_iris: Vec<String> = props_pred_iris
            .get(type_iri)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect();
        let pred_locals: Vec<String> = pred_iris.iter().map(|p| local_name(p)).collect();

        let mut ids: Vec<u64> = Vec::with_capacity(subs.len());
        let mut subject_col: Vec<String> = Vec::with_capacity(subs.len());
        let mut prop_cols: Vec<Vec<Option<String>>> = pred_locals
            .iter()
            .map(|_| Vec::with_capacity(subs.len()))
            .collect();

        let empty: HashMap<String, String> = HashMap::new();
        let type_props = props.get(type_iri);
        for (i, s) in subs.iter().enumerate() {
            ids.push(i as u64);
            subject_col.push(s.clone());
            let subj_data = type_props.and_then(|m| m.get(s)).unwrap_or(&empty);
            for (j, local) in pred_locals.iter().enumerate() {
                prop_cols[j].push(subj_data.get(local).cloned());
            }
        }

        let mut columns: Vec<Column> = Vec::with_capacity(2 + pred_locals.len());
        columns.push(Column::new(PlSmallStr::from("_id"), &ids));
        columns.push(Column::new(PlSmallStr::from("subject"), &subject_col));
        for (j, local) in pred_locals.iter().enumerate() {
            columns.push(Column::new(
                PlSmallStr::from(local.as_str()),
                &prop_cols[j],
            ));
        }

        let df = DataFrame::new(columns)
            .map_err(|e| format!("vertex frame for {type_local}: {e}"))?;

        let mut column_iris = HashMap::new();
        for (local, iri) in pred_locals.iter().zip(pred_iris.iter()) {
            column_iris.insert(local.clone(), iri.clone());
        }

        vertex_specs.push(VertexSpec {
            name: type_local,
            iri: type_iri.clone(),
            frame: df.lazy(),
            column_iris,
        });
    }

    // Build EdgeSpec per (src_type, predicate, dst_type).
    let mut edge_specs = Vec::with_capacity(edges.len());
    for ((src_type, pred_iri, dst_type), pairs) in edges {
        let src_index = id_by_type.get(&src_type);
        let dst_index = id_by_type.get(&dst_type);
        let (Some(src_index), Some(dst_index)) = (src_index, dst_index) else {
            continue;
        };

        let mut source_ids: Vec<u64> = Vec::with_capacity(pairs.len());
        let mut target_ids: Vec<u64> = Vec::with_capacity(pairs.len());
        for (s, t) in pairs {
            if let (Some(&sid), Some(&did)) = (src_index.get(&s), dst_index.get(&t)) {
                source_ids.push(sid);
                target_ids.push(did);
            }
        }

        let df = DataFrame::new(vec![
            Column::new(PlSmallStr::from("source"), &source_ids),
            Column::new(PlSmallStr::from("target"), &target_ids),
        ])
        .map_err(|e| format!("edge frame {src_type}_{pred_iri}_{dst_type}: {e}"))?;

        edge_specs.push(EdgeSpec {
            label: local_name(&pred_iri),
            iri: pred_iri,
            source_type: local_name(&src_type),
            target_type: local_name(&dst_type),
            frame: df.lazy(),
        });
    }

    Ok((vertex_specs, edge_specs))
}

fn subject_id(s: &NamedOrBlankNode) -> Option<String> {
    match s {
        NamedOrBlankNode::NamedNode(n) => Some(n.as_str().to_string()),
        NamedOrBlankNode::BlankNode(b) => Some(format!("_:{}", b.as_str())),
    }
}

/// Local name of an IRI: substring after the last `#` or `/`.
fn local_name(iri: &str) -> String {
    iri.rsplit_once('#')
        .or_else(|| iri.rsplit_once('/'))
        .map(|(_, name)| name)
        .unwrap_or(iri)
        .to_string()
}
