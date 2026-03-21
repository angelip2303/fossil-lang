//! GraphAr-compatible property graph materializer.
//!
//! Two entry points:
//!
//! - [`materialize_frames`] — generic: takes pre-built `VertexSpec`/`EdgeSpec` LazyFrames.
//!   Used by any caller that already has structured vertex/edge data (e.g. DCAT catalogs).
//!
//! - [`materialize`] — Fossil-specific: takes a denormalized source `LazyFrame` +
//!   `OutputConfig` array.  Handles DuckDB dedup + edge joins internally.
//!
//! Both share `compute_type_manifest_from_spec` for stats computation.
//!
//! Output layout (GraphAr gar/v1):
//! ```text
//! dataset/
//! ├── graph.yml
//! ├── {type}.vertex.yml
//! ├── {src}_{edge}_{dst}.edge.yml
//! ├── vertex/{type}.parquet                    # _id | subject | properties...
//! └── edge/{src}_{edge}_{dst}/
//!     ├── by_source.parquet                    # source | target (CSR)
//!     └── by_target.parquet                    # source | target (CSC)
//! ```

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use polars::prelude::sync_on_close::SyncOnCloseType;
use polars::prelude::*;

use fossil_lang::error::FossilError;
use fossil_lang::runtime::executor::{ColumnStat, DataManifest, EdgeManifest, TypeManifest};
use fossil_lang::traits::resolver::ResolvedPath;

use super::OutputConfig;
use super::RdfError;

// ── Public types ────────────────────────────────────────────────────────

/// Vertex type specification — lazy frame ready for parquet sink.
///
/// The `frame` must produce columns: `_id` (u64), `subject` (string), plus
/// zero or more property columns.
pub struct VertexSpec {
    /// Directory/type name (e.g. `"person"`, `"Dataset"`).
    pub name: String,
    /// Full RDF type IRI.  May be empty for non-RDF graphs.
    pub iri: String,
    /// Lazy frame producing `_id | subject | properties…`
    pub frame: LazyFrame,
    /// Optional column → IRI mapping for `ColumnStat.iri`.
    pub column_iris: HashMap<String, String>,
}

/// Edge type specification — lazy frame with source/target vertex IDs.
pub struct EdgeSpec {
    /// Short edge label (e.g. `"knows"`, `"dataset"`).
    pub label: String,
    /// Full predicate IRI.
    pub iri: String,
    /// Source vertex type name.
    pub source_type: String,
    /// Target vertex type name.
    pub target_type: String,
    /// Lazy frame producing `source | target` (u64 IDs).
    pub frame: LazyFrame,
}

// ── Constants & helpers ─────────────────────────────────────────────────

const VERTEX_CHUNK_SIZE: usize = 262_144;

const SINK_OPTIONS: SinkOptions = SinkOptions {
    mkdir: true,
    maintain_order: true,
    sync_on_close: SyncOnCloseType::None,
};

fn parquet_options() -> ParquetWriteOptions {
    ParquetWriteOptions {
        row_group_size: Some(VERTEX_CHUNK_SIZE),
        ..Default::default()
    }
}

fn polars_write_err(e: PolarsError) -> RdfError {
    RdfError::Write(e.to_string())
}

fn path_to_str(path: &Path) -> Result<&str, RdfError> {
    path.to_str()
        .ok_or_else(|| RdfError::Write(format!("non-UTF-8 path: {}", path.display())))
}

fn scan_local(path: &Path) -> Result<LazyFrame, RdfError> {
    LazyFrame::scan_parquet(PlPath::from_str(path_to_str(path)?), ScanArgsParquet::default())
        .map_err(polars_write_err)
}

fn sink_local(lf: LazyFrame, path: &Path) -> Result<(), FossilError> {
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    lf.sink_parquet(
        SinkTarget::Path(PlPath::from_str(path_to_str(path)?)),
        parquet_options(),
        None,
        SINK_OPTIONS,
    )
    .map_err(polars_write_err)?
    .collect()
    .map_err(polars_write_err)?;
    Ok(())
}

// ── Cloud I/O ───────────────────────────────────────────────────────────

struct GraphStore {
    root: ResolvedPath,
}

impl GraphStore {
    fn sink(&self, lf: LazyFrame, rel: &str) -> Result<(), FossilError> {
        let sub = self.root.join(rel);
        lf.sink_parquet(
            SinkTarget::Path(sub.pl_path().clone()),
            parquet_options(),
            sub.cloud_options().cloned(),
            SINK_OPTIONS,
        )
        .map_err(polars_write_err)?
        .collect()
        .map_err(polars_write_err)?;
        Ok(())
    }
}

fn upload(store: &GraphStore, local: &Path, rel: &str) -> Result<(), FossilError> {
    store.sink(scan_local(local)?, rel)
}

// ── Temp file management ────────────────────────────────────────────────

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

fn local_temp(prefix: &str, name: &str) -> PathBuf {
    let id = std::process::id();
    let seq = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    std::env::temp_dir().join(format!(
        "fossil_{prefix}_{}_{id}_{seq}.parquet",
        name.replace('/', "_")
    ))
}

struct TempGuard {
    paths: Vec<PathBuf>,
}

impl TempGuard {
    fn track(&mut self, path: PathBuf) -> PathBuf {
        self.paths.push(path.clone());
        path
    }
}

impl Drop for TempGuard {
    fn drop(&mut self) {
        for p in &self.paths {
            let _ = std::fs::remove_file(p);
        }
    }
}

// ── Shared stats computation ────────────────────────────────────────────

/// Infer ColumnStat datatype from the actual Polars column type.
fn polars_dtype_name(dtype: &DataType) -> &'static str {
    match dtype {
        DataType::Int8 | DataType::Int16 | DataType::Int32 | DataType::Int64
        | DataType::UInt8 | DataType::UInt16 | DataType::UInt32 | DataType::UInt64 => "int64",
        DataType::Float32 | DataType::Float64 => "double",
        DataType::Boolean => "boolean",
        DataType::Date => "date",
        _ => "string",
    }
}

/// Compute a `TypeManifest` from a local vertex parquet.
///
/// Shared by both `materialize` (Fossil) and `materialize_frames` (generic).
/// Collects stats (1 row) and samples (5 rows) in two passes over the local parquet.
fn compute_type_manifest_from_spec(
    name: &str,
    iri: &str,
    column_iris: &HashMap<String, String>,
    vertex_rel_path: &str,
    vertex: &LazyFrame,
) -> Result<TypeManifest, FossilError> {
    // Schema via plan analysis (no data scan), then stats + samples via 2 collects
    let schema = vertex.clone().collect_schema().map_err(polars_write_err)?;
    let prop_cols: Vec<(String, DataType)> = schema
        .iter()
        .filter(|(n, _)| n.as_str() != "_id" && n.as_str() != "subject")
        .map(|(n, dt)| (n.to_string(), dt.clone()))
        .collect();

    // Stats + samples in two collects (can't combine: stats=1 row, samples=5 rows)
    let mut stat_exprs: Vec<Expr> = vec![col("_id").count().alias("__entity_count")];
    for (cn, _) in &prop_cols {
        let c = col(PlSmallStr::from(cn.as_str()));
        stat_exprs.push(c.clone().count().alias(PlSmallStr::from(format!("{cn}__count").as_str())));
        stat_exprs.push(c.clone().n_unique().alias(PlSmallStr::from(format!("{cn}__nunique").as_str())));
        stat_exprs.push(c.clone().cast(DataType::String).min().alias(PlSmallStr::from(format!("{cn}__min").as_str())));
        stat_exprs.push(c.cast(DataType::String).max().alias(PlSmallStr::from(format!("{cn}__max").as_str())));
    }
    let stats_df = vertex.clone().select(stat_exprs).collect().map_err(polars_write_err)?;
    let entity_count = extract_u64(&stats_df, "__entity_count");

    // Samples (head 5 per column)
    let sample_exprs: Vec<Expr> = prop_cols
        .iter()
        .map(|(cn, _)| {
            col(PlSmallStr::from(cn.as_str()))
                .cast(DataType::String)
                .drop_nulls()
                .head(Some(5))
                .alias(PlSmallStr::from(cn.as_str()))
        })
        .collect();
    let samples_df = if !sample_exprs.is_empty() {
        vertex.clone().select(sample_exprs).collect().ok()
    } else {
        None
    };

    let columns: Vec<ColumnStat> = prop_cols
        .iter()
        .map(|(cn, dtype)| {
            let col_iri = column_iris.get(cn).cloned().unwrap_or_default();
            let datatype = polars_dtype_name(dtype).to_string();
            let count = extract_u64(&stats_df, &format!("{cn}__count"));
            let n_unique = extract_u64(&stats_df, &format!("{cn}__nunique"));
            let min = extract_string(&stats_df, &format!("{cn}__min"));
            let max = extract_string(&stats_df, &format!("{cn}__max"));
            let samples = samples_df
                .as_ref()
                .and_then(|df| {
                    df.column(cn.as_str()).ok()?.str().ok().map(|ca| {
                        ca.into_iter().filter_map(|v| v.map(String::from)).collect()
                    })
                })
                .unwrap_or_default();
            ColumnStat { name: cn.clone(), iri: col_iri, datatype, count, n_unique, min, max, samples }
        })
        .collect();

    Ok(TypeManifest {
        name: name.to_string(),
        iri: iri.to_string(),
        vertex_file: vertex_rel_path.to_string(),
        entity_count,
        columns,
    })
}

fn extract_u64(df: &DataFrame, col_name: &str) -> u64 {
    df.column(col_name)
        .ok()
        .and_then(|c| {
            c.u32().ok().map(|ca| ca.get(0).unwrap_or(0) as u64)
                .or_else(|| c.u64().ok().map(|ca| ca.get(0).unwrap_or(0)))
        })
        .unwrap_or(0)
}

fn extract_string(df: &DataFrame, col_name: &str) -> Option<String> {
    df.column(col_name).ok()?.str().ok()?.get(0).map(String::from)
}

// ── Generic entry point ─────────────────────────────────────────────────

/// Materialize pre-built vertex and edge LazyFrames as GraphAr parquets.
///
/// Each frame is sunk streaming to a local temp, stats computed, then
/// uploaded to `dest`.  No DuckDB, no dedup — callers provide clean frames.
pub fn materialize_frames(
    vertices: Vec<VertexSpec>,
    edges: Vec<EdgeSpec>,
    dest: &ResolvedPath,
) -> Result<DataManifest, FossilError> {
    let store = GraphStore { root: dest.clone() };
    let mut temps = TempGuard { paths: Vec::new() };

    let mut types = Vec::new();
    let mut edge_manifests = Vec::new();

    for spec in vertices {
        let vertex_rel = format!("vertex/{}.parquet", spec.name);
        let temp = temps.track(local_temp("mf_vtx", &spec.name));

        sink_local(spec.frame, &temp)?;

        let manifest = compute_type_manifest_from_spec(
            &spec.name, &spec.iri, &spec.column_iris,
            &vertex_rel, &scan_local(&temp)?,
        )?;
        types.push(manifest);
        upload(&store, &temp, &vertex_rel)?;
    }

    for spec in edges {
        let edge_dir = format!("{}_{}_{}", spec.source_type, spec.label, spec.target_type);
        let csr_rel = format!("edge/{edge_dir}/by_source.parquet");
        let temp = temps.track(local_temp("mf_edge", &edge_dir));

        sink_local(spec.frame, &temp)?;

        let count = scan_local(&temp)?
            .select([col("source").count().alias("n")])
            .collect()
            .map(|df| extract_u64(&df, "n"))
            .unwrap_or(0);

        if count > 0 {
            upload(&store, &temp, &csr_rel)?;
        }

        edge_manifests.push(EdgeManifest {
            name: spec.label,
            iri: spec.iri,
            source_type: spec.source_type,
            target_type: spec.target_type,
            by_source: csr_rel.clone(),
            by_target: csr_rel, // CSR only — callers needing CSC (e.g. materialize) handle it separately
            count,
        });
    }

    write_yaml_metadata(&store, &types, &edge_manifests)?;
    Ok(DataManifest { types, edges: edge_manifests })
}

// ── Fossil-specific entry point ─────────────────────────────────────────

/// Materialize a denormalized source LazyFrame into GraphAr parquets.
///
/// Handles DuckDB dedup (Phase 0-1) and edge join/sort (Phase 2).
/// Uses [`compute_type_manifest_from_spec`] for stats (shared with
/// [`materialize_frames`]).
pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    resolved: &ResolvedPath,
) -> Result<DataManifest, FossilError> {
    let store = GraphStore { root: resolved.clone() };
    let conn = duckdb::Connection::open_in_memory().map_err(RdfError::from)?;

    let mut types = Vec::new();
    let mut edges = Vec::new();
    let mut local_vertices: HashMap<String, PathBuf> = HashMap::new();
    let mut temps = TempGuard { paths: Vec::new() };

    // ── Phase 0: source → single local materialization (1 cloud read) ──
    let full_local = temps.track(local_temp("full", "source"));
    sink_local(frame.clone().with_row_index("_id", Some(0)), &full_local)?;

    // ── Phase 1: local → per-type vertex + stats + upload ──
    for config in configs {
        let vertex_rel = format!("vertex/{}.parquet", config.type_dir);

        // Polars: select vertex columns from local full (streaming)
        let raw_vertex_lf = scan_local(&full_local)?
            .select(config.selection.clone())
            .rename(["_subject"], ["subject"], true);

        // Sink raw vertex (may have duplicate subjects)
        let raw_vtx = temps.track(local_temp("raw_vtx", &config.type_dir));
        sink_local(raw_vertex_lf, &raw_vtx)?;

        // DuckDB: deduplicate by subject + assign sequential _id
        let deduped_vtx = temps.track(local_temp("vtx", &config.type_dir));
        let raw_str = path_to_str(&raw_vtx)?;
        let deduped_str = path_to_str(&deduped_vtx)?;
        conn.execute_batch(&format!(
            "COPY (
                SELECT row_number() OVER () - 1 AS _id, *
                FROM (SELECT DISTINCT ON (subject) * FROM read_parquet('{raw_str}'))
            ) TO '{deduped_str}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})"
        ))
        .map_err(RdfError::from)?;

        // Stats from deduped vertex (shared function)
        let vertex_lf = scan_local(&deduped_vtx)?;
        let manifest = compute_type_manifest_from_spec(
            &config.type_dir, &config.type_iri, &config.label_to_iri,
            &vertex_rel, &vertex_lf,
        )?;
        types.push(manifest);

        upload(&store, &deduped_vtx, &vertex_rel)?;

        local_vertices.insert(config.type_dir.clone(), deduped_vtx);
    }

    // ── Phase 2: local → edge staging + DuckDB join/sort + upload ──
    for config in configs {
        let src_local = local_vertices
            .get(&config.type_dir)
            .ok_or_else(|| RdfError::Write(format!("missing vertex: {}", config.type_dir)))?;

        for ref_edge in &config.ref_edges {
            let edge_dir = format!(
                "{}_{}_{}",
                config.type_dir, ref_edge.label, ref_edge.target_type_dir
            );
            let tgt_local = local_vertices
                .get(&ref_edge.target_type_dir)
                .ok_or_else(|| {
                    RdfError::Write(format!("missing vertex: {}", ref_edge.target_type_dir))
                })?;

            // Edge staging from local full (no cloud read!)
            let staging = temps.track(local_temp("stg", &edge_dir));
            let staging_lf = scan_local(&full_local)?
                .select([
                    config.subject_expr.clone().alias("src_iri"),
                    ref_edge.expr.clone().alias("tgt_iri"),
                ])
                .filter(col("src_iri").is_not_null().and(col("tgt_iri").is_not_null()));
            sink_local(staging_lf, &staging)?;

            // DuckDB: join + sort
            let edge_out = produce_edges_local(
                &conn, &mut temps, &staging, src_local, tgt_local, &edge_dir,
            )?;

            if edge_out.count > 0 {
                upload(&store, &edge_out.csr, &edge_out.csr_rel)?;
                upload(&store, &edge_out.csc, &edge_out.csc_rel)?;
            }

            edges.push(EdgeManifest {
                name: ref_edge.label.clone(),
                iri: ref_edge.predicate_uri.clone(),
                source_type: config.type_dir.clone(),
                target_type: ref_edge.target_type_dir.clone(),
                by_source: edge_out.csr_rel,
                by_target: edge_out.csc_rel,
                count: edge_out.count,
            });
        }
    }

    write_yaml_metadata(&store, &types, &edges)?;
    Ok(DataManifest { types, edges })
}

// ── DuckDB edge production (Fossil-specific) ────────────────────────────

struct EdgeOutput {
    csr: PathBuf,
    csc: PathBuf,
    csr_rel: String,
    csc_rel: String,
    count: u64,
}

fn produce_edges_local(
    conn: &duckdb::Connection,
    temps: &mut TempGuard,
    staging_local: &Path,
    src_vertex_local: &Path,
    tgt_vertex_local: &Path,
    edge_dir: &str,
) -> Result<EdgeOutput, FossilError> {
    let stg = path_to_str(staging_local)?;
    let src = path_to_str(src_vertex_local)?;
    let tgt = path_to_str(tgt_vertex_local)?;

    conn.execute_batch(&format!(
        "CREATE OR REPLACE TEMP TABLE __edges AS
            SELECT s._id AS source, t._id AS target
            FROM read_parquet('{stg}') e
            JOIN read_parquet('{src}') s ON e.src_iri = s.subject
            JOIN read_parquet('{tgt}') t ON e.tgt_iri = t.subject"
    ))
    .map_err(RdfError::from)?;

    let csr = temps.track(local_temp("csr", edge_dir));
    let csc = temps.track(local_temp("csc", edge_dir));

    conn.execute_batch(&format!(
        "COPY (SELECT source, target FROM __edges ORDER BY source, target)
         TO '{}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})",
        path_to_str(&csr)?
    ))
    .map_err(RdfError::from)?;
    conn.execute_batch(&format!(
        "COPY (SELECT source, target FROM __edges ORDER BY target, source)
         TO '{}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})",
        path_to_str(&csc)?
    ))
    .map_err(RdfError::from)?;

    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(RdfError::from)?;

    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(RdfError::from)?;

    Ok(EdgeOutput {
        csr,
        csc,
        csr_rel: format!("edge/{edge_dir}/by_source.parquet"),
        csc_rel: format!("edge/{edge_dir}/by_target.parquet"),
        count,
    })
}

// ── YAML metadata ───────────────────────────────────────────────────────

fn write_yaml_metadata(
    store: &GraphStore,
    types: &[TypeManifest],
    edges: &[EdgeManifest],
) -> Result<(), FossilError> {
    if store.root.pl_path().is_cloud_url() {
        return Ok(());
    }

    let vertex_ymls: Vec<String> = types.iter().map(|t| format!("{}.vertex.yml", t.name)).collect();
    let edge_ymls: Vec<String> = edges
        .iter()
        .map(|e| format!("{}_{}_{}.edge.yml", e.source_type, e.name, e.target_type))
        .collect();

    std::fs::write(
        store.root.join("graph.yml").to_str(),
        format!(
            "name: dataset\nprefix: ./\nvertices:\n{}\nedges:\n{}\nversion: gar/v1\n",
            vertex_ymls.iter().map(|v| format!("  - {v}")).collect::<Vec<_>>().join("\n"),
            edge_ymls.iter().map(|e| format!("  - {e}")).collect::<Vec<_>>().join("\n"),
        ),
    ).map_err(|e| RdfError::Write(e.to_string()))?;

    for t in types {
        let props: Vec<String> = t.columns.iter().map(|c| {
            format!("      - name: {}\n        data_type: {}\n        is_primary: false", c.name, c.datatype)
        }).collect();
        std::fs::write(
            store.root.join(&format!("{}.vertex.yml", t.name)).to_str(),
            format!(
                "type: {}\nchunk_size: {VERTEX_CHUNK_SIZE}\nprefix: vertex/{}/\nproperty_groups:\n  - file_type: parquet\n    properties:\n      - name: subject\n        data_type: string\n        is_primary: true\n{}\nversion: gar/v1\n",
                t.name, t.name, props.join("\n")
            ),
        ).map_err(|e| RdfError::Write(e.to_string()))?;
    }

    for e in edges {
        std::fs::write(
            store.root.join(&format!("{}_{}_{}.edge.yml", e.source_type, e.name, e.target_type)).to_str(),
            format!(
                "src_type: {src}\nedge_type: {edge}\ndst_type: {dst}\nchunk_size: 4194304\nsrc_chunk_size: {VERTEX_CHUNK_SIZE}\ndst_chunk_size: {VERTEX_CHUNK_SIZE}\ndirected: true\nprefix: edge/{src}_{edge}_{dst}/\nadj_lists:\n  - ordered: true\n    aligned_by: src\n    file_type: parquet\n  - ordered: true\n    aligned_by: dst\n    file_type: parquet\nversion: gar/v1\n",
                src = e.source_type, edge = e.name, dst = e.target_type,
            ),
        ).map_err(|e2| RdfError::Write(e2.to_string()))?;
    }

    Ok(())
}
