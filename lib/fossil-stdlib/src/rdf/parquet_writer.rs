//! GraphAr-compatible property graph materializer.
//!
//! Architecture: cloud I/O at the edges, pure local compute in the center.
//!
//!   Phase 0 — Polars: source (cloud) → single local materialization
//!   Phase 1 — Polars: local → per-type vertex + stats + upload
//!   Phase 2 — Polars + DuckDB: local → edge staging + join/sort + upload
//!
//!   Polars  = all cloud I/O + expression evaluation + stats
//!   DuckDB  = join + CSR/CSC sort (disk-spilling), pure local
//!   /tmp    = materialization points (Spark `persist(DISK_ONLY)`)
//!
//! Source is read ONCE. DuckDB never touches cloud storage.
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
//!     ├── by_source_offsets.parquet            # vertex_id | start_offset
//!     ├── by_target.parquet                    # source | target (CSC)
//!     └── by_target_offsets.parquet            # vertex_id | start_offset
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

// ── Cloud I/O (Polars only) ──

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

// ── Temp file management ──

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

// ── Main entry point ──

pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    resolved: &ResolvedPath,
) -> Result<DataManifest, FossilError> {
    let store = GraphStore { root: resolved.clone() };
    let conn = duckdb::Connection::open_in_memory().map_err(RdfError::from)?;
    let is_cloud = resolved.pl_path().is_cloud_url();

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

        // Select vertex columns + _id from the local full file (no cloud read)
        let mut vertex_exprs = config.selection.clone();
        vertex_exprs.push(col("_id"));
        let vertex_lf = scan_local(&full_local)?
            .select(vertex_exprs)
            .rename(["_subject"], ["subject"], true);

        if is_cloud {
            let vtx_local = temps.track(local_temp("vtx", &config.type_dir));
            sink_local(vertex_lf, &vtx_local)?;

            let manifest = compute_type_manifest(config, &vertex_rel, &scan_local(&vtx_local)?)?;
            types.push(manifest);

            upload(&store, &vtx_local, &vertex_rel)?;
            local_vertices.insert(config.type_dir.clone(), vtx_local);
        } else {
            store.sink(vertex_lf.clone(), &vertex_rel)?;
            let dest = PathBuf::from(resolved.join(&vertex_rel).to_str().to_string());

            let manifest = compute_type_manifest(config, &vertex_rel, &scan_local(&dest)?)?;
            types.push(manifest);
            local_vertices.insert(config.type_dir.clone(), dest);
        }
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

            // Edge staging from local full file (no cloud read!)
            let staging = temps.track(local_temp("stg", &edge_dir));
            let staging_lf = scan_local(&full_local)?
                .select([
                    config.subject_expr.clone().alias("src_iri"),
                    ref_edge.expr.clone().alias("tgt_iri"),
                ])
                .filter(col("src_iri").is_not_null().and(col("tgt_iri").is_not_null()));
            sink_local(staging_lf, &staging)?;

            // DuckDB: join + sort — all local
            let edge_out = produce_edges_local(
                &conn,
                &mut temps,
                &staging,
                src_local,
                tgt_local,
                &edge_dir,
            )?;

            // Upload edges to cloud (skip if 0 edges)
            if is_cloud && edge_out.count > 0 {
                upload(&store, &edge_out.csr, &edge_out.csr_rel)?;
                upload(&store, &edge_out.csc, &edge_out.csc_rel)?;
                upload(&store, &edge_out.csr_offsets, &edge_out.csr_offsets_rel)?;
                upload(&store, &edge_out.csc_offsets, &edge_out.csc_offsets_rel)?;
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

// ── DuckDB edge production (pure local) ──

struct EdgeOutput {
    csr: PathBuf,
    csc: PathBuf,
    csr_offsets: PathBuf,
    csc_offsets: PathBuf,
    csr_rel: String,
    csc_rel: String,
    csr_offsets_rel: String,
    csc_offsets_rel: String,
    count: u64,
}

fn write_sorted_edges(
    conn: &duckdb::Connection,
    primary: &str,
    secondary: &str,
    sorted_path: &str,
    offsets_path: &str,
) -> Result<(), RdfError> {
    conn.execute_batch(&format!(
        "COPY (SELECT source, target FROM __edges ORDER BY {primary}, {secondary})
         TO '{sorted_path}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})"
    ))?;
    conn.execute_batch(&format!(
        "COPY (
            WITH ranked AS (
                SELECT {primary} AS vertex_id,
                       ROW_NUMBER() OVER (ORDER BY {primary}, {secondary}) - 1 AS pos
                FROM __edges
            )
            SELECT vertex_id, MIN(pos) AS start_offset
            FROM ranked GROUP BY vertex_id ORDER BY vertex_id
        ) TO '{offsets_path}' (FORMAT PARQUET)"
    ))?;
    Ok(())
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
    let csr_off = temps.track(local_temp("csr_off", edge_dir));
    let csc_off = temps.track(local_temp("csc_off", edge_dir));

    write_sorted_edges(conn, "source", "target", path_to_str(&csr)?, path_to_str(&csr_off)?)?;
    write_sorted_edges(conn, "target", "source", path_to_str(&csc)?, path_to_str(&csc_off)?)?;

    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(RdfError::from)?;

    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(RdfError::from)?;

    Ok(EdgeOutput {
        csr,
        csc,
        csr_offsets: csr_off,
        csc_offsets: csc_off,
        csr_rel: format!("edge/{edge_dir}/by_source.parquet"),
        csc_rel: format!("edge/{edge_dir}/by_target.parquet"),
        csr_offsets_rel: format!("edge/{edge_dir}/by_source_offsets.parquet"),
        csc_offsets_rel: format!("edge/{edge_dir}/by_target_offsets.parquet"),
        count,
    })
}

// ── Stats ──

fn xsd_to_datatype_name(xsd: Option<&str>) -> &'static str {
    match xsd {
        Some(s) if s.ends_with("#integer") || s.ends_with("#int") || s.ends_with("#long") => "int64",
        Some(s) if s.ends_with("#float") || s.ends_with("#double") || s.ends_with("#decimal") => "double",
        Some(s) if s.ends_with("#boolean") => "boolean",
        Some(s) if s.ends_with("#date") => "date",
        _ => "string",
    }
}

fn compute_type_manifest(
    config: &OutputConfig,
    vertex_rel_path: &str,
    vertex: &LazyFrame,
) -> Result<TypeManifest, FossilError> {
    let label_to_iri = &config.label_to_iri;
    let prop_cols: Vec<&String> = label_to_iri.keys().collect();

    let mut stat_exprs: Vec<Expr> = vec![col("_id").count().alias("__entity_count")];
    for name in &prop_cols {
        let c = col(PlSmallStr::from(name.as_str()));
        stat_exprs.push(c.clone().count().alias(PlSmallStr::from(format!("{name}__count").as_str())));
        stat_exprs.push(c.clone().n_unique().alias(PlSmallStr::from(format!("{name}__nunique").as_str())));
        stat_exprs.push(c.clone().cast(DataType::String).min().alias(PlSmallStr::from(format!("{name}__min").as_str())));
        stat_exprs.push(c.cast(DataType::String).max().alias(PlSmallStr::from(format!("{name}__max").as_str())));
    }
    let stats_df = vertex.clone().select(stat_exprs).collect().map_err(polars_write_err)?;
    let entity_count = extract_u64(&stats_df, "__entity_count");

    let sample_exprs: Vec<Expr> = prop_cols
        .iter()
        .map(|name| {
            col(PlSmallStr::from(name.as_str()))
                .cast(DataType::String)
                .drop_nulls()
                .head(Some(5))
                .alias(PlSmallStr::from(name.as_str()))
        })
        .collect();
    let samples_df = if !sample_exprs.is_empty() {
        vertex.clone().select(sample_exprs).collect().ok()
    } else {
        None
    };

    let columns: Vec<ColumnStat> = prop_cols
        .iter()
        .map(|name| {
            let iri = label_to_iri.get(*name).cloned().unwrap_or_default();
            let xsd = config.xsd_types.get(iri.as_str()).copied();
            let datatype = xsd_to_datatype_name(xsd).to_string();
            let count = extract_u64(&stats_df, &format!("{name}__count"));
            let n_unique = extract_u64(&stats_df, &format!("{name}__nunique"));
            let min = extract_string(&stats_df, &format!("{name}__min"));
            let max = extract_string(&stats_df, &format!("{name}__max"));
            let samples = samples_df
                .as_ref()
                .and_then(|df| {
                    df.column(name.as_str()).ok()?.str().ok().map(|ca| {
                        ca.into_iter().filter_map(|v| v.map(String::from)).collect()
                    })
                })
                .unwrap_or_default();
            ColumnStat { name: (*name).clone(), iri, datatype, count, n_unique, min, max, samples }
        })
        .collect();

    Ok(TypeManifest {
        name: config.type_dir.clone(),
        iri: config.type_iri.clone(),
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

// ── YAML metadata ──

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
