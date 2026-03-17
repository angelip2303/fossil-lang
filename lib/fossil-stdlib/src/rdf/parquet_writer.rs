//! GraphAr-compatible property graph materializer.
//!
//! Two-engine architecture with local materialization points:
//!   Polars  — streaming sink, stats aggregation, local staging, cloud upload
//!   DuckDB  — join IRI→ID + CSR/CSC sort (disk-spilling), writes edges to cloud
//!
//! Source data is read ONCE per vertex type (Polars sink → local temp).
//! All subsequent operations (stats, upload, join) read from local.
//! Equivalent to Spark's `persist(DISK_ONLY)`.
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

/// GraphAr recommended vertex chunk size (2^18).
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

/// Convert a `Path` to a UTF-8 string, returning an error on non-UTF-8 paths.
fn path_to_str(path: &Path) -> Result<&str, RdfError> {
    path.to_str()
        .ok_or_else(|| RdfError::Write(format!("non-UTF-8 path: {}", path.display())))
}

fn scan_local(path: &Path) -> Result<LazyFrame, RdfError> {
    LazyFrame::scan_parquet(
        PlPath::from_str(path_to_str(path)?),
        ScanArgsParquet::default(),
    )
    .map_err(polars_write_err)
}

// ── DuckDB cloud credentials ──

fn configure_duckdb_cloud(
    conn: &duckdb::Connection,
    resolved: &ResolvedPath,
) -> Result<(), FossilError> {
    let config = resolved.cloud_config();
    if config.is_empty() {
        return Ok(());
    }
    // Case-insensitive lookup without allocating a lowercase copy of the map.
    let get = |key: &str| -> Option<&str> {
        config
            .iter()
            .find(|(k, _)| k.eq_ignore_ascii_case(key))
            .map(|(_, v)| v.as_str())
    };

    if let Some(account_name) = get("azure_storage_account_name") {
        conn.execute_batch("INSTALL azure; LOAD azure;")
            .map_err(RdfError::from)?;

        if let (Some(tenant), Some(client_id), Some(secret)) = (
            get("azure_storage_tenant_id"),
            get("azure_storage_client_id"),
            get("azure_storage_client_secret"),
        ) {
            conn.execute(
                "CREATE TEMPORARY SECRET __azure (
                    TYPE AZURE, PROVIDER service_principal,
                    ACCOUNT_NAME ?, TENANT_ID ?, CLIENT_ID ?, CLIENT_SECRET ?
                )",
                duckdb::params![account_name, tenant, client_id, secret],
            )
            .map_err(RdfError::from)?;
        } else if let Some(key) = get("azure_storage_account_key") {
            let cs = format!("AccountName={account_name};AccountKey={key};EndpointSuffix=core.windows.net");
            conn.execute(
                "CREATE TEMPORARY SECRET __azure (TYPE AZURE, CONNECTION_STRING ?)",
                [&cs],
            )
            .map_err(RdfError::from)?;
        } else if let Some(sas) = get("azure_storage_sas_token") {
            let cs = format!("AccountName={account_name};SharedAccessSignature={sas};EndpointSuffix=core.windows.net");
            conn.execute(
                "CREATE TEMPORARY SECRET __azure (TYPE AZURE, CONNECTION_STRING ?)",
                [&cs],
            )
            .map_err(RdfError::from)?;
        }
    }
    Ok(())
}

// ── I/O adapters ──

/// Polars-based cloud I/O. Credentials inherited from [`ResolvedPath`].
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

    fn abs_path(&self, rel: &str) -> String {
        self.root.join(rel).to_str().to_string()
    }
}

/// Sink a LazyFrame to local disk (no cloud credentials).
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

/// Monotonic counter for unique temp file names (avoids nanosecond collisions).
static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique local temp path for a given edge/vertex dir.
fn local_temp(prefix: &str, name: &str) -> PathBuf {
    let id = std::process::id();
    let seq = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    std::env::temp_dir().join(format!(
        "fossil_{prefix}_{}_{id}_{seq}.parquet",
        name.replace('/', "_")
    ))
}

/// RAII guard that removes temp files on drop, ensuring cleanup even on early return.
struct TempGuard {
    paths: Vec<PathBuf>,
}

impl Drop for TempGuard {
    fn drop(&mut self) {
        for path in &self.paths {
            let _ = std::fs::remove_file(path);
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
    configure_duckdb_cloud(&conn, resolved)?;

    let mut types = Vec::new();
    let mut edges = Vec::new();
    // Maps type_dir -> local path for DuckDB join (vertex materialization points).
    let mut local_vertices: HashMap<String, PathBuf> = HashMap::new();
    let mut temp_guard = TempGuard { paths: Vec::new() };

    // Build lazy vertex plans.
    let vertices: Vec<_> = configs
        .iter()
        .map(|config| {
            let vertex = frame
                .clone()
                .select(config.selection.clone())
                .rename(["_subject"], ["subject"], true)
                .with_row_index("_id", Some(0));
            (config, vertex)
        })
        .collect();

    // ── Pass 1: source -> local materialization -> stats + upload ──
    let is_cloud = resolved.pl_path().is_cloud_url();

    for (config, vertex) in &vertices {
        let vertex_rel = format!("vertex/{}.parquet", config.type_dir);

        // Both cloud and local paths need a local file for DuckDB joins later.
        // Cloud: stage locally first, then upload.
        // Local: sink to destination directly; it already *is* the local file.
        let local_path = if is_cloud {
            let local = local_temp("vtx", &config.type_dir);
            sink_local(vertex.clone(), &local)?;
            let upload_lf = scan_local(&local)?;
            store.sink(upload_lf, &vertex_rel)?;
            temp_guard.paths.push(local.clone());
            local
        } else {
            store.sink(vertex.clone(), &vertex_rel)?;
            PathBuf::from(store.abs_path(&vertex_rel))
        };

        let written = scan_local(&local_path)?;
        let manifest = compute_type_manifest(config, &vertex_rel, &written)?;
        types.push(manifest);
        local_vertices.insert(config.type_dir.clone(), local_path);
    }

    // ── Pass 2: edges via local staging + DuckDB join/sort -> cloud ──
    for (config, _) in &vertices {
        let src_local = local_vertices
            .get(&config.type_dir)
            .ok_or_else(|| RdfError::Write(format!("missing local vertex: {}", config.type_dir)))?;

        for ref_edge in &config.ref_edges {
            let edge_dir = format!(
                "{}_{}_{}",
                config.type_dir, ref_edge.label, ref_edge.target_type_dir
            );
            let tgt_local = local_vertices
                .get(&ref_edge.target_type_dir)
                .ok_or_else(|| RdfError::Write(format!("missing local vertex: {}", ref_edge.target_type_dir)))?;

            let (csr_rel, csc_rel, count) = produce_edges(
                &store,
                &conn,
                frame,
                config.subject_expr.clone(),
                ref_edge.expr.clone(),
                src_local,
                tgt_local,
                &edge_dir,
            )?;

            edges.push(EdgeManifest {
                name: ref_edge.label.clone(),
                iri: ref_edge.predicate_uri.clone(),
                source_type: config.type_dir.clone(),
                target_type: ref_edge.target_type_dir.clone(),
                by_source: csr_rel,
                by_target: csc_rel,
                count,
            });
        }
    }

    // YAML is written while temp files still exist (temp_guard drops after return).
    write_yaml_metadata(&store, &types, &edges)?;
    // temp_guard drops here, cleaning up all cloud temp files.
    Ok(DataManifest { types, edges })
}

// ── Edge production ──

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

/// DuckDB join + CSR/CSC sort. Reads LOCAL files, writes edges to cloud.
fn produce_edges(
    store: &GraphStore,
    conn: &duckdb::Connection,
    frame: &LazyFrame,
    src_iri_expr: Expr,
    tgt_iri_expr: Expr,
    src_vertex_local: &Path,
    tgt_vertex_local: &Path,
    edge_dir: &str,
) -> Result<(String, String, u64), FossilError> {
    // 1. Polars: source → local staging (1 source read, streaming)
    let staging = local_temp("stg", edge_dir);
    let raw = frame
        .clone()
        .select([
            src_iri_expr.alias("src_iri"),
            tgt_iri_expr.alias("tgt_iri"),
        ])
        .filter(col("src_iri").is_not_null().and(col("tgt_iri").is_not_null()));
    sink_local(raw, &staging)?;

    let stg = path_to_str(&staging)?;
    let src = path_to_str(src_vertex_local)?;
    let tgt = path_to_str(tgt_vertex_local)?;

    // 2. DuckDB: join LOCAL files → temp table (0 cloud reads)
    conn.execute_batch(&format!(
        "CREATE OR REPLACE TEMP TABLE __edges AS
            SELECT s._id AS source, t._id AS target
            FROM read_parquet('{stg}') e
            JOIN read_parquet('{src}') s ON e.src_iri = s.subject
            JOIN read_parquet('{tgt}') t ON e.tgt_iri = t.subject"
    ))
    .map_err(RdfError::from)?;

    // 3. CSR + CSC + offsets → cloud
    let csr_rel = format!("edge/{edge_dir}/by_source.parquet");
    let csc_rel = format!("edge/{edge_dir}/by_target.parquet");

    write_sorted_edges(
        conn,
        "source",
        "target",
        &store.abs_path(&csr_rel),
        &store.abs_path(&format!("edge/{edge_dir}/by_source_offsets.parquet")),
    )?;
    write_sorted_edges(
        conn,
        "target",
        "source",
        &store.abs_path(&csc_rel),
        &store.abs_path(&format!("edge/{edge_dir}/by_target_offsets.parquet")),
    )?;

    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(RdfError::from)?;

    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(RdfError::from)?;
    let _ = std::fs::remove_file(&staging);

    Ok((csr_rel, csc_rel, count))
}

// ── Stats (Polars streaming aggregation) ──

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
