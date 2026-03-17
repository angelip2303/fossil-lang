//! GraphAr-compatible property graph materializer.
//!
//! Two-engine architecture:
//!   Polars  — streaming sink for vertices, streaming aggregation for stats,
//!             local staging for edge IRI pairs
//!   DuckDB  — join IRI→ID + CSR/CSC sort for edges (disk-spilling)
//!
//! Each engine does what it's best at:
//!   - Polars: lazy evaluation, streaming sink, expression graphs
//!   - DuckDB: larger-than-RAM hash joins + external merge sorts
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

// ── DuckDB cloud credentials ──

/// Configure DuckDB cloud credentials via `CREATE TEMPORARY SECRET`.
///
/// Detects the auth method from available credentials and creates a
/// session-scoped secret. Credentials are passed as parameters (never
/// interpolated in SQL) to prevent injection.
fn configure_duckdb_cloud(
    conn: &duckdb::Connection,
    resolved: &ResolvedPath,
) -> Result<(), FossilError> {
    let config = resolved.cloud_config();
    if config.is_empty() {
        return Ok(());
    }

    // Case-insensitive lookup (keasy passes UPPERCASE env_var names).
    let lower: std::collections::HashMap<String, &str> = config
        .iter()
        .map(|(k, v)| (k.to_lowercase(), v.as_str()))
        .collect();
    let get = |key: &str| -> Option<&str> { lower.get(key).copied() };

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

/// Polars-based I/O for streaming sink to cloud. Cloud credentials are
/// inherited from [`ResolvedPath`] — impossible to forget.
struct GraphStore {
    root: ResolvedPath,
}

impl GraphStore {
    /// Streaming sink to the destination (cloud or local).
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

    /// Absolute path string for DuckDB queries.
    fn abs_path(&self, rel: &str) -> String {
        self.root.join(rel).to_str().to_string()
    }
}

/// Sink a LazyFrame to a local temp path (no cloud credentials).
fn sink_local(lf: LazyFrame, path: &std::path::Path) -> Result<(), FossilError> {
    lf.sink_parquet(
        SinkTarget::Path(PlPath::from_str(path.to_str().unwrap_or("/tmp/fossil_fallback.parquet"))),
        parquet_options(),
        None,
        SINK_OPTIONS,
    )
    .map_err(polars_write_err)?
    .collect()
    .map_err(polars_write_err)?;
    Ok(())
}

// ── Main entry point ──

/// Materialize a property graph as GraphAr-compatible Parquet files.
///
/// Pass 1: Polars streaming sink for vertices + Polars streaming stats.
/// Pass 2: Polars local staging + DuckDB join/sort for edges.
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

    // Build lazy vertex plans (reused for sink + stats).
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

    // ── Pass 1: sink vertices (Polars → cloud) + stats (Polars streaming) ──
    for (config, vertex) in &vertices {
        let vertex_rel = format!("vertex/{}.parquet", config.type_dir);
        store.sink(vertex.clone(), &vertex_rel)?;

        let manifest = compute_type_manifest(config, &vertex_rel, vertex)?;
        types.push(manifest);
    }

    // ── Pass 2: edges via local staging + DuckDB join/sort → cloud ──
    for (config, _) in &vertices {
        let src_vertex_rel = format!("vertex/{}.parquet", config.type_dir);

        for ref_edge in &config.ref_edges {
            let edge_dir = format!(
                "{}_{}_{}",
                config.type_dir, ref_edge.label, ref_edge.target_type_dir
            );
            let tgt_vertex_rel = format!("vertex/{}.parquet", ref_edge.target_type_dir);

            let (csr_rel, csc_rel, count) = produce_edges(
                &store,
                &conn,
                frame,
                config.subject_expr.clone(),
                ref_edge.expr.clone(),
                &src_vertex_rel,
                &tgt_vertex_rel,
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

    write_yaml_metadata(&store, &types, &edges)?;
    Ok(DataManifest { types, edges })
}

// ── Edge production (Polars staging + DuckDB join/sort) ──

/// Write a sorted edge file + its offset table. Reused for CSR and CSC.
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

/// Produce CSR + CSC edge parquets with offset tables.
///
/// 1. Polars sinks raw edge IRIs to LOCAL temp (no cloud round-trip)
/// 2. DuckDB reads local staging + cloud vertex parquets → join + sort → cloud
fn produce_edges(
    store: &GraphStore,
    conn: &duckdb::Connection,
    frame: &LazyFrame,
    src_iri_expr: Expr,
    tgt_iri_expr: Expr,
    src_vertex_rel: &str,
    tgt_vertex_rel: &str,
    edge_dir: &str,
) -> Result<(String, String, u64), FossilError> {
    // 1. Polars sink raw edges to LOCAL temp (fast, no cloud)
    let staging_local = std::env::temp_dir()
        .join(format!("fossil_{}.parquet", edge_dir.replace('/', "_")));
    let raw = frame
        .clone()
        .select([
            src_iri_expr.alias("src_iri"),
            tgt_iri_expr.alias("tgt_iri"),
        ])
        .filter(col("src_iri").is_not_null().and(col("tgt_iri").is_not_null()));
    sink_local(raw, &staging_local)?;

    let local_str = staging_local.to_str().unwrap_or("").to_string();
    let src_vtx = store.abs_path(src_vertex_rel);
    let tgt_vtx = store.abs_path(tgt_vertex_rel);

    // 2. DuckDB: join local staging + cloud vertices → temp table
    conn.execute_batch(&format!(
        "CREATE OR REPLACE TEMP TABLE __edges AS
            SELECT s._id AS source, t._id AS target
            FROM read_parquet('{local_str}') e
            JOIN read_parquet('{src_vtx}') s ON e.src_iri = s.subject
            JOIN read_parquet('{tgt_vtx}') t ON e.tgt_iri = t.subject"
    ))
    .map_err(RdfError::from)?;

    // 3. CSR + CSC + offsets (reads from temp table, writes to cloud)
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

    // 4. Edge count (from temp table, no cloud read)
    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(RdfError::from)?;

    // 5. Cleanup
    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(RdfError::from)?;
    let _ = std::fs::remove_file(&staging_local);

    Ok((csr_rel, csc_rel, count))
}

// ── Stats computation (Polars streaming aggregation) ──

fn xsd_to_datatype_name(xsd: Option<&str>) -> &'static str {
    match xsd {
        Some(s) if s.ends_with("#integer") || s.ends_with("#int") || s.ends_with("#long") => {
            "int64"
        }
        Some(s)
            if s.ends_with("#float") || s.ends_with("#double") || s.ends_with("#decimal") =>
        {
            "double"
        }
        Some(s) if s.ends_with("#boolean") => "boolean",
        Some(s) if s.ends_with("#date") => "date",
        _ => "string",
    }
}

/// Compute type manifest via Polars streaming aggregation.
///
/// Stats are 1-row aggregations — Polars processes in batches and only
/// keeps running state. No disk spilling needed.
fn compute_type_manifest(
    config: &OutputConfig,
    vertex_rel_path: &str,
    vertex: &LazyFrame,
) -> Result<TypeManifest, FossilError> {
    let label_to_iri = &config.label_to_iri;
    let prop_cols: Vec<&String> = label_to_iri.keys().collect();

    // Single batched query: entity_count + per-column count/n_unique/min/max
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

    // Samples: 5 values per column
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
                    df.column(name.as_str())
                        .ok()?
                        .str()
                        .ok()
                        .map(|ca| {
                            ca.into_iter()
                                .filter_map(|v| v.map(String::from))
                                .collect::<Vec<_>>()
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
            c.u32()
                .ok()
                .map(|ca| ca.get(0).unwrap_or(0) as u64)
                .or_else(|| c.u64().ok().map(|ca| ca.get(0).unwrap_or(0)))
        })
        .unwrap_or(0)
}

fn extract_string(df: &DataFrame, col_name: &str) -> Option<String> {
    df.column(col_name)
        .ok()
        .and_then(|c| c.str().ok())
        .and_then(|ca| ca.get(0))
        .map(String::from)
}

// ── YAML metadata (GraphAr gar/v1 spec) ──

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

    let graph_yml = format!(
        "name: dataset\nprefix: ./\nvertices:\n{}\nedges:\n{}\nversion: gar/v1\n",
        vertex_ymls.iter().map(|v| format!("  - {v}")).collect::<Vec<_>>().join("\n"),
        edge_ymls.iter().map(|e| format!("  - {e}")).collect::<Vec<_>>().join("\n"),
    );
    std::fs::write(store.root.join("graph.yml").to_str(), graph_yml)
        .map_err(|e| RdfError::Write(e.to_string()))?;

    for t in types {
        let props: Vec<String> = t.columns.iter().map(|c| {
            format!("      - name: {}\n        data_type: {}\n        is_primary: false", c.name, c.datatype)
        }).collect();
        let subject_prop = "      - name: subject\n        data_type: string\n        is_primary: true";
        let yml = format!(
            "type: {}\nchunk_size: {VERTEX_CHUNK_SIZE}\nprefix: vertex/{}/\nproperty_groups:\n  - file_type: parquet\n    properties:\n{}\n{}\nversion: gar/v1\n",
            t.name, t.name, subject_prop, props.join("\n")
        );
        std::fs::write(store.root.join(&format!("{}.vertex.yml", t.name)).to_str(), yml)
            .map_err(|e| RdfError::Write(e.to_string()))?;
    }

    for e in edges {
        let yml = format!(
            "src_type: {src}\nedge_type: {edge}\ndst_type: {dst}\nchunk_size: 4194304\nsrc_chunk_size: {VERTEX_CHUNK_SIZE}\ndst_chunk_size: {VERTEX_CHUNK_SIZE}\ndirected: true\nprefix: edge/{src}_{edge}_{dst}/\nadj_lists:\n  - ordered: true\n    aligned_by: src\n    file_type: parquet\n  - ordered: true\n    aligned_by: dst\n    file_type: parquet\nversion: gar/v1\n",
            src = e.source_type, edge = e.name, dst = e.target_type,
        );
        std::fs::write(
            store.root.join(&format!("{}_{}_{}.edge.yml", e.source_type, e.name, e.target_type)).to_str(),
            yml,
        )
        .map_err(|e2| RdfError::Write(e2.to_string()))?;
    }

    Ok(())
}
