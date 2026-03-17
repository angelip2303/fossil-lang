//! GraphAr-compatible property graph materializer.
//!
//! Two-pass architecture:
//!   Pass 1 — Polars streaming sink for vertex parquets, then scan-back for stats
//!   Pass 2 — DuckDB join + sort for CSR/CSC edge parquets (disk-spilling)
//!
//! Zero `.collect()` on data. Polars only calls `sink_parquet` (streaming writes).
//! DuckDB handles all analytical queries (joins, sorts, aggregations) with
//! automatic disk spilling under memory pressure.
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
//!     ├── by_source_offsets.parquet            # vertex_id | offset
//!     ├── by_target.parquet                    # source | target (CSC)
//!     └── by_target_offsets.parquet            # vertex_id | offset
//! ```

use polars::prelude::*;
use polars::prelude::sync_on_close::SyncOnCloseType;

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

fn duckdb_err(e: duckdb::Error) -> RdfError {
    RdfError::Write(format!("duckdb: {e}"))
}

// ── I/O adapters ──

/// Polars-based I/O for streaming sink and scan. Cloud credentials are
/// inherited from [`ResolvedPath`] — impossible to forget.
struct GraphStore {
    root: ResolvedPath,
}

impl GraphStore {
    /// Streaming sink: Polars writes the LazyFrame to Parquet without
    /// materializing the full dataset.
    fn sink(&self, lf: LazyFrame, rel: &str) -> Result<(), FossilError> {
        let sub = self.root.join(rel);
        let target = SinkTarget::Path(sub.pl_path().clone());
        lf.sink_parquet(target, parquet_options(), sub.cloud_options().cloned(), SINK_OPTIONS)
            .map_err(polars_write_err)?
            .collect()
            .map_err(polars_write_err)?;
        Ok(())
    }

    /// Absolute path string for a relative path (used by DuckDB).
    fn abs_path(&self, rel: &str) -> String {
        self.root.join(rel).to_str().to_string()
    }
}

// ── Main entry point ──

/// Materialize a property graph as GraphAr-compatible Parquet files.
///
/// Pass 1: Polars streaming sink for vertices + DuckDB stats from sunk parquets.
/// Pass 2: DuckDB join + CSR/CSC sort for edges with disk-spilling.
pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    resolved: &ResolvedPath,
) -> Result<DataManifest, FossilError> {
    let store = GraphStore { root: resolved.clone() };
    let conn = duckdb::Connection::open_in_memory().map_err(duckdb_err)?;

    let mut types = Vec::new();
    let mut edges = Vec::new();

    // Build lazy vertex plans (reused for sink only).
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

    // ── Pass 1: sink vertices (Polars streaming) + stats (DuckDB) ──
    for (config, vertex) in &vertices {
        let vertex_rel = format!("vertex/{}.parquet", config.type_dir);
        store.sink(vertex.clone(), &vertex_rel)?;

        let manifest = compute_type_manifest(&conn, config, &store.abs_path(&vertex_rel), &vertex_rel)?;
        types.push(manifest);
    }

    // ── Pass 2: edges via DuckDB (join + CSR/CSC sort + offset tables) ──
    for (config, _) in &vertices {
        let src_vertex_rel = format!("vertex/{}.parquet", config.type_dir);

        for ref_edge in &config.ref_edges {
            let edge_dir = format!(
                "{}_{}_{}",
                config.type_dir, ref_edge.label, ref_edge.target_type_dir
            );
            let csr_rel = format!("edge/{edge_dir}/by_source.parquet");
            let csc_rel = format!("edge/{edge_dir}/by_target.parquet");
            let tgt_vertex_rel = format!("vertex/{}.parquet", ref_edge.target_type_dir);

            let count = produce_edges(
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

// ── Edge production (DuckDB) ──

/// Join raw edges with vertex id_maps and produce CSR + CSC parquets.
///
/// DuckDB handles the hash join (IRI→ID), the ORDER BY (CSR/CSC), and
/// offset table generation — all with automatic disk spilling.
fn produce_edges(
    store: &GraphStore,
    conn: &duckdb::Connection,
    frame: &LazyFrame,
    src_iri_expr: Expr,
    tgt_iri_expr: Expr,
    src_vertex_rel: &str,
    tgt_vertex_rel: &str,
    edge_dir: &str,
) -> Result<u64, FossilError> {
    // 1. Sink raw edges via Polars streaming
    let staging_rel = format!("edge/{edge_dir}/_staging.parquet");
    let raw = frame
        .clone()
        .select([
            src_iri_expr.alias("src_iri"),
            tgt_iri_expr.alias("tgt_iri"),
        ])
        .filter(col("src_iri").is_not_null().and(col("tgt_iri").is_not_null()));
    store.sink(raw, &staging_rel)?;

    let staging = store.abs_path(&staging_rel);
    let src_vtx = store.abs_path(src_vertex_rel);
    let tgt_vtx = store.abs_path(tgt_vertex_rel);
    let csr_out = store.abs_path(&format!("edge/{edge_dir}/by_source.parquet"));
    let csc_out = store.abs_path(&format!("edge/{edge_dir}/by_target.parquet"));
    let csr_offsets = store.abs_path(&format!("edge/{edge_dir}/by_source_offsets.parquet"));
    let csc_offsets = store.abs_path(&format!("edge/{edge_dir}/by_target_offsets.parquet"));

    // 2. DuckDB: join once → temp table
    conn.execute_batch(&format!(
        "CREATE OR REPLACE TEMP TABLE __edges AS
            SELECT s._id AS source, t._id AS target
            FROM read_parquet('{staging}') e
            JOIN read_parquet('{src_vtx}') s ON e.src_iri = s.subject
            JOIN read_parquet('{tgt_vtx}') t ON e.tgt_iri = t.subject"
    ))
    .map_err(duckdb_err)?;

    // 3. CSR: ordered by source
    conn.execute_batch(&format!(
        "COPY (SELECT source, target FROM __edges ORDER BY source, target)
         TO '{csr_out}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})"
    ))
    .map_err(duckdb_err)?;

    // 4. CSC: ordered by target
    conn.execute_batch(&format!(
        "COPY (SELECT source, target FROM __edges ORDER BY target, source)
         TO '{csc_out}' (FORMAT PARQUET, ROW_GROUP_SIZE {VERTEX_CHUNK_SIZE})"
    ))
    .map_err(duckdb_err)?;

    // 5. Offset tables
    conn.execute_batch(&format!(
        "COPY (
            WITH ranked AS (
                SELECT source AS vertex_id, ROW_NUMBER() OVER (ORDER BY source, target) - 1 AS pos
                FROM __edges
            )
            SELECT vertex_id, MIN(pos) AS start_offset
            FROM ranked GROUP BY vertex_id ORDER BY vertex_id
        ) TO '{csr_offsets}' (FORMAT PARQUET)"
    ))
    .map_err(duckdb_err)?;

    conn.execute_batch(&format!(
        "COPY (
            WITH ranked AS (
                SELECT target AS vertex_id, ROW_NUMBER() OVER (ORDER BY target, source) - 1 AS pos
                FROM __edges
            )
            SELECT vertex_id, MIN(pos) AS start_offset
            FROM ranked GROUP BY vertex_id ORDER BY vertex_id
        ) TO '{csc_offsets}' (FORMAT PARQUET)"
    ))
    .map_err(duckdb_err)?;

    // 6. Edge count
    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(duckdb_err)?;

    // 7. Cleanup
    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(duckdb_err)?;
    let _ = std::fs::remove_file(store.abs_path(&staging_rel));

    Ok(count)
}

// ── Stats computation (DuckDB) ──

/// Map XSD datatype IRI to a human-readable name for the manifest.
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

/// Compute type manifest via DuckDB queries on the sunk vertex parquet.
fn compute_type_manifest(
    conn: &duckdb::Connection,
    config: &OutputConfig,
    vertex_abs_path: &str,
    vertex_rel_path: &str,
) -> Result<TypeManifest, FossilError> {
    let label_to_iri = &config.label_to_iri;

    // Property column names from the config's label_to_iri (excludes _id, subject).
    let prop_cols: Vec<String> = config
        .selection
        .iter()
        .filter_map(|expr| {
            if let Expr::Alias(_, name) = expr {
                let s = name.to_string();
                if s != "_subject" {
                    return Some(s);
                }
            }
            None
        })
        .collect();

    // Entity count
    let entity_count: u64 = conn
        .query_row(
            &format!("SELECT count(*) FROM read_parquet('{vertex_abs_path}')"),
            [],
            |row| row.get(0),
        )
        .map_err(duckdb_err)?;

    // Per-column stats + samples via DuckDB
    let columns: Vec<ColumnStat> = prop_cols
        .iter()
        .map(|name| {
            let iri = label_to_iri.get(name).cloned().unwrap_or_default();
            let xsd = config.xsd_types.get(iri.as_str()).copied();
            let datatype = xsd_to_datatype_name(xsd).to_string();

            let stats_sql = format!(
                "SELECT count(\"{name}\"), count(DISTINCT \"{name}\"), \
                 min(\"{name}\")::VARCHAR, max(\"{name}\")::VARCHAR \
                 FROM read_parquet('{vertex_abs_path}')"
            );
            let (count, n_unique, min, max) = conn
                .query_row(&stats_sql, [], |row| {
                    Ok((
                        row.get::<_, u64>(0).unwrap_or(0),
                        row.get::<_, u64>(1).unwrap_or(0),
                        row.get::<_, Option<String>>(2).unwrap_or(None),
                        row.get::<_, Option<String>>(3).unwrap_or(None),
                    ))
                })
                .unwrap_or((0, 0, None, None));

            let samples_sql = format!(
                "SELECT \"{name}\"::VARCHAR FROM read_parquet('{vertex_abs_path}') \
                 WHERE \"{name}\" IS NOT NULL LIMIT 5"
            );
            let samples = conn
                .prepare(&samples_sql)
                .and_then(|mut stmt| {
                    stmt.query_map([], |row| row.get::<_, String>(0))
                        .map(|rows| rows.filter_map(|r| r.ok()).collect::<Vec<_>>())
                })
                .unwrap_or_default();

            ColumnStat {
                name: name.clone(),
                iri,
                datatype,
                count,
                n_unique,
                min,
                max,
                samples,
            }
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

// ── YAML metadata (GraphAr gar/v1 spec) ──

fn write_yaml_metadata(
    store: &GraphStore,
    types: &[TypeManifest],
    edges: &[EdgeManifest],
) -> Result<(), FossilError> {
    if store.root.pl_path().is_cloud_url() {
        // YAML metadata is local-only. For cloud, the DataManifest JSON
        // (stored in keasy's SQLite) serves as the canonical metadata.
        return Ok(());
    }

    let vertex_ymls: Vec<String> = types
        .iter()
        .map(|t| format!("{}.vertex.yml", t.name))
        .collect();
    let edge_ymls: Vec<String> = edges
        .iter()
        .map(|e| {
            format!(
                "{}_{}_{}.edge.yml",
                e.source_type, e.name, e.target_type
            )
        })
        .collect();
    let graph_yml = format!(
        "name: dataset\nprefix: ./\nvertices:\n{}\nedges:\n{}\nversion: gar/v1\n",
        vertex_ymls
            .iter()
            .map(|v| format!("  - {v}"))
            .collect::<Vec<_>>()
            .join("\n"),
        edge_ymls
            .iter()
            .map(|e| format!("  - {e}"))
            .collect::<Vec<_>>()
            .join("\n"),
    );
    std::fs::write(store.root.join("graph.yml").to_str(), graph_yml)
        .map_err(|e| RdfError::Write(e.to_string()))?;

    for t in types {
        let props: Vec<String> = t
            .columns
            .iter()
            .map(|c| {
                format!(
                    "      - name: {}\n        data_type: {}\n        is_primary: false",
                    c.name, c.datatype
                )
            })
            .collect();
        let subject_prop =
            "      - name: subject\n        data_type: string\n        is_primary: true";
        let yml = format!(
            "type: {}\nchunk_size: {VERTEX_CHUNK_SIZE}\nprefix: vertex/{}/\nproperty_groups:\n  - file_type: parquet\n    properties:\n{}\n{}\nversion: gar/v1\n",
            t.name, t.name, subject_prop, props.join("\n")
        );
        std::fs::write(
            store.root.join(&format!("{}.vertex.yml", t.name)).to_str(),
            yml,
        )
        .map_err(|e| RdfError::Write(e.to_string()))?;
    }

    for e in edges {
        let yml = format!(
            "src_type: {src}\nedge_type: {edge}\ndst_type: {dst}\nchunk_size: 4194304\nsrc_chunk_size: {VERTEX_CHUNK_SIZE}\ndst_chunk_size: {VERTEX_CHUNK_SIZE}\ndirected: true\nprefix: edge/{src}_{edge}_{dst}/\nadj_lists:\n  - ordered: true\n    aligned_by: src\n    file_type: parquet\n  - ordered: true\n    aligned_by: dst\n    file_type: parquet\nversion: gar/v1\n",
            src = e.source_type,
            edge = e.name,
            dst = e.target_type,
        );
        std::fs::write(
            store
                .root
                .join(&format!(
                    "{}_{}_{}.edge.yml",
                    e.source_type, e.name, e.target_type
                ))
                .to_str(),
            yml,
        )
        .map_err(|e2| RdfError::Write(e2.to_string()))?;
    }

    Ok(())
}
