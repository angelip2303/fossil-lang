//! GraphAr-compatible property graph materializer.
//!
//! Two-pass architecture:
//!   Pass 1 — Polars streaming sink for vertex parquets, then DuckDB stats
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

/// Configure DuckDB with cloud credentials from the ResolvedPath.
/// Maps Polars credential keys to DuckDB Azure/S3 settings.
fn configure_duckdb_cloud(
    conn: &duckdb::Connection,
    resolved: &ResolvedPath,
) -> Result<(), FossilError> {
    let config = resolved.cloud_config();
    if config.is_empty() {
        return Ok(());
    }

    // DuckDB Azure configuration via SET statements.
    // Key mapping: Polars uses lowercase snake_case, DuckDB uses the same.
    let duckdb_settings: &[(&str, &str)] = &[
        ("azure_storage_account_name", "azure_account_name"),
        ("azure_storage_account_key", "azure_account_key"),
        ("azure_storage_sas_token", "azure_sas_token"),
        ("azure_storage_client_id", "azure_client_id"),
        ("azure_storage_client_secret", "azure_client_secret"),
        ("azure_storage_tenant_id", "azure_tenant_id"),
    ];

    let mut any_set = false;
    for (polars_key, duckdb_key) in duckdb_settings {
        if let Some(value) = config.get(*polars_key) {
            conn.execute_batch(&format!("SET {duckdb_key} = '{value}'"))
                .map_err(RdfError::from)?;
            any_set = true;
        }
    }

    if any_set {
        // Load Azure extension if any Azure credential was set
        let _ = conn.execute_batch("INSTALL azure; LOAD azure;");
    }

    Ok(())
}

// ── I/O adapters ──

/// Polars-based I/O for streaming sink. Cloud credentials are inherited
/// from [`ResolvedPath`] — impossible to forget.
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
    let conn = duckdb::Connection::open_in_memory().map_err(RdfError::from)?;
    configure_duckdb_cloud(&conn, resolved)?;

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

        let manifest =
            compute_type_manifest(&conn, config, &store.abs_path(&vertex_rel), &vertex_rel)?;
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

// ── Edge production (DuckDB) ──

/// Write a sorted edge file + its offset table. Reused for both CSR and CSC.
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

/// Join raw edges with vertex id_maps and produce CSR + CSC parquets.
///
/// DuckDB handles the hash join (IRI→ID), the ORDER BY (CSR/CSC), and
/// offset table generation — all with automatic disk spilling.
///
/// Returns `(csr_rel, csc_rel, edge_count)`.
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

    // 2. DuckDB: join once → temp table
    conn.execute_batch(&format!(
        "CREATE OR REPLACE TEMP TABLE __edges AS
            SELECT s._id AS source, t._id AS target
            FROM read_parquet('{staging}') e
            JOIN read_parquet('{src_vtx}') s ON e.src_iri = s.subject
            JOIN read_parquet('{tgt_vtx}') t ON e.tgt_iri = t.subject"
    ))
    .map_err(RdfError::from)?;

    // 3. CSR + CSC with shared helper
    let csr_rel = format!("edge/{edge_dir}/by_source.parquet");
    let csc_rel = format!("edge/{edge_dir}/by_target.parquet");
    let csr_offsets_rel = format!("edge/{edge_dir}/by_source_offsets.parquet");
    let csc_offsets_rel = format!("edge/{edge_dir}/by_target_offsets.parquet");

    write_sorted_edges(
        conn,
        "source",
        "target",
        &store.abs_path(&csr_rel),
        &store.abs_path(&csr_offsets_rel),
    )?;
    write_sorted_edges(
        conn,
        "target",
        "source",
        &store.abs_path(&csc_rel),
        &store.abs_path(&csc_offsets_rel),
    )?;

    // 4. Edge count
    let count: u64 = conn
        .query_row("SELECT count(*) FROM __edges", [], |row| row.get(0))
        .map_err(RdfError::from)?;

    // 5. Cleanup
    conn.execute_batch("DROP TABLE IF EXISTS __edges")
        .map_err(RdfError::from)?;
    let _ = std::fs::remove_file(store.abs_path(&staging_rel));

    Ok((csr_rel, csc_rel, count))
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
///
/// Uses two batched queries (stats + samples) instead of per-column queries.
fn compute_type_manifest(
    conn: &duckdb::Connection,
    config: &OutputConfig,
    vertex_abs_path: &str,
    vertex_rel_path: &str,
) -> Result<TypeManifest, FossilError> {
    let label_to_iri = &config.label_to_iri;

    // Property column names from label_to_iri keys (the source of truth).
    let prop_cols: Vec<&String> = label_to_iri.keys().collect();

    // Entity count + all column stats in a single query (1 scan).
    let stat_parts: Vec<String> = prop_cols
        .iter()
        .map(|name| {
            format!(
                "count(\"{name}\"), count(DISTINCT \"{name}\"), \
                 min(\"{name}\")::VARCHAR, max(\"{name}\")::VARCHAR"
            )
        })
        .collect();

    let stats_sql = if stat_parts.is_empty() {
        format!("SELECT count(*) FROM read_parquet('{vertex_abs_path}')")
    } else {
        format!(
            "SELECT count(*), {} FROM read_parquet('{vertex_abs_path}')",
            stat_parts.join(", ")
        )
    };

    let mut stmt = conn.prepare(&stats_sql).map_err(RdfError::from)?;
    let mut rows = stmt.query([]).map_err(RdfError::from)?;
    let row = rows
        .next()
        .map_err(RdfError::from)?
        .ok_or_else(|| RdfError::Write("stats query returned no rows".into()))?;

    let entity_count: u64 = row.get::<_, u64>(0).unwrap_or(0);

    let columns: Vec<ColumnStat> = prop_cols
        .iter()
        .enumerate()
        .map(|(i, name)| {
            let iri = label_to_iri.get(*name).cloned().unwrap_or_default();
            let xsd = config.xsd_types.get(iri.as_str()).copied();
            let datatype = xsd_to_datatype_name(xsd).to_string();

            // 4 columns per prop, offset by 1 (entity_count is column 0)
            let base = 1 + i * 4;
            let count = row.get::<_, u64>(base).unwrap_or(0);
            let n_unique = row.get::<_, u64>(base + 1).unwrap_or(0);
            let min = row.get::<_, Option<String>>(base + 2).unwrap_or(None);
            let max = row.get::<_, Option<String>>(base + 3).unwrap_or(None);

            // Samples: deferred to a second query below
            ColumnStat {
                name: (*name).clone(),
                iri,
                datatype,
                count,
                n_unique,
                min,
                max,
                samples: Vec::new(),
            }
        })
        .collect();

    // Samples: single UNION ALL query (1 scan) instead of N separate queries.
    if !prop_cols.is_empty() {
        let sample_parts: Vec<String> = prop_cols
            .iter()
            .map(|name| {
                format!(
                    "SELECT '{name}' AS col_name, \"{name}\"::VARCHAR AS val \
                     FROM read_parquet('{vertex_abs_path}') \
                     WHERE \"{name}\" IS NOT NULL LIMIT 5"
                )
            })
            .collect();
        let samples_sql = sample_parts.join(" UNION ALL ");

        if let Ok(mut stmt) = conn.prepare(&samples_sql) {
            if let Ok(sample_rows) = stmt.query_map([], |r| {
                Ok((
                    r.get::<_, String>(0).unwrap_or_default(),
                    r.get::<_, String>(1).unwrap_or_default(),
                ))
            }) {
                let mut columns = columns;
                for pair in sample_rows.flatten() {
                    if let Some(col) = columns.iter_mut().find(|c| c.name == pair.0) {
                        col.samples.push(pair.1);
                    }
                }
                return Ok(TypeManifest {
                    name: config.type_dir.clone(),
                    iri: config.type_iri.clone(),
                    vertex_file: vertex_rel_path.to_string(),
                    entity_count,
                    columns,
                });
            }
        }
    }

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
