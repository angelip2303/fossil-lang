//! GraphAr-compatible property graph materializer.
//!
//! Produces vertex files (one per type) and edge files (CSR + CSC per edge type)
//! as Parquet, plus YAML metadata following the GraphAr `gar/v1` spec.
//!
//! Output layout:
//! ```text
//! dataset/
//! ├── graph.yml
//! ├── {type}.vertex.yml
//! ├── {src}_{edge}_{dst}.edge.yml
//! ├── vertex/{type}.parquet         # _id | subject | properties...
//! └── edge/{src}_{edge}_{dst}/
//!     ├── by_source.parquet         # source | target (sorted by source, CSR)
//!     └── by_target.parquet         # source | target (sorted by target, CSC)
//! ```

use polars::prelude::*;
use polars::prelude::sync_on_close::SyncOnCloseType;

use fossil_lang::error::FossilError;
use fossil_lang::runtime::executor::{ColumnStat, DataManifest, EdgeManifest, TypeManifest};
use fossil_lang::traits::resolver::ResolvedPath;

use super::OutputConfig;
use super::RdfError;

const SINK_OPTIONS: SinkOptions = SinkOptions {
    mkdir: true,
    maintain_order: true,
    sync_on_close: SyncOnCloseType::None,
};

fn parquet_options() -> ParquetWriteOptions {
    ParquetWriteOptions {
        row_group_size: Some(50_000),
        ..Default::default()
    }
}

fn polars_write_err(e: PolarsError) -> RdfError {
    RdfError::Write(e.to_string())
}

/// Parquet-specific I/O adapter built from a `ResolvedPath`. Cloud credentials
/// are inherited by every operation — impossible to forget.
struct GraphStore {
    root: ResolvedPath,
}

impl GraphStore {
    fn sink(&self, lf: LazyFrame, rel: &str) -> Result<(), FossilError> {
        let sub = self.root.join(rel);
        let target = SinkTarget::Path(sub.pl_path().clone());
        lf.sink_parquet(target, parquet_options(), sub.cloud_options().cloned(), SINK_OPTIONS)
            .map_err(polars_write_err)?
            .collect()
            .map_err(polars_write_err)?;
        Ok(())
    }

    fn scan(&self, rel: &str) -> Result<LazyFrame, RdfError> {
        let sub = self.root.join(rel);
        LazyFrame::scan_parquet(
            sub.pl_path().clone(),
            ScanArgsParquet {
                cloud_options: sub.cloud_options().cloned(),
                ..Default::default()
            },
        )
        .map_err(polars_write_err)
    }
}

/// Map XSD datatype IRI to a human-readable name for the manifest.
fn xsd_to_datatype_name(xsd: Option<&str>) -> &'static str {
    match xsd {
        Some(s) if s.ends_with("#integer") || s.ends_with("#int") || s.ends_with("#long") => "int64",
        Some(s) if s.ends_with("#float") || s.ends_with("#double") || s.ends_with("#decimal") => "double",
        Some(s) if s.ends_with("#boolean") => "boolean",
        Some(s) if s.ends_with("#date") => "date",
        _ => "string",
    }
}

/// Materialize a property graph as GraphAr-compatible Parquet files.
pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    resolved: &ResolvedPath,
) -> Result<DataManifest, FossilError> {
    let store = GraphStore { root: resolved.clone() };
    let mut types = Vec::new();
    let mut edges = Vec::new();

    // ── Pass 1: write all vertex files (edges may reference any type) ──
    for config in configs {
        let vertex = frame
            .clone()
            .select(config.selection.clone())
            .rename(["_subject"], ["subject"], true)
            .with_row_index("_id", Some(0));

        let vertex_path = format!("vertex/{}.parquet", config.type_dir);
        store.sink(vertex, &vertex_path)?;

        let type_manifest = compute_type_manifest(config, &vertex_path, &store)?;
        types.push(type_manifest);
    }

    // ── Pass 2: write edge files (all vertex files now exist) ──
    for config in configs {
        let vertex_path = format!("vertex/{}.parquet", config.type_dir);
        let id_map = store
            .scan(&vertex_path)?
            .select([col("_id"), col("subject")]);

        for ref_edge in &config.ref_edges {
            let edge_dir = format!(
                "{}_{}_{}",
                config.type_dir, ref_edge.label, ref_edge.target_type_dir
            );

            let raw_edges = frame
                .clone()
                .select([
                    config.subject_expr.clone().alias("src_iri"),
                    ref_edge.expr.clone().alias("tgt_iri"),
                ])
                .filter(col("tgt_iri").is_not_null().and(col("src_iri").is_not_null()));

            let with_src_id = raw_edges.join(
                id_map
                    .clone()
                    .rename(["subject", "_id"], ["src_iri", "source"], true),
                [col("src_iri")],
                [col("src_iri")],
                JoinType::Inner.into(),
            );

            let target_vertex_path =
                format!("vertex/{}.parquet", ref_edge.target_type_dir);
            let target_id_map = store.scan(&target_vertex_path)?.select([
                col("_id").alias("target"),
                col("subject").alias("tgt_iri"),
            ]);

            let edges_with_ids = with_src_id
                .join(
                    target_id_map,
                    [col("tgt_iri")],
                    [col("tgt_iri")],
                    JoinType::Inner.into(),
                )
                .select([col("source"), col("target")]);

            // CSR: sorted by (source, target)
            let by_source_path = format!("edge/{edge_dir}/by_source.parquet");
            store.sink(
                edges_with_ids
                    .clone()
                    .sort(
                        ["source", "target"],
                        SortMultipleOptions::default(),
                    ),
                &by_source_path,
            )?;

            // CSC: sorted by (target, source)
            let by_target_path = format!("edge/{edge_dir}/by_target.parquet");
            store.sink(
                edges_with_ids.sort(
                    ["target", "source"],
                    SortMultipleOptions::default(),
                ),
                &by_target_path,
            )?;

            let edge_count = store
                .scan(&by_source_path)
                .ok()
                .and_then(|lf| lf.select([col("source").count().alias("n")]).collect().ok())
                .map(|df| extract_u64(&df, "n"))
                .unwrap_or(0);

            edges.push(EdgeManifest {
                name: ref_edge.label.clone(),
                iri: ref_edge.predicate_uri.clone(),
                source_type: config.type_dir.clone(),
                target_type: ref_edge.target_type_dir.clone(),
                by_source: by_source_path,
                by_target: by_target_path,
                count: edge_count,
            });
        }
    }

    // Write YAML metadata (local only)
    write_yaml_metadata(&store, &types, &edges)?;

    Ok(DataManifest { types, edges })
}

/// Compute type manifest by scanning the written vertex file.
/// Uses a single batched query for all column stats (avoids N+1 scans).
fn compute_type_manifest(
    config: &OutputConfig,
    vertex_rel_path: &str,
    store: &GraphStore,
) -> Result<TypeManifest, FossilError> {
    let mut lf = store.scan(vertex_rel_path)?;

    let schema = lf.collect_schema().map_err(polars_write_err)?;
    let label_to_iri = &config.label_to_iri;

    // Collect property column names (skip _id and subject)
    let prop_cols: Vec<String> = schema
        .iter()
        .map(|(name, _)| name.to_string())
        .filter(|name| name != "_id" && name != "subject")
        .collect();

    // Single batched query: entity_count + per-column count/n_unique/min/max
    let mut stat_exprs: Vec<Expr> = vec![col("_id").count().alias("__entity_count")];
    for name in &prop_cols {
        let c = col(PlSmallStr::from(name.as_str()));
        stat_exprs.push(c.clone().count().alias(PlSmallStr::from(format!("{name}__count").as_str())));
        stat_exprs.push(c.clone().n_unique().alias(PlSmallStr::from(format!("{name}__nunique").as_str())));
        stat_exprs.push(c.clone().cast(DataType::String).min().alias(PlSmallStr::from(format!("{name}__min").as_str())));
        stat_exprs.push(c.cast(DataType::String).max().alias(PlSmallStr::from(format!("{name}__max").as_str())));
    }
    let stats_df = lf.clone().select(stat_exprs).collect().map_err(polars_write_err)?;
    let entity_count = extract_u64(&stats_df, "__entity_count");

    // Single batched query for samples (5 per column)
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
        lf.select(sample_exprs).collect().ok()
    } else {
        None
    };

    // Build ColumnStats from the two DataFrames
    let columns: Vec<ColumnStat> = prop_cols
        .iter()
        .map(|name| {
            let iri = label_to_iri.get(name).cloned().unwrap_or_default();
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

            ColumnStat { name: name.clone(), iri, datatype, count, n_unique, min, max, samples }
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
    // For cloud storage, YAML writing is skipped (metadata lives in the manifest JSON).
    if store.root.pl_path().is_cloud_url() {
        return Ok(());
    }

    // graph.yml
    let vertex_ymls: Vec<String> = types
        .iter()
        .map(|t| format!("{}.vertex.yml", t.name))
        .collect();
    let edge_ymls: Vec<String> = edges
        .iter()
        .map(|e| format!("{}_{}_{}.edge.yml", e.source_type, e.name, e.target_type))
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

    // Vertex YMLs
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
            "type: {}\nchunk_size: 262144\nprefix: vertex/{}/\nproperty_groups:\n  - file_type: parquet\n    properties:\n{}\n{}\nversion: gar/v1\n",
            t.name, t.name, subject_prop, props.join("\n")
        );
        std::fs::write(
            store.root.join(&format!("{}.vertex.yml", t.name)).to_str(),
            yml,
        )
        .map_err(|e| RdfError::Write(e.to_string()))?;
    }

    // Edge YMLs
    for e in edges {
        let yml = format!(
            "src_type: {}\nedge_type: {}\ndst_type: {}\nchunk_size: 4194304\nsrc_chunk_size: 262144\ndst_chunk_size: 262144\ndirected: true\nprefix: edge/{}_{}_{}/\nadj_lists:\n  - ordered: true\n    aligned_by: src\n    file_type: parquet\n  - ordered: true\n    aligned_by: dst\n    file_type: parquet\nversion: gar/v1\n",
            e.source_type, e.name, e.target_type,
            e.source_type, e.name, e.target_type,
        );
        std::fs::write(
            store.root.join(&format!(
                "{}_{}_{}.edge.yml",
                e.source_type, e.name, e.target_type
            )).to_str(),
            yml,
        )
        .map_err(|e2| RdfError::Write(e2.to_string()))?;
    }

    Ok(())
}
