//! Parquet RDF materializer — produces HDT-style indices as static Parquet files.
//!
//! Everything stays as `LazyFrame` until `sink_parquet` flushes to disk/cloud.
//! A single `.cache()` ensures the source is scanned only once.
//!
//! Output layout:
//! ```text
//! dataset/
//! ├── _subjects.parquet    # SPO: sorted by subject
//! ├── _objects.parquet     # OPS: sorted by object
//! ├── _predicates/         # PSO: 1 file per predicate
//! │   ├── height.parquet
//! │   └── type.parquet
//! ├── _meta.parquet        # Per-predicate statistics
//! └── _types.parquet       # Type IRI → directory mapping
//! ```

use std::collections::HashMap;

use polars::prelude::*;
use polars::prelude::cloud::CloudOptions;
use polars::prelude::sync_on_close::SyncOnCloseType;

use fossil_lang::error::FossilError;
use fossil_lang::runtime::executor::{PredicateStat, RdfManifest};

use super::OutputConfig;
use super::RdfError;

const RDF_TYPE_IRI: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

const SINK_OPTIONS: SinkOptions = SinkOptions {
    mkdir: true,
    maintain_order: true,
    sync_on_close: SyncOnCloseType::None,
};

fn polars_write_err(e: PolarsError) -> RdfError {
    RdfError::Write(e.to_string())
}

fn sink(lf: LazyFrame, path: &str, cloud_opts: Option<CloudOptions>) -> Result<(), FossilError> {
    let target = SinkTarget::Path(PlPath::from_str(path));
    let options = ParquetWriteOptions {
        row_group_size: Some(50_000),
        ..Default::default()
    };
    lf.sink_parquet(target, options, cloud_opts, SINK_OPTIONS)
        .map_err(polars_write_err)?
        .collect()
        .map_err(polars_write_err)?;
    Ok(())
}

/// Materialize a LazyFrame into Parquet RDF indices.
/// Returns an `RdfManifest` with relative file paths and per-predicate statistics.
pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    base_path: &str,
    cloud_opts: Option<CloudOptions>,
) -> Result<RdfManifest, FossilError> {
    let triple_frames: Vec<LazyFrame> = configs
        .iter()
        .map(|config| triplify(frame, config))
        .collect::<Result<_, _>>()?;

    if triple_frames.is_empty() {
        return Ok(RdfManifest {
            subjects: "_subjects.parquet".into(),
            objects: "_objects.parquet".into(),
            types: "_types.parquet".into(),
            predicates: Vec::new(),
            meta: Vec::new(),
            entity_count: 0,
        });
    }

    // Single cache — source scanned once, shared across all sinks
    let triples = concat(&triple_frames, UnionArgs::default())
        .map_err(polars_write_err)?
        .cache();

    // SPO index
    sink(
        triples.clone().sort(["subject"], Default::default()),
        &format!("{base_path}/_subjects.parquet"),
        cloud_opts.clone(),
    )?;

    // OPS index
    sink(
        triples
            .clone()
            .sort(["object_datatype", "object"], Default::default()),
        &format!("{base_path}/_objects.parquet"),
        cloud_opts.clone(),
    )?;

    // PSO indices (one file per predicate)
    let pred_names = predicate_names(configs);
    let mut predicate_paths = Vec::with_capacity(pred_names.len());
    for pred in &pred_names {
        let filename = predicate_to_filename(pred);
        let rel = format!("_predicates/{filename}.parquet");
        sink(
            triples
                .clone()
                .filter(col("predicate").eq(lit(pred.as_str())))
                .select([col("subject"), col("object")]),
            &format!("{base_path}/{rel}"),
            cloud_opts.clone(),
        )?;
        predicate_paths.push(rel);
    }

    // Type mappings
    sink(
        DataFrame::new(vec![
            Column::new(
                "type_iri".into(),
                &configs
                    .iter()
                    .map(|c| c.type_iri.as_str())
                    .collect::<Vec<_>>(),
            ),
            Column::new(
                "type_dir".into(),
                &configs
                    .iter()
                    .map(|c| c.type_dir.as_str())
                    .collect::<Vec<_>>(),
            ),
        ])
        .map_err(polars_write_err)?
        .lazy(),
        &format!("{base_path}/_types.parquet"),
        cloud_opts.clone(),
    )?;

    // Build datatype map from configs
    let mut datatype_map: HashMap<String, String> = HashMap::new();
    datatype_map.insert(RDF_TYPE_IRI.to_string(), "uri".to_string());
    for config in configs {
        for (pred, xsd) in &config.xsd_types {
            datatype_map.insert(pred.clone(), xsd.to_string());
        }
        for pred in &config.ref_predicates {
            datatype_map.insert(pred.clone(), "uri".to_string());
        }
    }

    // Predicate statistics — collect into DataFrame to extract manifest meta
    let meta_lf = triples
        .clone()
        .group_by([col("predicate")])
        .agg([
            col("object").count().alias("count"),
            col("object").n_unique().alias("n_unique"),
            col("object").min().alias("min"),
            col("object").max().alias("max"),
        ]);

    // Materialize meta into memory so we can both sink to parquet and extract stats
    let meta_df = meta_lf.collect().map_err(polars_write_err)?;

    // Sink meta to parquet
    sink(
        meta_df.clone().lazy(),
        &format!("{base_path}/_meta.parquet"),
        cloud_opts.clone(),
    )?;

    // Collect samples per predicate (5 representative values)
    let samples_lf = triples
        .clone()
        .group_by([col("predicate")])
        .agg([col("object").head(Some(5)).alias("samples")]);
    let samples_df = samples_lf.collect().map_err(polars_write_err)?;
    let mut samples_map: HashMap<String, Vec<String>> = HashMap::new();
    if let (Some(pred_col), Some(samp_col)) =
        (samples_df.column("predicate").ok(), samples_df.column("samples").ok())
    {
        for i in 0..samples_df.height() {
            let pred = pred_col.str().ok().and_then(|s| s.get(i)).unwrap_or("");
            let vals = if let Some(list_ca) = samp_col.list().ok() {
                if let Some(series) = list_ca.get_as_series(i) {
                    if let Ok(str_ca) = series.str() {
                        str_ca.into_iter()
                            .filter_map(|opt| opt.map(String::from))
                            .collect::<Vec<_>>()
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };
            samples_map.insert(pred.to_string(), vals);
        }
    }

    // Entity count — unique subjects
    let entity_count = triples
        .select([col("subject")])
        .unique(None, UniqueKeepStrategy::First)
        .collect()
        .map(|df| df.height() as u64)
        .unwrap_or(0);

    // Extract PredicateStats from the collected DataFrame
    let meta = extract_predicate_stats(&meta_df, &datatype_map, &samples_map);

    Ok(RdfManifest {
        subjects: "_subjects.parquet".into(),
        objects: "_objects.parquet".into(),
        types: "_types.parquet".into(),
        predicates: predicate_paths,
        meta,
        entity_count,
    })
}

/// Extract `Vec<PredicateStat>` from the meta DataFrame.
fn extract_predicate_stats(
    df: &DataFrame,
    datatype_map: &HashMap<String, String>,
    samples_map: &HashMap<String, Vec<String>>,
) -> Vec<PredicateStat> {
    let len = df.height();
    let pred_col = df.column("predicate").ok();
    let count_col = df.column("count").ok();
    let nunique_col = df.column("n_unique").ok();
    let min_col = df.column("min").ok();
    let max_col = df.column("max").ok();

    (0..len)
        .map(|i| {
            let predicate = pred_col
                .and_then(|c| c.str().ok())
                .and_then(|s| s.get(i))
                .unwrap_or("")
                .to_string();
            let count = count_col
                .and_then(|c| c.u32().ok().map(|s| s.get(i).unwrap_or(0) as u64)
                    .or_else(|| c.u64().ok().map(|s| s.get(i).unwrap_or(0))))
                .unwrap_or(0);
            let n_unique = nunique_col
                .and_then(|c| c.u32().ok().map(|s| s.get(i).unwrap_or(0) as u64)
                    .or_else(|| c.u64().ok().map(|s| s.get(i).unwrap_or(0))))
                .unwrap_or(0);
            let min = min_col
                .and_then(|c| c.str().ok())
                .and_then(|s| s.get(i))
                .map(String::from);
            let max = max_col
                .and_then(|c| c.str().ok())
                .and_then(|s| s.get(i))
                .map(String::from);
            let datatype = datatype_map.get(&predicate).cloned();
            let samples = samples_map.get(&predicate).cloned().unwrap_or_default();
            PredicateStat { predicate, count, n_unique, min, max, datatype, samples }
        })
        .collect()
}

/// Triplify an output config into (subject, predicate, object, object_datatype) rows.
fn triplify(plan_frame: &LazyFrame, config: &OutputConfig) -> Result<LazyFrame, FossilError> {
    let loc = fossil_lang::ast::Loc::generated();
    let selected = plan_frame.clone().select(config.selection.clone());

    let pred_cols: Vec<PlSmallStr> = config
        .selection
        .iter()
        .filter_map(|e| match e {
            Expr::Alias(_, name) if name.as_str() != "_subject" && name.as_str() != "_type" => {
                Some(name.clone())
            }
            _ => None,
        })
        .collect();

    let mut frames: Vec<LazyFrame> = Vec::new();

    // rdf:type triples
    let has_type = config
        .selection
        .iter()
        .any(|e| matches!(e, Expr::Alias(_, n) if n.as_str() == "_type"));
    if has_type {
        frames.push(
            selected
                .clone()
                .filter(col("_subject").is_not_null().and(col("_type").is_not_null()))
                .select([
                    col("_subject").alias("subject"),
                    lit(RDF_TYPE_IRI).alias("predicate"),
                    col("_type").alias("object"),
                    lit("uri").alias("object_datatype"),
                ]),
        );
    }

    // Per-predicate triples
    for pred_col in &pred_cols {
        let is_ref = config.ref_predicates.contains(pred_col.as_str());
        let xsd = config.xsd_types.get(pred_col.as_str()).copied();
        let datatype_str = if is_ref {
            "uri".to_string()
        } else {
            xsd.unwrap_or("").to_string()
        };

        frames.push(
            selected
                .clone()
                .filter(
                    col("_subject")
                        .is_not_null()
                        .and(col(pred_col.clone()).is_not_null()),
                )
                .select([
                    col("_subject").alias("subject"),
                    lit(pred_col.as_str()).alias("predicate"),
                    col(pred_col.clone())
                        .cast(DataType::String)
                        .alias("object"),
                    lit(datatype_str).alias("object_datatype"),
                ]),
        );
    }

    if frames.is_empty() {
        let empty = DataFrame::new(vec![
            Column::new_empty("subject".into(), &DataType::String),
            Column::new_empty("predicate".into(), &DataType::String),
            Column::new_empty("object".into(), &DataType::String),
            Column::new_empty("object_datatype".into(), &DataType::String),
        ])
        .map_err(|e| FossilError::evaluation(e.to_string(), loc))?;
        return Ok(empty.lazy());
    }

    concat(&frames, UnionArgs::default()).map_err(|e| FossilError::evaluation(e.to_string(), loc))
}

/// Collect predicate URIs statically from output configs.
fn predicate_names(configs: &[OutputConfig]) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for config in configs {
        if config
            .selection
            .iter()
            .any(|e| matches!(e, Expr::Alias(_, n) if n.as_str() == "_type"))
        {
            if !names.contains(&RDF_TYPE_IRI.to_string()) {
                names.push(RDF_TYPE_IRI.to_string());
            }
        }
        for e in &config.selection {
            if let Expr::Alias(_, name) = e {
                let s = name.to_string();
                if s != "_subject" && s != "_type" && !names.contains(&s) {
                    names.push(s);
                }
            }
        }
    }
    names
}

/// Convert a predicate URI to a safe filename.
fn predicate_to_filename(uri: &str) -> String {
    let name = uri
        .rsplit_once('#')
        .or_else(|| uri.rsplit_once('/'))
        .map(|(_, name)| name)
        .unwrap_or(uri);
    name.to_lowercase()
}
