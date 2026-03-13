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
use polars::prelude::sync_on_close::SyncOnCloseType;

use fossil_lang::error::FossilError;

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

fn sink(lf: LazyFrame, path: &str, _creds: &HashMap<String, String>) -> Result<(), FossilError> {
    let target = SinkTarget::Path(PlPath::from_str(path));
    let options = ParquetWriteOptions {
        row_group_size: Some(50_000),
        ..Default::default()
    };
    lf.sink_parquet(target, options, None, SINK_OPTIONS)
        .map_err(polars_write_err)?
        .collect()
        .map_err(polars_write_err)?;
    Ok(())
}

/// Materialize a LazyFrame into Parquet RDF indices.
pub fn materialize(
    frame: &LazyFrame,
    configs: &[OutputConfig],
    base_path: &str,
    creds: &HashMap<String, String>,
) -> Result<(), FossilError> {
    let triple_frames: Vec<LazyFrame> = configs
        .iter()
        .map(|config| triplify(frame, config))
        .collect::<Result<_, _>>()?;

    if triple_frames.is_empty() {
        return Ok(());
    }

    // Single cache — source scanned once, shared across all sinks
    let triples = concat(&triple_frames, UnionArgs::default())
        .map_err(polars_write_err)?
        .cache();

    // SPO index
    sink(
        triples.clone().sort(["subject"], Default::default()),
        &format!("{base_path}/_subjects.parquet"),
        creds,
    )?;

    // OPS index
    sink(
        triples
            .clone()
            .sort(["object_datatype", "object"], Default::default()),
        &format!("{base_path}/_objects.parquet"),
        creds,
    )?;

    // PSO indices (one file per predicate)
    for pred in &predicate_names(configs) {
        sink(
            triples
                .clone()
                .filter(col("predicate").eq(lit(pred.as_str())))
                .select([col("subject"), col("object")]),
            &format!(
                "{base_path}/_predicates/{}.parquet",
                predicate_to_filename(pred)
            ),
            creds,
        )?;
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
        creds,
    )?;

    // Predicate statistics (last use — no clone needed)
    sink(
        triples
            .group_by([col("predicate")])
            .agg([
                col("object").count().alias("count"),
                col("object").n_unique().alias("n_unique"),
                col("object").min().alias("min"),
                col("object").max().alias("max"),
            ]),
        &format!("{base_path}/_meta.parquet"),
        creds,
    )?;

    Ok(())
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
