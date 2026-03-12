use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::io::Write;

use polars::prelude::*;
use serde::{Deserialize, Serialize};

use fossil_lang::error::FossilError;
use fossil_lang::runtime::chunked_executor::ChunkedExecutor;
use fossil_lang::runtime::output::OutputResolver;
use fossil_lang::runtime::value::Plan;

use super::RdfError;

const RDF_TYPE_IRI: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// Output configuration for a single RDF type's fragment writing.
pub struct OutputConfig {
    /// Polars selection expressions producing `_subject`, `_type`, and predicate columns.
    pub selection: Vec<Expr>,
    /// Predicate URI → XSD datatype IRI for typed literals.
    pub xsd_types: HashMap<String, &'static str>,
    /// Predicate URIs whose values are references (URIs/blank nodes, not literals).
    pub ref_predicates: HashSet<String>,
    /// Directory name for this type's fragments (e.g., `"wall"`).
    pub type_dir: String,
    /// Full RDF type IRI (e.g., `"http://example.com/bim#Wall"`).
    pub type_iri: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FragmentManifest {
    pub version: u32,
    pub fragment_size: usize,
    pub total_triples: u64,
    pub types: HashMap<String, TypeManifest>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeManifest {
    pub dir: String,
    pub fragments: u32,
    pub total_triples: u64,
    pub total_subjects: u64,
    pub predicates: HashMap<String, u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FragmentProfile {
    pub total_triples: u64,
    pub total_subjects: u64,
    pub types: Vec<ProfileType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileType {
    pub iri: String,
    pub triples: u64,
    pub subjects: u64,
    pub predicates: Vec<ProfilePredicate>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfilePredicate {
    pub iri: String,
    pub count: u64,
}

struct TypeWriteState {
    fragment_index: u32,
    total_triples: u64,
    total_subjects: u64,
    predicate_counts: HashMap<String, u64>,
}

struct BatchStats {
    triples: u64,
    subjects: u64,
    predicate_counts: HashMap<String, u64>,
}

/// Write RDF fragments from a plan using columnar N-Triples serialization.
///
/// For each output type, executes the plan in batches (`fragment_size` rows per batch),
/// serializes each batch to N-Triples using vectorized Polars operations, and writes
/// each batch as a numbered `.nt` fragment file.
///
/// Also writes `manifest.json` and `profile.json` to the base path.
pub fn write_fragments(
    plan: &Plan,
    configs: &[OutputConfig],
    base_path: &str,
    fragment_size: usize,
    output_resolver: &dyn OutputResolver,
) -> Result<FragmentManifest, FossilError> {
    let executor = ChunkedExecutor::new(fragment_size);

    let states: Vec<RefCell<TypeWriteState>> = configs
        .iter()
        .map(|_| {
            RefCell::new(TypeWriteState {
                fragment_index: 0,
                total_triples: 0,
                total_subjects: 0,
                predicate_counts: HashMap::new(),
            })
        })
        .collect();

    executor
        .execute_plan_batched(plan, |batch| {
            let lazy_batch = batch.clone().lazy();

            for (i, config) in configs.iter().enumerate() {
                if config.selection.is_empty() {
                    continue;
                }

                let rdf_batch = lazy_batch
                    .clone()
                    .select(config.selection.clone())
                    .collect()
                    .map_err(|e| {
                        PolarsError::ComputeError(
                            format!("Failed to apply RDF selection: {e}").into(),
                        )
                    })?;

                if rdf_batch.height() == 0 {
                    continue;
                }

                let (nt_bytes, batch_stats) =
                    batch_to_nt(&rdf_batch, &config.xsd_types, &config.ref_predicates)?;

                if nt_bytes.is_empty() {
                    continue;
                }

                let mut state = states[i].borrow_mut();
                let path = format!(
                    "{base_path}/{}/{:06}.nt",
                    config.type_dir, state.fragment_index
                );
                let mut dest = output_resolver.resolve_output(&path).map_err(|e| {
                    PolarsError::ComputeError(
                        format!("Failed to resolve output {path}: {e}").into(),
                    )
                })?;
                dest.writer.write_all(&nt_bytes).map_err(|e| {
                    PolarsError::ComputeError(
                        format!("Failed to write fragment {path}: {e}").into(),
                    )
                })?;
                dest.writer.flush().map_err(|e| {
                    PolarsError::ComputeError(
                        format!("Failed to flush fragment {path}: {e}").into(),
                    )
                })?;

                state.fragment_index += 1;
                state.total_triples += batch_stats.triples;
                state.total_subjects += batch_stats.subjects;
                for (pred, count) in &batch_stats.predicate_counts {
                    *state.predicate_counts.entry(pred.clone()).or_default() += count;
                }
            }

            Ok(())
        })
        .map_err(|e| RdfError::Write(e.to_string()))?;

    // Build manifest from accumulated states
    let mut manifest = FragmentManifest {
        version: 1,
        fragment_size,
        total_triples: 0,
        types: HashMap::new(),
    };

    for (i, config) in configs.iter().enumerate() {
        let state = states[i].borrow();
        if state.fragment_index == 0 {
            continue;
        }
        let type_manifest = TypeManifest {
            dir: config.type_dir.clone(),
            fragments: state.fragment_index,
            total_triples: state.total_triples,
            total_subjects: state.total_subjects,
            predicates: state.predicate_counts.clone(),
        };
        manifest.total_triples += state.total_triples;
        manifest.types.insert(config.type_iri.clone(), type_manifest);
    }

    // Write manifest.json
    write_json(output_resolver, &format!("{base_path}/manifest.json"), &manifest)?;

    // Write profile.json
    let profile = build_profile(&manifest);
    write_json(output_resolver, &format!("{base_path}/profile.json"), &profile)?;

    Ok(manifest)
}

fn write_json<T: Serialize>(
    output_resolver: &dyn OutputResolver,
    path: &str,
    value: &T,
) -> Result<(), FossilError> {
    let json = serde_json::to_vec_pretty(value)
        .map_err(|e| RdfError::Write(format!("Failed to serialize JSON: {e}")))?;
    let mut dest = output_resolver
        .resolve_output(path)
        .map_err(|e| RdfError::Write(format!("Failed to resolve output {path}: {e}")))?;
    dest.writer
        .write_all(&json)
        .map_err(|e| RdfError::Write(e.to_string()))?;
    Ok(())
}

/// Convert a DataFrame batch to N-Triples bytes using columnar Polars operations.
///
/// Instead of iterating row-by-row through a TripleWriter, this builds complete
/// N-Triples lines using vectorized string operations (concat_str, str.replace_all)
/// and writes the resulting bytes in bulk.
fn batch_to_nt(
    batch: &DataFrame,
    xsd_types: &HashMap<String, &'static str>,
    ref_predicates: &HashSet<String>,
) -> PolarsResult<(Vec<u8>, BatchStats)> {
    // Filter null subjects
    let df = batch
        .clone()
        .lazy()
        .filter(col("_subject").is_not_null())
        .collect()?;

    if df.height() == 0 {
        return Ok((
            Vec::new(),
            BatchStats {
                triples: 0,
                subjects: 0,
                predicate_counts: HashMap::new(),
            },
        ));
    }

    let subject_count = df.height() as u64;

    // Format subject: blank nodes as-is, URIs wrapped in <>
    let df = df
        .lazy()
        .with_column(
            when(col("_subject").str().starts_with(lit("_:")))
                .then(col("_subject"))
                .otherwise(concat_str([lit("<"), col("_subject"), lit(">")], "", false))
                .alias("_fmt_subject"),
        )
        .collect()?;

    let mut output = Vec::new();
    let mut stats = BatchStats {
        triples: 0,
        subjects: subject_count,
        predicate_counts: HashMap::new(),
    };

    // rdf:type triples
    if df.column("_type").is_ok() {
        let lines = df
            .clone()
            .lazy()
            .filter(col("_type").is_not_null())
            .select([concat_str(
                [
                    col("_fmt_subject"),
                    lit(format!(" <{RDF_TYPE_IRI}> <")),
                    col("_type"),
                    lit("> ."),
                ],
                "",
                false,
            )
            .alias("_line")])
            .collect()?;

        let count = write_lines_to_buf(&lines, &mut output)?;
        if count > 0 {
            stats.triples += count;
            stats
                .predicate_counts
                .insert(RDF_TYPE_IRI.to_string(), count);
        }
    }

    // Predicate columns (everything except special columns)
    let predicate_cols: Vec<String> = df
        .get_column_names()
        .into_iter()
        .filter(|n| {
            let s = n.as_str();
            s != "_subject" && s != "_type" && s != "_graph" && s != "_fmt_subject"
        })
        .map(|n| n.to_string())
        .collect();

    for col_name in &predicate_cols {
        let pred = col_name
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
            .unwrap_or(col_name);

        let is_ref = ref_predicates.contains(col_name);
        let xsd = xsd_types.get(col_name.as_str()).copied();

        let lines = if is_ref {
            build_ref_lines(&df, col_name, pred)?
        } else {
            build_literal_lines(&df, col_name, pred, xsd)?
        };

        let count = write_lines_to_buf(&lines, &mut output)?;
        if count > 0 {
            stats.triples += count;
            stats.predicate_counts.insert(pred.to_string(), count);
        }
    }

    Ok((output, stats))
}

/// Build NT lines for a reference predicate (object is a URI or blank node).
fn build_ref_lines(df: &DataFrame, col_name: &str, pred: &str) -> PolarsResult<DataFrame> {
    df.clone()
        .lazy()
        .filter(col(col_name).is_not_null())
        .with_column(
            when(
                col(col_name)
                    .cast(DataType::String)
                    .str()
                    .starts_with(lit("_:")),
            )
            .then(col(col_name).cast(DataType::String))
            .otherwise(concat_str(
                [lit("<"), col(col_name).cast(DataType::String), lit(">")],
                "",
                false,
            ))
            .alias("_fmt_obj"),
        )
        .select([concat_str(
            [
                col("_fmt_subject"),
                lit(format!(" <{pred}> ")),
                col("_fmt_obj"),
                lit(" ."),
            ],
            "",
            false,
        )
        .alias("_line")])
        .collect()
}

/// Build NT lines for a literal predicate, optionally with an XSD datatype.
fn build_literal_lines(
    df: &DataFrame,
    col_name: &str,
    pred: &str,
    xsd: Option<&str>,
) -> PolarsResult<DataFrame> {
    let escaped = escape_nt_expr(col(col_name));

    let line_expr = match xsd {
        Some(xsd_iri) => concat_str(
            [
                col("_fmt_subject"),
                lit(format!(" <{pred}> \"")),
                escaped,
                lit(format!("\"^^<{xsd_iri}> .")),
            ],
            "",
            false,
        ),
        None => concat_str(
            [
                col("_fmt_subject"),
                lit(format!(" <{pred}> \"")),
                escaped,
                lit("\" ."),
            ],
            "",
            false,
        ),
    };

    df.clone()
        .lazy()
        .filter(col(col_name).is_not_null())
        .select([line_expr.alias("_line")])
        .collect()
}

/// Apply N-Triples escaping to a string expression (vectorized).
///
/// Escapes: `\` → `\\`, `"` → `\"`, newline → `\n`, CR → `\r`, tab → `\t`.
/// Order matters: backslash must be escaped first to avoid double-escaping.
fn escape_nt_expr(expr: Expr) -> Expr {
    expr.cast(DataType::String)
        .str()
        .replace_all(lit("\\"), lit("\\\\"), true)
        .str()
        .replace_all(lit("\""), lit("\\\""), true)
        .str()
        .replace_all(lit("\n"), lit("\\n"), true)
        .str()
        .replace_all(lit("\r"), lit("\\r"), true)
        .str()
        .replace_all(lit("\t"), lit("\\t"), true)
}

/// Write all lines from a DataFrame's `_line` column to a byte buffer.
/// Returns the number of lines written.
fn write_lines_to_buf(df: &DataFrame, buf: &mut Vec<u8>) -> PolarsResult<u64> {
    if df.height() == 0 {
        return Ok(0);
    }
    let lines = df.column("_line")?.str()?;
    let mut count = 0u64;
    for val in lines.into_iter().flatten() {
        buf.extend_from_slice(val.as_bytes());
        buf.push(b'\n');
        count += 1;
    }
    Ok(count)
}

fn build_profile(manifest: &FragmentManifest) -> FragmentProfile {
    let mut total_subjects = 0u64;
    let types: Vec<ProfileType> = manifest
        .types
        .iter()
        .map(|(iri, tm)| {
            total_subjects += tm.total_subjects;
            ProfileType {
                iri: iri.clone(),
                triples: tm.total_triples,
                subjects: tm.total_subjects,
                predicates: tm
                    .predicates
                    .iter()
                    .map(|(pred, count)| ProfilePredicate {
                        iri: pred.clone(),
                        count: *count,
                    })
                    .collect(),
            }
        })
        .collect();

    FragmentProfile {
        total_triples: manifest.total_triples,
        total_subjects,
        types,
    }
}
