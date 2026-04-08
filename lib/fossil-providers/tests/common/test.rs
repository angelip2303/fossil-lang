use std::collections::HashSet;
use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;
use fossil_rdf::DataManifest;
use polars::prelude::*;

use crate::TestSuiteError;

/// A simple RDF triple for comparison (subject, predicate, object as formatted string).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Triple {
    subject: String,
    predicate: String,
    object: String,
}

fn test(test_case: &str) -> Result<bool, TestSuiteError> {
    let mut gcx = GlobalContext::default();
    fossil_providers::init(&mut gcx);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let mapping = PathBuf::from(format!("tests/{}/mapping.fossil", test_case));
    let result = compiler.compile(CompilerInput::File(mapping))?;
    let exec_result = IrExecutor::execute(result.program)?;

    // Find the rdf-parquet output and read its manifest from disk
    let base_path = exec_result
        .outputs
        .iter()
        .find(|o| o.kind == fossil_rdf::OUTPUT_KIND)
        .map(|o| o.path.as_str())
        .unwrap_or("");

    let manifest = if !base_path.is_empty() {
        let manifest_path = format!("{base_path}/_manifest.json");
        std::fs::read_to_string(&manifest_path)
            .ok()
            .and_then(|json| serde_json::from_str::<DataManifest>(&json).ok())
    } else {
        None
    };

    let actual = match manifest.as_ref() {
        Some(m) => reconstruct_triples(base_path, m),
        None => Ok(HashSet::new()),
    };
    let expected = parse_ntriples_file(test_case)?;

    match actual {
        Ok(triples) => Ok(triples == expected),
        Err(_) if expected.is_empty() => Ok(true),
        Err(e) => Err(e),
    }
}

/// Reconstruct RDF triples from GraphAr vertex + edge files.
fn reconstruct_triples(
    base_path: &str,
    manifest: &DataManifest,
) -> Result<HashSet<Triple>, TestSuiteError> {
    let mut triples = HashSet::new();

    for type_manifest in &manifest.types {
        let path = format!("{}/{}", base_path, type_manifest.vertex_file);
        let df = LazyFrame::scan_parquet(PlPath::from_str(&path), Default::default())
            .map_err(|e| TestSuiteError::Other(format!("Failed to read {path}: {e}")))?
            .collect()
            .map_err(|e| TestSuiteError::Other(format!("Failed to collect {path}: {e}")))?;

        let subjects = df
            .column("subject")
            .map_err(polars_err)?
            .str()
            .map_err(polars_err)?;

        // rdf:type triples — only when the IRI is a proper URI (not just the type name)
        if type_manifest.iri.contains("://") || type_manifest.iri.contains('#') {
            for i in 0..df.height() {
                let s = subjects.get(i).unwrap_or("");
                if !s.is_empty() {
                    triples.insert(Triple {
                        subject: format_subject(s),
                        predicate: "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".into(),
                        object: format!("<{}>", type_manifest.iri),
                    });
                }
            }
        }

        // Property triples (scalar columns)
        for col_stat in &type_manifest.columns {
            let col_data = df
                .column(&col_stat.name)
                .map_err(polars_err)?
                .cast(&DataType::String)
                .map_err(polars_err)?;
            let str_col = col_data.str().map_err(polars_err)?;

            for i in 0..df.height() {
                let s = subjects.get(i).unwrap_or("");
                if let Some(val) = str_col.get(i) {
                    if !s.is_empty() {
                        let object_str = if col_stat.datatype == "string" || col_stat.iri.is_empty()
                        {
                            format!("\"{}\"", val)
                        } else {
                            // Map datatype name back to XSD IRI
                            let xsd = match col_stat.datatype.as_str() {
                                "int64" => "http://www.w3.org/2001/XMLSchema#integer",
                                "double" => "http://www.w3.org/2001/XMLSchema#double",
                                "boolean" => "http://www.w3.org/2001/XMLSchema#boolean",
                                "date" => "http://www.w3.org/2001/XMLSchema#date",
                                _ => "",
                            };
                            if xsd.is_empty() {
                                format!("\"{}\"", val)
                            } else {
                                format!("\"{}\"^^<{}>", val, xsd)
                            }
                        };
                        triples.insert(Triple {
                            subject: format_subject(s),
                            predicate: format!("<{}>", col_stat.iri),
                            object: object_str,
                        });
                    }
                }
            }
        }
    }

    // Edge triples (ref columns → URI objects)
    for edge_manifest in &manifest.edges {
        let path = format!("{}/{}", base_path, edge_manifest.by_source);
        let edge_df = LazyFrame::scan_parquet(PlPath::from_str(&path), Default::default())
            .map_err(|e| TestSuiteError::Other(format!("Failed to read {path}: {e}")))?
            .collect()
            .map_err(|e| TestSuiteError::Other(format!("Failed to collect {path}: {e}")))?;

        // Load vertex ID→IRI maps for source and target types
        let src_vertex_path = manifest
            .types
            .iter()
            .find(|t| t.name == edge_manifest.source_type)
            .map(|t| format!("{}/{}", base_path, t.vertex_file))
            .ok_or_else(|| {
                TestSuiteError::Other(format!(
                    "Source type {} not found",
                    edge_manifest.source_type
                ))
            })?;
        let tgt_vertex_path = manifest
            .types
            .iter()
            .find(|t| t.name == edge_manifest.target_type)
            .map(|t| format!("{}/{}", base_path, t.vertex_file))
            .ok_or_else(|| {
                TestSuiteError::Other(format!(
                    "Target type {} not found",
                    edge_manifest.target_type
                ))
            })?;

        let src_map = build_id_to_iri_map(&src_vertex_path)?;
        let tgt_map = build_id_to_iri_map(&tgt_vertex_path)?;

        let sources = edge_df
            .column("source")
            .map_err(polars_err)?;
        let targets = edge_df
            .column("target")
            .map_err(polars_err)?;

        for i in 0..edge_df.height() {
            let src_id = sources
                .i64()
                .ok()
                .and_then(|ca| ca.get(i))
                .or_else(|| {
                    sources
                        .u32()
                        .ok()
                        .and_then(|ca| ca.get(i).map(|v| v as i64))
                })
                .unwrap_or(-1);
            let tgt_id = targets
                .i64()
                .ok()
                .and_then(|ca| ca.get(i))
                .or_else(|| {
                    targets
                        .u32()
                        .ok()
                        .and_then(|ca| ca.get(i).map(|v| v as i64))
                })
                .unwrap_or(-1);

            if let (Some(src_iri), Some(tgt_iri)) = (
                src_map.get(&(src_id as u64)),
                tgt_map.get(&(tgt_id as u64)),
            ) {
                triples.insert(Triple {
                    subject: format_subject(src_iri),
                    predicate: format!("<{}>", edge_manifest.iri),
                    object: format_subject(tgt_iri), // URIs for refs
                });
            }
        }
    }

    Ok(triples)
}

fn build_id_to_iri_map(
    vertex_path: &str,
) -> Result<std::collections::HashMap<u64, String>, TestSuiteError> {
    let df = LazyFrame::scan_parquet(PlPath::from_str(vertex_path), Default::default())
        .map_err(|e| TestSuiteError::Other(format!("Failed to read {vertex_path}: {e}")))?
        .select([col("_id"), col("subject")])
        .collect()
        .map_err(|e| TestSuiteError::Other(format!("Failed to collect {vertex_path}: {e}")))?;

    let ids = df.column("_id").map_err(polars_err)?;
    let subjects = df
        .column("subject")
        .map_err(polars_err)?
        .str()
        .map_err(polars_err)?;

    let mut map = std::collections::HashMap::new();
    for i in 0..df.height() {
        let id = ids
            .u32()
            .ok()
            .and_then(|ca| ca.get(i).map(|v| v as u64))
            .or_else(|| ids.u64().ok().and_then(|ca| ca.get(i)))
            .unwrap_or(0);
        let iri = subjects.get(i).unwrap_or("").to_string();
        map.insert(id, iri);
    }
    Ok(map)
}

/// Format a subject: blank nodes as-is, URIs wrapped in `<>`.
fn format_subject(s: &str) -> String {
    if s.starts_with("_:") {
        s.to_string()
    } else {
        format!("<{s}>")
    }
}

/// Parse an N-Triples/N-Quads file into a set of triples.
fn parse_ntriples_file(test_case: &str) -> Result<HashSet<Triple>, TestSuiteError> {
    let path = format!("tests/{}/output.nq", test_case);
    let content = std::fs::read_to_string(&path)?;
    let mut triples = HashSet::new();

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some(triple) = parse_ntriples_line(line) {
            triples.insert(triple);
        }
    }

    Ok(triples)
}

/// Parse a single N-Triples line into a Triple.
fn parse_ntriples_line(line: &str) -> Option<Triple> {
    let line = line.trim().strip_suffix('.')?;
    let line = line.trim();

    let (subject, rest) = parse_nt_term(line)?;
    let rest = rest.trim_start();
    let (predicate, rest) = parse_nt_term(rest)?;
    let rest = rest.trim_start();
    let (object, _) = parse_nt_term(rest)?;

    Some(Triple {
        subject,
        predicate,
        object,
    })
}

/// Parse a single N-Triples term (URI, blank node, or literal).
fn parse_nt_term(s: &str) -> Option<(String, &str)> {
    let s = s.trim_start();
    if s.starts_with('<') {
        let end = s.find('>')?;
        Some((s[..=end].to_string(), &s[end + 1..]))
    } else if s.starts_with("_:") {
        let end = s[2..]
            .find(|c: char| c.is_whitespace())
            .map(|i| i + 2)
            .unwrap_or(s.len());
        Some((s[..end].to_string(), &s[end..]))
    } else if s.starts_with('"') {
        let mut i = 1;
        let bytes = s.as_bytes();
        while i < bytes.len() {
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                i += 2;
            } else if bytes[i] == b'"' {
                break;
            } else {
                i += 1;
            }
        }
        if i >= bytes.len() {
            return None;
        }
        let after_quote = &s[i + 1..];
        let full = if after_quote.starts_with("^^") {
            let type_part = &after_quote[2..];
            if type_part.starts_with('<') {
                let type_end = type_part.find('>')?;
                let end = i + 1 + 2 + type_end + 1;
                s[..end].to_string()
            } else {
                s[..=i].to_string()
            }
        } else if after_quote.starts_with('@') {
            let lang_end = after_quote[1..]
                .find(|c: char| c.is_whitespace())
                .map(|j| i + 1 + 1 + j)
                .unwrap_or(s.len());
            s[..lang_end].to_string()
        } else {
            s[..=i].to_string()
        };
        let consumed = full.len();
        Some((full, &s[consumed..]))
    } else {
        None
    }
}

fn polars_err(e: PolarsError) -> TestSuiteError {
    TestSuiteError::Other(e.to_string())
}

fn cleanup_actual_output(test_case: &str) {
    let actual_dir = format!("tests/{}/actual", test_case);
    let _ = std::fs::remove_dir_all(&actual_dir);
}

pub fn test_positive(test_case: &str) -> Result<(), TestSuiteError> {
    cleanup_actual_output(test_case);
    match test(test_case)? {
        true => Ok(()),
        false => Err(TestSuiteError::NotEquals),
    }
}

pub fn test_negative(test_case: &str) -> Result<(), TestSuiteError> {
    cleanup_actual_output(test_case);
    match test(test_case) {
        Ok(_) => Err(TestSuiteError::NotEquals),
        Err(_) => Ok(()),
    }
}
