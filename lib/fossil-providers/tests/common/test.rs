use std::collections::HashSet;
use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;
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
    let _ = IrExecutor::execute(result.program)?;

    let actual = read_parquet_triples(test_case);
    let expected = parse_ntriples_file(test_case)?;

    // If no Parquet was produced and expected is empty, that's correct
    match actual {
        Ok(triples) => Ok(triples == expected),
        Err(_) if expected.is_empty() => Ok(true),
        Err(e) => Err(e),
    }
}

/// Read triples from the materialized `_subjects.parquet`.
fn read_parquet_triples(test_case: &str) -> Result<HashSet<Triple>, TestSuiteError> {
    let path = format!("tests/{}/actual/_subjects.parquet", test_case);
    let df = LazyFrame::scan_parquet(PlPath::from_str(&path), Default::default())
        .map_err(|e| TestSuiteError::Other(format!("Failed to read {path}: {e}")))?
        .select([col("subject"), col("predicate"), col("object"), col("object_datatype")])
        .collect()
        .map_err(|e| TestSuiteError::Other(format!("Failed to collect {path}: {e}")))?;

    let subjects = df.column("subject").map_err(polars_err)?.str().map_err(polars_err)?;
    let predicates = df.column("predicate").map_err(polars_err)?.str().map_err(polars_err)?;
    let objects = df.column("object").map_err(polars_err)?.str().map_err(polars_err)?;
    let datatypes = df.column("object_datatype").map_err(polars_err)?.str().map_err(polars_err)?;

    let mut triples = HashSet::new();
    for i in 0..df.height() {
        let s = subjects.get(i).unwrap_or("");
        let p = predicates.get(i).unwrap_or("");
        let o = objects.get(i).unwrap_or("");
        let dt = datatypes.get(i).unwrap_or("");

        let object_str = format_object(o, dt);
        triples.insert(Triple {
            subject: format_subject(s),
            predicate: format!("<{p}>"),
            object: object_str,
        });
    }

    Ok(triples)
}

/// Format a subject: blank nodes as-is, URIs wrapped in `<>`.
fn format_subject(s: &str) -> String {
    if s.starts_with("_:") {
        s.to_string()
    } else {
        format!("<{s}>")
    }
}

/// Format an object value with its datatype.
fn format_object(value: &str, datatype: &str) -> String {
    if datatype == "uri" {
        // URI or blank node
        if value.starts_with("_:") {
            value.to_string()
        } else {
            format!("<{value}>")
        }
    } else if datatype.is_empty() {
        // Plain string literal
        format!("\"{}\"", value)
    } else {
        // Typed literal
        format!("\"{}\"^^<{}>", value, datatype)
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
///
/// Format: `<subject> <predicate> <object> .` or `<subject> <predicate> "literal"^^<type> .`
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
        // URI: <...>
        let end = s.find('>')?;
        Some((s[..=end].to_string(), &s[end + 1..]))
    } else if s.starts_with("_:") {
        // Blank node: _:name
        let end = s[2..]
            .find(|c: char| c.is_whitespace())
            .map(|i| i + 2)
            .unwrap_or(s.len());
        Some((s[..end].to_string(), &s[end..]))
    } else if s.starts_with('"') {
        // Literal: "value" or "value"^^<type> or "value"@lang
        // N-Triples escape sequences are always ASCII (\", \\, \n, \r, \t, \uXXXX, \UXXXXXXXX)
        // so i += 2 is safe for valid N-Triples. Guard against malformed input anyway.
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
            return None; // unterminated string
        }
        let after_quote = &s[i + 1..];
        let full = if after_quote.starts_with("^^") {
            // Typed literal
            let type_part = &after_quote[2..];
            if type_part.starts_with('<') {
                let type_end = type_part.find('>')?;
                let end = i + 1 + 2 + type_end + 1;
                s[..end].to_string()
            } else {
                s[..=i].to_string()
            }
        } else if after_quote.starts_with('@') {
            // Language-tagged literal
            let lang_end = after_quote[1..]
                .find(|c: char| c.is_whitespace())
                .map(|j| i + 1 + 1 + j)
                .unwrap_or(s.len());
            s[..lang_end].to_string()
        } else {
            // Simple literal
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
