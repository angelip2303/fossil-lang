use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use oxrdf::{BlankNode, GraphNameRef, Literal, NamedNode, NamedOrBlankNode, QuadRef, Term};
use oxrdfio::{RdfFormat, RdfSerializer, WriterQuadSerializer};
use polars::prelude::*;

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const BNODE_PREFIX: &str = "_:";

/// Parsed subject with pre-computed type for efficient iteration
enum ParsedSubject {
    BlankNode(BlankNode),
    NamedNode(NamedNode),
}

impl ParsedSubject {
    fn as_ref(&self) -> NamedOrBlankNode {
        match self {
            ParsedSubject::BlankNode(b) => b.clone().into(),
            ParsedSubject::NamedNode(n) => n.clone().into(),
        }
    }
}

#[inline]
fn clean_subject(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

fn parse_subject_cleaned(s: &str) -> Result<ParsedSubject, PolarsError> {
    if let Some(id) = s.strip_prefix(BNODE_PREFIX) {
        let bnode = BlankNode::new(id)
            .map_err(|_| PolarsError::ComputeError(format!("Invalid blank node: {s}").into()))?;
        return Ok(ParsedSubject::BlankNode(bnode));
    }

    let node = NamedNode::new(s)
        .map_err(|_| PolarsError::ComputeError(format!("Invalid subject URI: {s}").into()))?;
    Ok(ParsedSubject::NamedNode(node))
}

fn parse_predicate(s: &str) -> Result<NamedNode, PolarsError> {
    let uri = s
        .strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s);
    NamedNode::new(uri)
        .map_err(|_| PolarsError::ComputeError(format!("Invalid predicate: {s}").into()))
}

#[inline]
fn parse_object(s: &str) -> Result<Term, PolarsError> {
    if let Some(id) = s.strip_prefix(BNODE_PREFIX) {
        let bnode = BlankNode::new(id)
            .map_err(|_| PolarsError::ComputeError(format!("Invalid blank node: {s}").into()))?;
        return Ok(bnode.into());
    }

    if let Some(uri) = s.strip_prefix('<').and_then(|s| s.strip_suffix('>')) {
        let node = NamedNode::new(uri)
            .map_err(|_| PolarsError::ComputeError(format!("Invalid URI: {s}").into()))?;
        return Ok(node.into());
    }

    // Check for bare URI (http://, https://, urn:)
    // Use byte comparison for speed - avoid string operations
    let bytes = s.as_bytes();
    let is_uri = bytes.len() > 4 && (bytes.starts_with(b"http") || bytes.starts_with(b"urn:"));

    if is_uri {
        let node = NamedNode::new(s)
            .map_err(|_| PolarsError::ComputeError(format!("Invalid URI: {s}").into()))?;
        return Ok(node.into());
    }

    // Default: literal
    Ok(Literal::new_simple_literal(s).into())
}

pub struct RdfBatchWriter {
    serializer: WriterQuadSerializer<BufWriter<File>>,
}

impl RdfBatchWriter {
    pub fn new(path: PathBuf, format: RdfFormat) -> std::io::Result<Self> {
        let file = File::create(path)?;
        let writer = BufWriter::with_capacity(256 * 1024, file);
        let serializer = RdfSerializer::from_format(format).for_writer(writer);
        Ok(Self { serializer })
    }

    /// Stream a DataFrame batch directly to the RDF file
    ///
    /// Optimizations:
    /// - Pre-filter rows with null subjects (columnar operation)
    /// - Subjects parsed once and reused across all predicate columns
    /// - Predicates parsed once per column (they're constant)
    /// - Zero-copy quad serialization using QuadRef
    pub fn write_batch(&mut self, batch: &DataFrame) -> PolarsResult<()> {
        // Pre-filter: drop rows where subject is null
        // This is a columnar operation - removes the null check from the hot loop
        let filtered = batch
            .clone()
            .lazy()
            .filter(col("_subject").is_not_null())
            .collect()?;

        // Early exit if no valid rows
        if filtered.height() == 0 {
            return Ok(());
        }

        // Parse all subjects once - reused for every predicate column
        // Subjects are guaranteed non-null after filtering
        let subject_strs = filtered.column("_subject")?.str()?;
        let parsed_subjects: Vec<ParsedSubject> = subject_strs
            .iter()
            .flatten()
            .filter_map(|s| parse_subject_cleaned(clean_subject(s)).ok())
            .collect();

        // Stream rdf:type triples (if present)
        if let Ok(type_col) = filtered.column("_type") {
            let types = type_col.cast(&DataType::String)?;
            let types = types.str()?;
            let pred = parse_predicate(RDF_TYPE)?;

            // Subject guaranteed non-null, only check object
            for (subj, obj) in parsed_subjects.iter().zip(types.iter()) {
                if let Some(obj) = obj {
                    let subj_ref = subj.as_ref();
                    self.write_quad_parsed(&subj_ref, &pred, obj)?;
                }
            }
        }

        // Get predicate columns (excluding meta columns)
        let predicate_cols: Vec<String> = filtered
            .get_column_names()
            .into_iter()
            .filter(|n| {
                let s = n.as_str();
                s != "_subject" && s != "_type"
            })
            .map(|n| n.to_string())
            .collect();

        // Stream each predicate column using pre-parsed subjects
        for name in &predicate_cols {
            let pred = parse_predicate(name)?;
            let objects = filtered.column(name.as_str())?.cast(&DataType::String)?;
            let objects = objects.str()?;

            // Subject guaranteed non-null, only check object
            for (subj, obj) in parsed_subjects.iter().zip(objects.iter()) {
                if let Some(obj) = obj {
                    let subj_ref = subj.as_ref();
                    self.write_quad_parsed(&subj_ref, &pred, obj)?;
                }
            }
        }

        Ok(())
    }

    /// Write a quad with pre-parsed subject (avoids re-parsing subject for each predicate)
    ///
    /// Uses QuadRef to avoid cloning subject/predicate strings - only references are passed.
    #[inline]
    fn write_quad_parsed(
        &mut self,
        subj: &NamedOrBlankNode,
        pred: &NamedNode,
        obj: &str,
    ) -> PolarsResult<()> {
        let obj = parse_object(obj)?;
        // Use QuadRef::new with references - no string cloning
        let quad = QuadRef::new(subj, pred, &obj, GraphNameRef::DefaultGraph);
        self.serializer
            .serialize_quad(quad)
            .map_err(|_| PolarsError::ComputeError("Serialization failed".into()))
    }

    pub fn finish(self) -> PolarsResult<()> {
        self.serializer
            .finish()
            .map_err(|_| PolarsError::ComputeError("Failed to finish RDF file".into()))?;
        Ok(())
    }
}

// ============================================================================
// COLUMNAR N-TRIPLES SERIALIZER
// ============================================================================
//
// Fully columnar RDF serialization using Polars string operations.
// No per-row function calls - everything is vectorized.

use super::metadata::RdfTermType;

/// Predicate column info for serialization
pub struct PredicateColumn {
    /// Predicate URI
    pub uri: String,
    /// Column name in DataFrame
    pub column: String,
    /// Term type for proper RDF formatting
    pub term_type: RdfTermType,
}

/// Columnar N-Triples writer
///
/// Uses Polars vectorized string operations for maximum throughput.
/// All formatting happens in columnar operations, then writes in bulk.
///
/// Supports proper XSD datatypes for typed literals.
pub struct ColumnarNTriplesWriter {
    writer: BufWriter<File>,
}

impl ColumnarNTriplesWriter {
    pub fn new(path: PathBuf) -> std::io::Result<Self> {
        let file = File::create(path)?;
        let writer = BufWriter::with_capacity(512 * 1024, file); // 512KB buffer
        Ok(Self { writer })
    }

    /// Write a batch with explicit predicate info (includes datatypes)
    ///
    /// This is the preferred method when you have schema information.
    pub fn write_batch_typed(
        &mut self,
        batch: &DataFrame,
        predicates: &[PredicateColumn],
    ) -> PolarsResult<()> {
        if batch.height() == 0 {
            return Ok(());
        }

        let subjects = batch.column("_subject")?.str()?;

        // Write rdf:type triples if present
        if let Ok(type_col) = batch.column("_type") {
            let types = type_col.cast(&DataType::String)?;
            let types_str = types.str()?;
            self.write_column_vectorized(subjects, RDF_TYPE, types_str, &RdfTermType::Uri)?;
        }

        // Write each predicate column with proper datatype
        for pred in predicates {
            if let Ok(col) = batch.column(pred.column.as_str()) {
                let objects = col.cast(&DataType::String)?;
                let objects_str = objects.str()?;
                self.write_column_vectorized(subjects, &pred.uri, objects_str, &pred.term_type)?;
            }
        }

        Ok(())
    }

    /// Write a batch inferring types from column data types
    ///
    /// Simpler API when predicate URIs are column names.
    pub fn write_batch(&mut self, batch: &DataFrame) -> PolarsResult<()> {
        if batch.height() == 0 {
            return Ok(());
        }

        let subjects = batch.column("_subject")?.str()?;

        // Write rdf:type triples if present
        if let Ok(type_col) = batch.column("_type") {
            let types = type_col.cast(&DataType::String)?;
            let types_str = types.str()?;
            self.write_column_vectorized(subjects, RDF_TYPE, types_str, &RdfTermType::Uri)?;
        }

        // Write each predicate column, inferring type from Polars dtype
        for col_name in batch.get_column_names() {
            let name = col_name.as_str();
            if name == "_subject" || name == "_type" {
                continue;
            }

            let column = batch.column(col_name)?;
            let term_type = polars_dtype_to_rdf_type(column.dtype());

            let objects = column.cast(&DataType::String)?;
            let objects_str = objects.str()?;
            self.write_column_vectorized(subjects, name, objects_str, &term_type)?;
        }

        Ok(())
    }

    /// Write all triples for a single predicate column using vectorized ops
    fn write_column_vectorized(
        &mut self,
        subjects: &StringChunked,
        predicate: &str,
        objects: &StringChunked,
        term_type: &RdfTermType,
    ) -> PolarsResult<()> {
        // Build DataFrame with subject and object
        let df = DataFrame::new(vec![
            subjects.clone().into_series().with_name("s".into()).into(),
            objects.clone().into_series().with_name("o".into()).into(),
        ])?;

        // Filter nulls using columnar operation
        let df = df
            .lazy()
            .filter(col("s").is_not_null().and(col("o").is_not_null()))
            .collect()?;

        if df.height() == 0 {
            return Ok(());
        }

        // Build the object formatting expression based on term type
        let object_expr = match term_type {
            RdfTermType::Uri => {
                // URI: <uri> or _:bnode
                when(col("o").str().starts_with(lit("_:")))
                    .then(col("o"))
                    .otherwise(concat_str([lit("<"), col("o"), lit(">")], "", true))
            }
            _ => {
                // Literal with optional datatype suffix
                let escaped = col("o")
                    .str()
                    .replace_all(lit("\\"), lit("\\\\"), true)
                    .str()
                    .replace_all(lit("\""), lit("\\\""), true)
                    .str()
                    .replace_all(lit("\n"), lit("\\n"), true)
                    .str()
                    .replace_all(lit("\r"), lit("\\r"), true)
                    .str()
                    .replace_all(lit("\t"), lit("\\t"), true);

                let suffix = term_type.xsd_suffix().unwrap_or("");
                concat_str([lit("\""), escaped, lit("\""), lit(suffix)], "", true)
            }
        };

        // Format complete N-Triple line (columnar)
        let formatted = df
            .lazy()
            .select([concat_str(
                [
                    // Subject: <uri> or _:bnode
                    when(col("s").str().starts_with(lit("_:")))
                        .then(col("s"))
                        .otherwise(concat_str([lit("<"), col("s"), lit(">")], "", true)),
                    lit(" <"),
                    lit(predicate),
                    lit("> "),
                    object_expr,
                    lit(" .\n"),
                ],
                "",
                true,
            )
            .alias("triple")])
            .collect()?;

        // Write all formatted triples at once
        let triples = formatted.column("triple")?.str()?;
        for triple in triples.iter().flatten() {
            self.writer
                .write_all(triple.as_bytes())
                .map_err(|e| PolarsError::ComputeError(format!("Write failed: {}", e).into()))?;
        }

        Ok(())
    }

    pub fn finish(mut self) -> PolarsResult<()> {
        self.writer
            .flush()
            .map_err(|e| PolarsError::ComputeError(format!("Flush failed: {}", e).into()))?;
        Ok(())
    }
}

/// Map Polars data type to RDF term type
fn polars_dtype_to_rdf_type(dtype: &DataType) -> RdfTermType {
    match dtype {
        DataType::Boolean => RdfTermType::Boolean,
        DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UInt8
        | DataType::UInt16
        | DataType::UInt32
        | DataType::UInt64 => RdfTermType::Integer,
        DataType::Float32 | DataType::Float64 => RdfTermType::Double,
        DataType::Date => RdfTermType::Date,
        DataType::Datetime(_, _) => RdfTermType::DateTime,
        DataType::String | DataType::Categorical(_, _) => RdfTermType::String,
        _ => RdfTermType::String, // Default to string for unknown types
    }
}
