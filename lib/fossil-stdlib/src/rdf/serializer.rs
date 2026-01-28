//! RDF serialization using Oxigraph
//!
//! This module provides streaming RDF serialization from DataFrames to various
//! RDF formats using Oxigraph's serialization capabilities.

use std::fs::File;
use std::io::BufWriter;
use std::path::Path;

use oxigraph::io::{RdfFormat, RdfSerializer};
use oxigraph::model::{BlankNodeRef, GraphNameRef, LiteralRef, NamedNodeRef, NamedOrBlankNodeRef, QuadRef, TermRef};
use polars::prelude::*;

use super::RdfMetadata;

/// Supported RDF output formats
///
/// Infers format from file extension for KISS design.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// N-Triples (.nt) - simple line-based format
    NTriples,
    /// N-Quads (.nq) - N-Triples with graph name
    NQuads,
    /// Turtle (.ttl) - compact, human-readable
    Turtle,
    /// TriG (.trig) - Turtle with named graphs
    TriG,
    /// RDF/XML (.rdf, .xml) - XML-based format
    RdfXml,
}

impl OutputFormat {
    /// Infer output format from file extension
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the output file
    ///
    /// # Returns
    ///
    /// The detected format, defaulting to N-Quads if unknown
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let ext = path
            .as_ref()
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");

        match ext.to_lowercase().as_str() {
            "nt" => OutputFormat::NTriples,
            "nq" => OutputFormat::NQuads,
            "ttl" => OutputFormat::Turtle,
            "trig" => OutputFormat::TriG,
            "rdf" | "xml" => OutputFormat::RdfXml,
            _ => OutputFormat::NQuads, // Default
        }
    }

    /// Convert to Oxigraph's RdfFormat
    fn to_rdf_format(self) -> RdfFormat {
        match self {
            OutputFormat::NTriples => RdfFormat::NTriples,
            OutputFormat::NQuads => RdfFormat::NQuads,
            OutputFormat::Turtle => RdfFormat::Turtle,
            OutputFormat::TriG => RdfFormat::TriG,
            OutputFormat::RdfXml => RdfFormat::RdfXml,
        }
    }
}

/// Batch writer for converting DataFrames to RDF triples
///
/// Processes DataFrame batches and serializes them as RDF using Oxigraph.
/// Supports streaming/batched execution for constant memory usage.
pub struct RdfBatchWriter {
    serializer: oxigraph::io::WriterQuadSerializer<BufWriter<File>>,
}

impl RdfBatchWriter {
    /// Create a new RDF batch writer
    ///
    /// # Arguments
    ///
    /// * `path` - Output file path
    /// * `format` - RDF output format
    /// * `_metadata` - RDF metadata containing type information (unused, kept for API compatibility)
    pub fn new<P: AsRef<Path>>(
        path: P,
        format: OutputFormat,
        _metadata: &RdfMetadata,
    ) -> std::io::Result<Self> {
        let file = File::create(path)?;
        let writer = BufWriter::new(file);
        let serializer = RdfSerializer::from_format(format.to_rdf_format()).for_writer(writer);

        Ok(Self { serializer })
    }

    /// Write a batch DataFrame as RDF triples
    ///
    /// The DataFrame must have:
    /// - `_subject`: Subject URI or blank node ID
    /// - `_type` (optional): rdf:type URI
    /// - Other columns: predicate URIs mapped to their values
    ///
    /// # Arguments
    ///
    /// * `batch` - DataFrame batch with RDF-structured columns
    pub fn write_batch(&mut self, batch: &DataFrame) -> PolarsResult<()> {
        let subject_col = batch.column("_subject")?;
        let type_col = batch.column("_type").ok();

        // Get all predicate columns (everything except _subject and _type)
        let predicate_cols: Vec<_> = batch
            .get_column_names()
            .into_iter()
            .filter(|name| *name != "_subject" && *name != "_type")
            .collect();

        let height = batch.height();

        for row_idx in 0..height {
            // 1. Parse subject (URI or blank node)
            let subject_value = subject_col.get(row_idx)?;
            let subject_str = subject_value.str_value();

            // 2. Generate rdf:type triple if _type exists
            if let Some(ref type_col) = type_col {
                let type_value = type_col.get(row_idx)?;
                if !type_value.is_null() {
                    let type_uri = type_value.str_value();
                    self.write_triple(&subject_str, RDF_TYPE, &type_uri, TermType::NamedNode)?;
                }
            }

            // 3. Generate triples for each predicate column
            for pred_name in &predicate_cols {
                let col = batch.column(pred_name)?;
                let value = col.get(row_idx)?;

                if !value.is_null() {
                    let object_str = value.str_value();
                    self.write_triple(&subject_str, pred_name, &object_str, TermType::Literal)?;
                }
            }
        }

        Ok(())
    }

    /// Write a single RDF triple
    fn write_triple(
        &mut self,
        subject: &str,
        predicate: &str,
        object: &str,
        object_type: TermType,
    ) -> PolarsResult<()> {
        // Parse subject (blank node if starts with "_:")
        let subject_ref: NamedOrBlankNodeRef = if subject.starts_with("_:") {
            let blank_id = &subject[2..];
            NamedOrBlankNodeRef::BlankNode(BlankNodeRef::new(blank_id).map_err(|e| {
                PolarsError::ComputeError(format!("Invalid blank node ID '{}': {}", blank_id, e).into())
            })?)
        } else {
            NamedOrBlankNodeRef::NamedNode(NamedNodeRef::new(subject).map_err(|e| {
                PolarsError::ComputeError(format!("Invalid subject URI '{}': {}", subject, e).into())
            })?)
        };

        // Parse predicate (always a named node)
        let predicate_node = NamedNodeRef::new(predicate).map_err(|e| {
            PolarsError::ComputeError(format!("Invalid predicate URI '{}': {}", predicate, e).into())
        })?;

        // Parse object based on type
        let object_term: TermRef = match object_type {
            TermType::NamedNode => {
                TermRef::NamedNode(NamedNodeRef::new(object).map_err(|e| {
                    PolarsError::ComputeError(format!("Invalid object URI '{}': {}", object, e).into())
                })?)
            }
            TermType::Literal => TermRef::Literal(LiteralRef::new_simple_literal(object)),
        };

        // Create and serialize the quad (default graph)
        let quad = QuadRef::new(subject_ref, predicate_node, object_term, GraphNameRef::DefaultGraph);
        self.serializer.serialize_quad(quad).map_err(|e| {
            PolarsError::ComputeError(format!("Failed to serialize triple: {}", e).into())
        })?;

        Ok(())
    }

    /// Finish writing and close the file
    pub fn finish(self) -> PolarsResult<()> {
        self.serializer.finish().map_err(|e| {
            PolarsError::ComputeError(format!("Failed to finish RDF serialization: {}", e).into())
        })?;
        Ok(())
    }
}

/// Type of RDF term for objects
#[derive(Debug, Clone, Copy)]
enum TermType {
    NamedNode,
    Literal,
}

/// The rdf:type predicate URI
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_from_extension() {
        assert_eq!(OutputFormat::from_path("test.nt"), OutputFormat::NTriples);
        assert_eq!(OutputFormat::from_path("test.nq"), OutputFormat::NQuads);
        assert_eq!(OutputFormat::from_path("test.ttl"), OutputFormat::Turtle);
        assert_eq!(OutputFormat::from_path("test.trig"), OutputFormat::TriG);
        assert_eq!(OutputFormat::from_path("test.rdf"), OutputFormat::RdfXml);
        assert_eq!(OutputFormat::from_path("test.xml"), OutputFormat::RdfXml);
        // Unknown defaults to NQuads
        assert_eq!(OutputFormat::from_path("test.unknown"), OutputFormat::NQuads);
    }
}
