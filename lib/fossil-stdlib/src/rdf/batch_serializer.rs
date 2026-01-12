//! Efficient batch RDF serialization using Oxigraph
//!
//! This module provides efficient, columnar RDF serialization that:
//! - Processes data in batches to avoid loading everything in memory
//! - Uses columnar operations (Polars) for vectorized performance
//! - Leverages Oxigraph Store for correct RDF serialization with automatic prefix handling
//! - Extracts URIs from type attributes (#[uri("...")])

use std::fs::File;

use oxigraph::io::RdfSerializer;
use oxigraph::model::{GraphNameRef, Literal, NamedNode, Quad, Triple, TripleRef};
use oxigraph::store::Store;
use polars::prelude::*;
use std::io::BufWriter;

use super::RdfMetadata;

/// Configuration for batch processing
const DEFAULT_BATCH_SIZE: usize = 10_000;

/// Efficient batch RDF serializer using Oxigraph Store
pub struct BatchRdfSerializer {
    store: Store,
    interner: fossil_lang::context::Interner,
    batch_size: usize,
}

impl BatchRdfSerializer {
    /// Create a new batch serializer
    pub fn new(
        interner: fossil_lang::context::Interner,
        batch_size: Option<usize>,
    ) -> Result<Self, String> {
        let store = Store::new().map_err(|e| format!("Failed to create Oxigraph store: {}", e))?;

        Ok(Self {
            store,
            interner,
            batch_size: batch_size.unwrap_or(DEFAULT_BATCH_SIZE),
        })
    }

    /// Serialize a LazyFrame to RDF and write to file
    pub fn serialize_to_file(
        &mut self,
        lazy_frame: LazyFrame,
        subjects: &[impl AsRef<str>],
        metadata: &RdfMetadata,
        filename: &str,
    ) -> Result<(), String> {
        // Insert all triples into the store
        self.insert_triples(lazy_frame, subjects, metadata)?;

        // Determine format from extension
        let format = if filename.ends_with(".ttl") {
            oxigraph::io::RdfFormat::Turtle
        } else if filename.ends_with(".nt") {
            oxigraph::io::RdfFormat::NTriples
        } else if filename.ends_with(".rdf") || filename.ends_with(".xml") {
            oxigraph::io::RdfFormat::RdfXml
        } else {
            return Err(format!(
                "Unsupported file extension: {} (use .ttl or .nt)",
                filename
            ));
        };

        // Let Oxigraph serialize the graph (now properly grouped by subject)
        let file = File::create(filename).map_err(|e| format!("Failed to create file: {}", e))?;

        self.store
            .dump_graph_to_writer(GraphNameRef::DefaultGraph, format, file)
            .map_err(|e| format!("Failed to serialize graph: {}", e))?;

        Ok(())
    }

    /// Insert triples into the store (columnar processing)
    fn insert_triples(
        &mut self,
        lazy_frame: LazyFrame,
        subjects: &[impl AsRef<str>],
        metadata: &RdfMetadata,
    ) -> Result<(), String> {
        let total_rows = subjects.len();

        // Process in batches
        for batch_start in (0..total_rows).step_by(self.batch_size) {
            let batch_end = (batch_start + self.batch_size).min(total_rows);
            let batch_len = batch_end - batch_start;

            // Slice the LazyFrame for this batch (stays lazy)
            let batch_lf = lazy_frame
                .clone()
                .slice(batch_start as i64, batch_len as u32);

            // Collect only this batch
            let batch_df = batch_lf
                .collect()
                .map_err(|e| format!("Failed to collect batch: {}", e))?;

            // Get subjects for this batch
            let batch_subjects = &subjects[batch_start..batch_end];

            // Insert triples from this batch
            self.insert_batch(&batch_df, batch_subjects, metadata)?;
        }

        Ok(())
    }

    /// Insert triples from a single batch into the store
    /// Groups by subject for better Turtle serialization
    fn insert_batch(
        &mut self,
        batch: &DataFrame,
        subjects: &[impl AsRef<str>],
        metadata: &RdfMetadata,
    ) -> Result<(), String> {
        // Process row by row (by subject) for better Turtle grouping
        for row_idx in 0..batch.height() {
            let subject_uri = subjects[row_idx].as_ref();
            let subject =
                NamedNode::new(subject_uri).map_err(|e| format!("Invalid subject URI: {}", e))?;

            // For each column (predicate) in this row
            for col_name in batch.get_column_names() {
                let col_sym = self.interner.intern(col_name);

                // Check if this field has a predicate URI mapping
                if let Some(predicate_uri) = metadata.get_predicate(col_sym) {
                    let predicate = NamedNode::new(predicate_uri)
                        .map_err(|e| format!("Invalid predicate URI: {}", e))?;

                    let column = batch
                        .column(col_name)
                        .map_err(|e| format!("Failed to get column: {}", e))?;
                    let series = column.as_materialized_series();

                    // Get value for this row
                    let object = self.value_to_literal(series, row_idx)?;

                    if let Some(obj) = object {
                        let triple = Triple::new(subject.clone(), predicate, obj);
                        self.store
                            .insert(triple.as_ref().in_graph(GraphNameRef::DefaultGraph))
                            .map_err(|e| format!("Failed to insert triple: {}", e))?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Convert a value from a series at a given index to a Literal
    fn value_to_literal(&self, series: &Series, idx: usize) -> Result<Option<Literal>, String> {
        match series.dtype() {
            DataType::String => {
                let values = series
                    .str()
                    .map_err(|e| format!("Failed to cast to string: {}", e))?;
                Ok(values.get(idx).map(|v| Literal::new_simple_literal(v)))
            }
            DataType::Int64 | DataType::Int32 | DataType::UInt32 | DataType::UInt64 => {
                let values = series
                    .cast(&DataType::Int64)
                    .map_err(|e| format!("Failed to cast to i64: {}", e))?;
                let i64_values = values
                    .i64()
                    .map_err(|e| format!("Failed to get i64 values: {}", e))?;
                let xsd_integer = NamedNode::new_unchecked(xsd::INTEGER_STR);
                Ok(i64_values
                    .get(idx)
                    .map(|v| Literal::new_typed_literal(v.to_string(), xsd_integer)))
            }
            DataType::Float64 | DataType::Float32 => {
                let values = series
                    .cast(&DataType::Float64)
                    .map_err(|e| format!("Failed to cast to f64: {}", e))?;
                let f64_values = values
                    .f64()
                    .map_err(|e| format!("Failed to get f64 values: {}", e))?;
                let xsd_double = NamedNode::new_unchecked(xsd::DOUBLE_STR);
                Ok(f64_values
                    .get(idx)
                    .map(|v| Literal::new_typed_literal(v.to_string(), xsd_double)))
            }
            DataType::Boolean => {
                let values = series
                    .bool()
                    .map_err(|e| format!("Failed to cast to bool: {}", e))?;
                let xsd_boolean = NamedNode::new_unchecked(xsd::BOOLEAN_STR);
                Ok(values
                    .get(idx)
                    .map(|v| Literal::new_typed_literal(v.to_string(), xsd_boolean)))
            }
            _ => Err(format!("Unsupported data type: {:?}", series.dtype())),
        }
    }
}

/// XSD datatype URI strings
mod xsd {
    pub const INTEGER_STR: &str = "http://www.w3.org/2001/XMLSchema#integer";
    pub const DOUBLE_STR: &str = "http://www.w3.org/2001/XMLSchema#double";
    pub const BOOLEAN_STR: &str = "http://www.w3.org/2001/XMLSchema#boolean";
}

/// Helper function to create a batch serializer
pub fn create_serializer(
    interner: fossil_lang::context::Interner,
    batch_size: Option<usize>,
) -> Result<BatchRdfSerializer, String> {
    BatchRdfSerializer::new(interner, batch_size)
}
