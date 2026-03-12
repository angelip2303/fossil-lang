use std::collections::HashMap;

use polars::prelude::*;

use super::triple_writer::TripleWriter;

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

#[inline]
fn clean_subject(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

#[inline]
fn clean_predicate(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

pub struct RdfBatchWriter {
    writer: Box<dyn TripleWriter>,
}

impl RdfBatchWriter {
    pub fn new(writer: Box<dyn TripleWriter>) -> Self {
        Self { writer }
    }

    /// Stream a DataFrame batch directly to the RDF writer.
    ///
    /// `xsd_types` maps predicate URI → XSD datatype IRI for typed literals.
    /// Columns whose predicate is not in the map are serialized as simple literals.
    ///
    /// Optimizations:
    /// - Pre-filter rows with null subjects (columnar operation)
    /// - Subjects extracted once and reused across all predicate columns
    /// - Zero intermediate allocations for URIs (string slices from DataFrame)
    pub fn write_batch(
        &mut self,
        batch: &DataFrame,
        xsd_types: &HashMap<String, &'static str>,
    ) -> PolarsResult<()> {
        // Pre-filter: drop rows where subject is null
        let filtered = batch
            .clone()
            .lazy()
            .filter(col("_subject").is_not_null())
            .collect()?;

        if filtered.height() == 0 {
            return Ok(());
        }

        let subject_strs = filtered.column("_subject")?.str()?;

        // Collect graph values if column exists
        let graph_strs = filtered
            .column("_graph")
            .ok()
            .and_then(|c| c.cast(&DataType::String).ok());
        let graph_col = graph_strs.as_ref().and_then(|c| c.str().ok());

        // rdf:type column
        if let Ok(type_col) = filtered.column("_type") {
            let types = type_col.cast(&DataType::String)?;
            let types = types.str()?;

            for i in 0..filtered.height() {
                let Some(subj) = subject_strs.get(i) else { continue };
                let Some(type_val) = types.get(i) else { continue };
                let graph = graph_col.and_then(|g| g.get(i));

                self.writer
                    .write_triple(clean_subject(subj), RDF_TYPE, type_val, None, None, graph)
                    .map_err(|e| PolarsError::ComputeError(format!("RDF write error: {e}").into()))?;
            }
        }

        // Predicate columns
        let predicate_cols: Vec<String> = filtered
            .get_column_names()
            .into_iter()
            .filter(|n| {
                let s = n.as_str();
                s != "_subject" && s != "_type" && s != "_graph"
            })
            .map(|n| n.to_string())
            .collect();

        for name in &predicate_cols {
            let pred = clean_predicate(name);
            let xsd_type = xsd_types.get(name.as_str()).copied();
            let objects = filtered.column(name.as_str())?.cast(&DataType::String)?;
            let objects = objects.str()?;

            for i in 0..filtered.height() {
                let Some(subj) = subject_strs.get(i) else { continue };
                let Some(obj) = objects.get(i) else { continue };
                let graph = graph_col.and_then(|g| g.get(i));

                self.writer
                    .write_triple(clean_subject(subj), pred, obj, xsd_type, None, graph)
                    .map_err(|e| PolarsError::ComputeError(format!("RDF write error: {e}").into()))?;
            }
        }

        Ok(())
    }

    pub fn finish(mut self) -> PolarsResult<()> {
        self.writer
            .finish()
            .map_err(|e| PolarsError::ComputeError(format!("Failed to finish RDF: {e}").into()))
    }
}
