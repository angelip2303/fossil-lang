use std::io::Write;

use oxrdf::{BlankNode, GraphNameRef, Literal, NamedNode, NamedOrBlankNode, QuadRef, Term};
use oxrdfio::{RdfFormat, RdfSerializer, WriterQuadSerializer};
use polars::prelude::*;

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
const BNODE_PREFIX: &str = "_:";

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

/// Parsed graph name for RDF quads
#[derive(Clone)]
enum ParsedGraph {
    Default,
    Named(NamedNode),
}

impl ParsedGraph {
    fn as_ref(&self) -> GraphNameRef<'_> {
        match self {
            ParsedGraph::Default => GraphNameRef::DefaultGraph,
            ParsedGraph::Named(n) => GraphNameRef::NamedNode(n.as_ref()),
        }
    }
}

fn parse_graph(s: Option<&str>) -> ParsedGraph {
    match s {
        None => ParsedGraph::Default,
        Some(s) => {
            let uri = s.strip_prefix('<').and_then(|s| s.strip_suffix('>')).unwrap_or(s);
            match NamedNode::new(uri) {
                Ok(n) => ParsedGraph::Named(n),
                Err(_) => ParsedGraph::Default,
            }
        }
    }
}

#[inline]
fn clean_subject(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

fn parse_subject(s: &str) -> Result<ParsedSubject, PolarsError> {
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

    Ok(Literal::new_simple_literal(s).into())
}

pub struct RdfBatchWriter {
    serializer: WriterQuadSerializer<Box<dyn Write>>,
}

impl RdfBatchWriter {
    pub fn new(writer: Box<dyn Write>, format: RdfFormat) -> Self {
        let serializer = RdfSerializer::from_format(format).for_writer(writer);
        Self { serializer }
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

        if filtered.height() == 0 {
            return Ok(());
        }

        // Parse all subjects once - reused for every predicate column
        // Subjects are guaranteed non-null after filtering
        let subject_strs = filtered.column("_subject")?.str()?;
        let parsed_subjects: Vec<ParsedSubject> = subject_strs
            .iter()
            .flatten()
            .filter_map(|s| parse_subject(clean_subject(s)).ok())
            .collect();

        let parsed_graphs: Vec<ParsedGraph> = if let Ok(graph_col) = filtered.column("_graph") {
            let graphs = graph_col.cast(&DataType::String)?;
            let graphs = graphs.str()?;
            graphs.iter().map(parse_graph).collect()
        } else {
            vec![ParsedGraph::Default; filtered.height()]
        };

        if let Ok(type_col) = filtered.column("_type") {
            let types = type_col.cast(&DataType::String)?;
            let types = types.str()?;
            let pred = parse_predicate(RDF_TYPE)?;

            // Subject guaranteed non-null, only check object
            for ((subj, graph), obj) in parsed_subjects.iter().zip(parsed_graphs.iter()).zip(types.iter()) {
                if let Some(obj) = obj {
                    let subj_ref = subj.as_ref();
                    self.write_quad_parsed(&subj_ref, &pred, obj, graph)?;
                }
            }
        }

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
            let pred = parse_predicate(name)?;
            let objects = filtered.column(name.as_str())?.cast(&DataType::String)?;
            let objects = objects.str()?;

            // Subject guaranteed non-null, only check object
            for ((subj, graph), obj) in parsed_subjects.iter().zip(parsed_graphs.iter()).zip(objects.iter()) {
                if let Some(obj) = obj {
                    let subj_ref = subj.as_ref();
                    self.write_quad_parsed(&subj_ref, &pred, obj, graph)?;
                }
            }
        }

        Ok(())
    }

    /// Write a quad with pre-parsed subject and graph
    ///
    /// Uses QuadRef to avoid cloning subject/predicate strings - only references are passed.
    #[inline]
    fn write_quad_parsed(
        &mut self,
        subj: &NamedOrBlankNode,
        pred: &NamedNode,
        obj: &str,
        graph: &ParsedGraph,
    ) -> PolarsResult<()> {
        let obj = parse_object(obj)?;
        // Use QuadRef::new with references - no string cloning
        let quad = QuadRef::new(subj, pred, &obj, graph.as_ref());
        self.serializer
            .serialize_quad(quad)
            .map_err(|_| PolarsError::ComputeError("Serialization failed".into()))
    }

    pub fn finish(self) -> PolarsResult<Box<dyn Write>> {
        self.serializer
            .finish()
            .map_err(|_| PolarsError::ComputeError("Failed to finish RDF file".into()))
    }
}
