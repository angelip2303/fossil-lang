use std::io::Write;

use super::triple_writer::TripleWriter;
use oxrdf::{BlankNode, GraphNameRef, Literal, NamedNode, NamedNodeRef, NamedOrBlankNode, QuadRef, Term};
use oxrdfio::{RdfFormat, RdfSerializer, WriterQuadSerializer};

const BNODE_PREFIX: &str = "_:";

pub struct OxrdfTripleWriter {
    serializer: Option<WriterQuadSerializer<Box<dyn Write + Send>>>,
}

impl OxrdfTripleWriter {
    pub fn new(writer: Box<dyn Write + Send>, format: RdfFormat) -> Self {
        let serializer = RdfSerializer::from_format(format).for_writer(writer);
        Self { serializer: Some(serializer) }
    }
}

impl TripleWriter for OxrdfTripleWriter {
    fn write_triple(
        &mut self,
        subject: &str,
        predicate: &str,
        object: &str,
        datatype: Option<&str>,
        _lang: Option<&str>,
        graph: Option<&str>,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let serializer = self.serializer.as_mut()
            .ok_or("Writer already finished")?;

        let subj = parse_subject(subject)?;
        let pred_uri = predicate.strip_prefix('<').and_then(|s| s.strip_suffix('>')).unwrap_or(predicate);
        let pred = NamedNode::new(pred_uri)
            .map_err(|e| format!("Invalid predicate: {e}"))?;
        let obj = parse_object(object, datatype)?;
        let graph_name = parse_graph(graph);

        let quad = QuadRef::new(&subj, &pred, &obj, graph_name.as_ref());
        serializer
            .serialize_quad(quad)
            .map_err(|e| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Serialization failed: {e}").into()
            })
    }

    fn finish(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        if let Some(serializer) = self.serializer.take() {
            let mut writer = serializer.finish()
                .map_err(|e| -> Box<dyn std::error::Error + Send + Sync> {
                    format!("Failed to finish: {e}").into()
                })?;
            writer.flush()?;
        }
        Ok(())
    }
}

enum OwnedGraphName {
    Default,
    Named(NamedNode),
}

impl OwnedGraphName {
    fn as_ref(&self) -> GraphNameRef<'_> {
        match self {
            OwnedGraphName::Default => GraphNameRef::DefaultGraph,
            OwnedGraphName::Named(n) => GraphNameRef::NamedNode(n.as_ref()),
        }
    }
}

fn parse_graph(graph: Option<&str>) -> OwnedGraphName {
    match graph {
        None => OwnedGraphName::Default,
        Some(s) => {
            let uri = s.strip_prefix('<').and_then(|s| s.strip_suffix('>')).unwrap_or(s);
            match NamedNode::new(uri) {
                Ok(n) => OwnedGraphName::Named(n),
                Err(_) => OwnedGraphName::Default,
            }
        }
    }
}

fn parse_subject(s: &str) -> Result<NamedOrBlankNode, Box<dyn std::error::Error + Send + Sync>> {
    let s = s.strip_prefix('<').and_then(|inner| inner.strip_suffix('>')).unwrap_or(s);
    if let Some(id) = s.strip_prefix(BNODE_PREFIX) {
        let bnode = BlankNode::new(id)
            .map_err(|_| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Invalid blank node: {s}").into()
            })?;
        Ok(bnode.into())
    } else {
        let node = NamedNode::new(s)
            .map_err(|_| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Invalid subject URI: {s}").into()
            })?;
        Ok(node.into())
    }
}

fn parse_object(s: &str, datatype: Option<&str>) -> Result<Term, Box<dyn std::error::Error + Send + Sync>> {
    if let Some(id) = s.strip_prefix(BNODE_PREFIX) {
        let bnode = BlankNode::new(id)
            .map_err(|_| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Invalid blank node: {s}").into()
            })?;
        return Ok(bnode.into());
    }

    if let Some(uri) = s.strip_prefix('<').and_then(|s| s.strip_suffix('>')) {
        let node = NamedNode::new(uri)
            .map_err(|_| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Invalid URI: {s}").into()
            })?;
        return Ok(node.into());
    }

    let bytes = s.as_bytes();
    let is_uri = bytes.len() > 4 && (bytes.starts_with(b"http") || bytes.starts_with(b"urn:"));
    if is_uri {
        let node = NamedNode::new(s)
            .map_err(|_| -> Box<dyn std::error::Error + Send + Sync> {
                format!("Invalid URI: {s}").into()
            })?;
        return Ok(node.into());
    }

    match datatype {
        Some(dt) => Ok(Literal::new_typed_literal(s, NamedNodeRef::new_unchecked(dt)).into()),
        None => Ok(Literal::new_simple_literal(s).into()),
    }
}
