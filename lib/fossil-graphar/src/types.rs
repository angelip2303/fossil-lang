//! GraphAr canonical types (graph.yml, vertex.yml, edge.yml).

use serde::{Deserialize, Serialize};

/// Top-level graph descriptor (graph.yml).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphInfo {
    pub name: String,
    /// Base path prefix for all data files.
    pub prefix: String,
    /// Vertex info YAML filenames (e.g., "person.vertex.yml").
    pub vertices: Vec<String>,
    /// Edge info YAML filenames (e.g., "person_knows_person.edge.yml").
    pub edges: Vec<String>,
    pub version: String,
}

impl GraphInfo {
    pub fn new(name: &str, prefix: &str) -> Self {
        Self {
            name: name.to_string(),
            prefix: prefix.to_string(),
            vertices: Vec::new(),
            edges: Vec::new(),
            version: "gar/v1".to_string(),
        }
    }
}

/// Per-vertex-type descriptor (e.g., person.vertex.yml).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VertexInfo {
    /// Vertex type name (e.g., "person").
    #[serde(rename = "type")]
    pub type_name: String,
    /// Rows per chunk file.
    pub chunk_size: usize,
    /// Directory prefix (e.g., "vertex/person/").
    pub prefix: String,
    pub property_groups: Vec<PropertyGroup>,
    pub version: String,
}

impl VertexInfo {
    pub fn new(type_name: &str, chunk_size: usize) -> Self {
        Self {
            type_name: type_name.to_string(),
            chunk_size,
            prefix: format!("vertex/{}/", type_name),
            property_groups: Vec::new(),
            version: "gar/v1".to_string(),
        }
    }

    pub fn with_properties(mut self, properties: Vec<Property>) -> Self {
        self.property_groups.push(PropertyGroup {
            file_type: "parquet".to_string(),
            prefix: String::new(), // auto-generated from property names
            properties,
        });
        self
    }

    pub fn chunk_count(&self, vertex_count: usize) -> usize {
        (vertex_count + self.chunk_size - 1) / self.chunk_size
    }

    pub fn yml_filename(&self) -> String {
        format!("{}.vertex.yml", self.type_name)
    }
}

/// Per-edge-type descriptor (e.g., person_knows_person.edge.yml).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeInfo {
    pub src_type: String,
    pub edge_type: String,
    pub dst_type: String,
    pub chunk_size: usize,
    pub src_chunk_size: usize,
    pub dst_chunk_size: usize,
    pub directed: bool,
    /// Directory prefix (e.g., "edge/person_knows_person/").
    pub prefix: String,
    pub adj_lists: Vec<AdjListInfo>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub property_groups: Vec<PropertyGroup>,
    pub version: String,
}

impl EdgeInfo {
    pub fn new(src: &str, edge: &str, dst: &str, vertex_chunk_size: usize, edge_chunk_size: usize) -> Self {
        Self {
            src_type: src.to_string(),
            edge_type: edge.to_string(),
            dst_type: dst.to_string(),
            chunk_size: edge_chunk_size,
            src_chunk_size: vertex_chunk_size,
            dst_chunk_size: vertex_chunk_size,
            directed: true,
            prefix: format!("edge/{}_{}_{}/" , src, edge, dst),
            adj_lists: vec![
                AdjListInfo::ordered_by_source(),
                AdjListInfo::ordered_by_dest(),
            ],
            property_groups: Vec::new(),
            version: "gar/v1".to_string(),
        }
    }

    pub fn yml_filename(&self) -> String {
        format!("{}_{}_{}.edge.yml", self.src_type, self.edge_type, self.dst_type)
    }
}

/// Adjacency list variant (CSR or CSC).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdjListInfo {
    pub ordered: bool,
    pub aligned_by: String,
    pub file_type: String,
}

impl AdjListInfo {
    pub fn ordered_by_source() -> Self {
        Self { ordered: true, aligned_by: "src".to_string(), file_type: "parquet".to_string() }
    }
    pub fn ordered_by_dest() -> Self {
        Self { ordered: true, aligned_by: "dst".to_string(), file_type: "parquet".to_string() }
    }
}

/// Property group within a vertex or edge type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropertyGroup {
    pub file_type: String,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub prefix: String,
    pub properties: Vec<Property>,
}

/// Single property (column) definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    pub name: String,
    pub data_type: String,
    #[serde(default)]
    pub is_primary: bool,
}
