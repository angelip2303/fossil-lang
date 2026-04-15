//! YAML serialization for GraphAr metadata files.

use crate::types::{EdgeInfo, GraphInfo, VertexInfo};

pub fn write_graph_yml(info: &GraphInfo) -> Result<String, String> {
    serde_yml::to_string(info).map_err(|e| format!("graph.yml serialization failed: {e}"))
}

pub fn write_vertex_yml(info: &VertexInfo) -> Result<String, String> {
    serde_yml::to_string(info).map_err(|e| format!("vertex yml serialization failed: {e}"))
}

pub fn write_edge_yml(info: &EdgeInfo) -> Result<String, String> {
    serde_yml::to_string(info).map_err(|e| format!("edge yml serialization failed: {e}"))
}

pub fn read_graph_yml(yaml: &str) -> Result<GraphInfo, String> {
    serde_yml::from_str(yaml).map_err(|e| format!("graph.yml parse failed: {e}"))
}

pub fn read_vertex_yml(yaml: &str) -> Result<VertexInfo, String> {
    serde_yml::from_str(yaml).map_err(|e| format!("vertex yml parse failed: {e}"))
}

pub fn read_edge_yml(yaml: &str) -> Result<EdgeInfo, String> {
    serde_yml::from_str(yaml).map_err(|e| format!("edge yml parse failed: {e}"))
}
