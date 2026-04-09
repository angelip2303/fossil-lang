//! Path helpers for GraphAr canonical file layout.

/// Vertex chunk path: `vertex/{type}/{group}/chunk{i}.parquet`
pub fn vertex_chunk_path(type_name: &str, group: &str, chunk: usize) -> String {
    let group_dir = if group.is_empty() { "all".to_string() } else { group.to_string() };
    format!("vertex/{}/{}/chunk{}.parquet", type_name, group_dir, chunk)
}

/// Edge adjacency chunk path: `edge/{triple}/{adj_type}/adj_list/part{src}/chunk{edge}.parquet`
pub fn edge_chunk_path(src: &str, edge: &str, dst: &str, adj_type: &str, src_chunk: usize, edge_chunk: usize) -> String {
    format!("edge/{}_{}_{}/{}/adj_list/part{}/chunk{}.parquet", src, edge, dst, adj_type, src_chunk, edge_chunk)
}

/// Vertex count sidecar: `vertex/{type}/vertex_count`
pub fn vertex_count_path(type_name: &str) -> String {
    format!("vertex/{}/vertex_count", type_name)
}

/// Edge vertex count sidecar: `edge/{triple}/{adj_type}/vertex_count`
pub fn edge_vertex_count_path(src: &str, edge: &str, dst: &str, adj_type: &str) -> String {
    format!("edge/{}_{}_{}/{}/vertex_count", src, edge, dst, adj_type)
}

/// Edge count sidecar: `edge/{triple}/{adj_type}/edge_count{chunk}`
pub fn edge_count_path(src: &str, edge: &str, dst: &str, adj_type: &str, chunk: usize) -> String {
    format!("edge/{}_{}_{}/{}/edge_count{}", src, edge, dst, adj_type, chunk)
}
