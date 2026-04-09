//! SourcePlugin — data format providers (csv, excel, pdf, ifc, ...).
//!
//! Thin: no I/O. Tells the compiler what schema a source has and
//! tells the host how to load it (SQL or preprocess).

use std::collections::HashMap;

/// Information about a source parameter.
#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub name: &'static str,
    pub required: bool,
    pub expected_type: Option<&'static str>,
}

/// How the compiler gets the schema of a source.
#[derive(Debug, Clone)]
pub enum SchemaSource {
    /// Host executes this SQL to get column info (e.g., "DESCRIBE SELECT * FROM read_csv(...)").
    Sql(String),
    /// Schema is always the same (e.g., PDF element schema).
    Static(Vec<ColumnDef>),
}

/// Column definition for static schemas.
#[derive(Debug, Clone)]
pub struct ColumnDef {
    pub name: String,
    pub data_type: String,
    pub nullable: bool,
}

/// How the host loads data from this source.
#[derive(Debug, Clone)]
pub enum LoadAction {
    /// Execute SQL directly (e.g., "SELECT * FROM read_csv('path', ...)").
    Sql(String),
    /// Requires host-side preprocessing (e.g., PDF text extraction).
    Preprocess(PreprocessConfig),
}

/// Configuration for host-side preprocessing.
#[derive(Debug, Clone)]
pub struct PreprocessConfig {
    /// Handler name registered in the host (e.g., "pdf_extract").
    pub handler: String,
    /// Source file path.
    pub path: String,
    /// Expected output schema after preprocessing.
    pub schema: Vec<ColumnDef>,
    /// Additional parameters.
    pub params: HashMap<String, String>,
}

/// Trait for data source plugins.
pub trait SourcePlugin: Send + Sync {
    /// Provider name used in scripts (e.g., "csv" for `csv!(...)`).
    fn name(&self) -> &'static str;

    /// File extensions this provider handles.
    fn extensions(&self) -> &'static [&'static str];

    /// Parameters accepted by the provider invocation.
    fn params(&self) -> Vec<ParamInfo>;

    /// How to get the schema at compile time.
    fn schema_source(&self, path: &str, args: &HashMap<String, String>) -> SchemaSource;

    /// How to load the data at runtime.
    fn load(&self, path: &str, args: &HashMap<String, String>) -> LoadAction;
}
