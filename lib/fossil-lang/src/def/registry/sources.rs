//! Source registry: data readers (csv, parquet, json, ...).
//!
//! Hosts register sources via `FossilDbBuilder::with_source(SourceDef::new(...))`.
//! Sources are looked up by name during AST → IR lowering.

/// Registry of data sources. Stored inside `FossilRegistry`.
#[derive(Debug, Clone, Default)]
pub struct SourceRegistry {
    entries: Vec<SourceDef>,
}

impl SourceRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, def: SourceDef) {
        self.entries.push(def);
    }

    pub fn find(&self, name: &str) -> Option<&SourceDef> {
        self.entries.iter().find(|s| s.name == name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SourceDef> {
        self.entries.iter()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// A data source declaration. The host provides one per supported format
/// (csv, parquet, etc.); the compiler uses the params for validation and
/// the schema for type inference.
#[derive(Debug, Clone)]
pub struct SourceDef {
    pub name: String,
    pub params: Vec<ParamDef>,
    /// Static schema, or `None` if the host resolves it via `HasSchemaResolver`.
    pub schema: Option<Vec<(String, String)>>,
}

impl SourceDef {
    /// Constructor that accepts string-like name (handles `.into()` internally).
    pub fn new(name: impl Into<String>, params: Vec<ParamDef>) -> Self {
        Self {
            name: name.into(),
            params,
            schema: None,
        }
    }

    /// Builder: attach a static schema (column name + sql type pairs).
    pub fn with_schema(mut self, schema: Vec<(String, String)>) -> Self {
        self.schema = Some(schema);
        self
    }

    /// Convenience: build schema from `&[(&str, &str)]` slices, no manual `.into()`.
    pub fn with_schema_pairs(self, pairs: &[(&str, &str)]) -> Self {
        self.with_schema(
            pairs
                .iter()
                .map(|(n, t)| (n.to_string(), t.to_string()))
                .collect(),
        )
    }
}

/// Parameter declaration for a source.
#[derive(Debug, Clone)]
pub struct ParamDef {
    pub name: String,
    pub required: bool,
    pub default: String,
}

impl ParamDef {
    pub fn required(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            required: true,
            default: String::new(),
        }
    }

    pub fn with_default(name: impl Into<String>, default: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            required: false,
            default: default.into(),
        }
    }
}
