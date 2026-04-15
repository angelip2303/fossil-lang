//! Sink registry: data writers (Rdf.materialize, future Turtle, JsonLd...).
//!
//! Sinks are language built-ins (registered via `register_defaults`).
//! They identify by qualified name (namespace.name) and carry a logical format.

/// Registry of sinks. Stored inside `FossilRegistry`.
#[derive(Debug, Clone, Default)]
pub struct SinkRegistry {
    entries: Vec<SinkDef>,
}

impl SinkRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, def: SinkDef) {
        self.entries.push(def);
    }

    pub fn find(&self, namespace: &str, name: &str) -> Option<&SinkDef> {
        self.entries
            .iter()
            .find(|s| s.namespace == namespace && s.name == name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SinkDef> {
        self.entries.iter()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// A sink declaration. Identified by qualified name (namespace.name).
#[derive(Debug, Clone)]
pub struct SinkDef {
    pub namespace: String,
    pub name: String,
    pub format: SinkFormat,
}

impl SinkDef {
    pub fn new(
        namespace: impl Into<String>,
        name: impl Into<String>,
        format: SinkFormat,
    ) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            format,
        }
    }
}

/// Logical sink output format. Backend-independent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SinkFormat {
    Graphar,
    // Future: Turtle, JsonLd, Parquet, etc.
}

impl SinkFormat {
    pub fn as_str(self) -> &'static str {
        match self {
            SinkFormat::Graphar => "graphar",
        }
    }
}
