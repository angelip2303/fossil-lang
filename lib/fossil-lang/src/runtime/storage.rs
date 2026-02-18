use std::collections::HashMap;

/// Strategy for resolving cloud storage credentials.
/// Empty = local mode / env var fallback (CLI, tests, local dev).
/// Populated = explicit credentials (keasy server mode).
#[derive(Debug, Clone, Default)]
pub struct StorageConfig(HashMap<String, String>);

impl StorageConfig {
    pub fn new(map: HashMap<String, String>) -> Self {
        Self(map)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(|s| s.as_str())
    }

    pub fn as_map(&self) -> &HashMap<String, String> {
        &self.0
    }
}
