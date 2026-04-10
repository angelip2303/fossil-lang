use std::collections::HashMap;

/// A resolved path ready for I/O: URL + cloud credentials.
#[derive(Clone)]
pub struct ResolvedPath {
    url: String,
    cloud_config: HashMap<String, String>,
}

impl ResolvedPath {
    pub fn new(url: &str, _cloud_options: Option<()>) -> Self {
        Self { url: url.to_string(), cloud_config: HashMap::new() }
    }

    pub fn with_config(url: &str, cloud_config: HashMap<String, String>) -> Self {
        Self { url: url.to_string(), cloud_config }
    }

    pub fn join(&self, rel: &str) -> Self {
        Self {
            url: format!("{}/{}", self.url.trim_end_matches('/'), rel),
            cloud_config: self.cloud_config.clone(),
        }
    }

    pub fn cloud_config(&self) -> &HashMap<String, String> { &self.cloud_config }
    pub fn to_str(&self) -> &str { &self.url }
}

impl std::fmt::Debug for ResolvedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedPath").field("url", &self.url).finish()
    }
}

/// Host-provided path resolution.
pub trait PathResolver: Send + Sync + std::fmt::Debug {
    fn resolve(&self, raw_path: &str) -> Result<ResolvedPath, String>;
}

