use std::collections::HashMap;

/// A resolved path ready for I/O: physical URL + credentials for this specific resource.
#[derive(Debug, Clone)]
pub struct ResolvedPath {
    pub url: String,
    pub credentials: HashMap<String, String>,
}

/// Host-provided path resolution. Fossil passes raw path strings, host returns URL + credentials.
pub trait PathResolver: Send + Sync + std::fmt::Debug {
    fn resolve(&self, raw_path: &str) -> Result<ResolvedPath, String>;
}

/// Default resolver for standalone Fossil: local/cloud pass through, @ rejected.
/// Cloud credentials come from env vars (Polars default behavior with empty credentials map).
#[derive(Debug)]
pub struct DefaultPathResolver;

impl PathResolver for DefaultPathResolver {
    fn resolve(&self, raw_path: &str) -> Result<ResolvedPath, String> {
        if raw_path.starts_with('@') {
            return Err(format!(
                "Host references ({raw_path}) not available in standalone mode"
            ));
        }
        Ok(ResolvedPath {
            url: raw_path.to_string(),
            credentials: HashMap::new(),
        })
    }
}
