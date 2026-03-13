use polars::prelude::cloud::CloudOptions;

/// A resolved path ready for I/O: physical URL + opaque cloud credentials.
#[derive(Clone)]
pub struct ResolvedPath {
    pub url: String,
    pub cloud_options: Option<CloudOptions>,
}

impl std::fmt::Debug for ResolvedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedPath")
            .field("url", &self.url)
            .field("cloud_options", &self.cloud_options.as_ref().map(|_| "***"))
            .finish()
    }
}

/// Host-provided path resolution. Fossil passes raw path strings, host returns URL + cloud options.
pub trait PathResolver: Send + Sync + std::fmt::Debug {
    fn resolve(&self, raw_path: &str) -> Result<ResolvedPath, String>;
}

/// Default resolver for standalone Fossil: local/cloud pass through, @ rejected.
/// Cloud credentials come from env vars (Polars default behavior).
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
            cloud_options: None,
        })
    }
}
