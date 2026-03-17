use polars::prelude::PlPath;
use polars::prelude::cloud::CloudOptions;

/// A resolved path ready for I/O: physical URL + opaque cloud credentials.
/// This is the transversal type that ensures credentials always travel with
/// the path. Fields are private — use methods to interact.
#[derive(Clone)]
pub struct ResolvedPath {
    path: PlPath,
    cloud_options: Option<CloudOptions>,
}

impl ResolvedPath {
    pub fn new(url: &str, cloud_options: Option<CloudOptions>) -> Self {
        Self {
            path: PlPath::from_str(url),
            cloud_options,
        }
    }

    /// Derive a sub-path that inherits cloud credentials.
    pub fn join(&self, rel: &str) -> Self {
        Self {
            path: self.path.as_ref().join(rel),
            cloud_options: self.cloud_options.clone(),
        }
    }

    pub fn pl_path(&self) -> &PlPath {
        &self.path
    }

    pub fn cloud_options(&self) -> Option<&CloudOptions> {
        self.cloud_options.as_ref()
    }

    pub fn to_str(&self) -> &str {
        self.path.to_str()
    }
}

impl std::fmt::Debug for ResolvedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedPath")
            .field("path", &self.to_str())
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
        Ok(ResolvedPath::new(raw_path, None))
    }
}
