use std::collections::HashMap;

/// A resolved path ready for I/O: physical URL + cloud credentials.
#[derive(Clone)]
pub struct ResolvedPath {
    url: String,
    #[cfg(feature = "polars")]
    path: polars::prelude::PlPath,
    #[cfg(feature = "polars")]
    cloud_options: Option<polars::prelude::cloud::CloudOptions>,
    cloud_config: HashMap<String, String>,
}

impl ResolvedPath {
    #[cfg(feature = "polars")]
    pub fn new(url: &str, cloud_options: Option<polars::prelude::cloud::CloudOptions>) -> Self {
        Self {
            url: url.to_string(),
            path: polars::prelude::PlPath::from_str(url),
            cloud_options,
            cloud_config: HashMap::new(),
        }
    }

    #[cfg(not(feature = "polars"))]
    pub fn new(url: &str, _cloud_options: Option<()>) -> Self {
        Self {
            url: url.to_string(),
            cloud_config: HashMap::new(),
        }
    }

    #[cfg(feature = "polars")]
    pub fn with_config(
        url: &str,
        cloud_options: Option<polars::prelude::cloud::CloudOptions>,
        cloud_config: HashMap<String, String>,
    ) -> Self {
        Self {
            url: url.to_string(),
            path: polars::prelude::PlPath::from_str(url),
            cloud_options,
            cloud_config,
        }
    }

    pub fn join(&self, rel: &str) -> Self {
        let new_url = format!("{}/{}", self.url.trim_end_matches('/'), rel);
        Self {
            url: new_url.clone(),
            #[cfg(feature = "polars")]
            path: polars::prelude::PlPath::from_str(&new_url),
            #[cfg(feature = "polars")]
            cloud_options: self.cloud_options.clone(),
            cloud_config: self.cloud_config.clone(),
        }
    }

    #[cfg(feature = "polars")]
    pub fn pl_path(&self) -> &polars::prelude::PlPath { &self.path }

    #[cfg(feature = "polars")]
    pub fn cloud_options(&self) -> Option<&polars::prelude::cloud::CloudOptions> { self.cloud_options.as_ref() }

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

/// Default resolver: local/cloud pass through, @ rejected.
#[derive(Debug)]
pub struct DefaultPathResolver;

impl PathResolver for DefaultPathResolver {
    fn resolve(&self, raw_path: &str) -> Result<ResolvedPath, String> {
        if raw_path.starts_with('@') {
            return Err(format!("Host references ({raw_path}) not available in standalone mode"));
        }
        Ok(ResolvedPath::new(raw_path, None))
    }
}
