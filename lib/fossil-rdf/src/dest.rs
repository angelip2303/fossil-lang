use polars::prelude::*;
use polars::prelude::cloud::CloudOptions;

use crate::error::RdfError;

/// Destination for a GraphAr property graph.
///
/// Wraps a base path and optional cloud credentials.  DuckDB operations
/// always work on local temp files — cloud uploads go through Polars sink.
pub struct GraphDest {
    base: PlPath,
    cloud_opts: Option<CloudOptions>,
}

impl GraphDest {
    pub fn local(path: &str) -> Self {
        Self {
            base: PlPath::from_str(path),
            cloud_opts: None,
        }
    }

    pub fn cloud(path: &str, opts: CloudOptions) -> Self {
        Self {
            base: PlPath::from_str(path),
            cloud_opts: Some(opts),
        }
    }

    pub fn join(&self, rel: &str) -> Self {
        Self {
            base: self.base.as_ref().join(rel),
            cloud_opts: self.cloud_opts.clone(),
        }
    }

    pub fn base(&self) -> &PlPath {
        &self.base
    }

    pub fn to_str(&self) -> &str {
        self.base.to_str()
    }

    pub fn is_cloud(&self) -> bool {
        self.base.is_cloud_url()
    }

    /// Sink a LazyFrame to a relative path under this destination.
    pub(crate) fn sink(&self, lf: LazyFrame, rel: &str) -> Result<(), RdfError> {
        let sub = self.join(rel);
        lf.sink_parquet(
            SinkTarget::Path(sub.base.clone()),
            super::materializer::parquet_options(),
            sub.cloud_opts.clone(),
            super::materializer::SINK_OPTIONS,
        )
        .map_err(super::materializer::polars_write_err)?
        .collect()
        .map_err(super::materializer::polars_write_err)?;
        Ok(())
    }
}
