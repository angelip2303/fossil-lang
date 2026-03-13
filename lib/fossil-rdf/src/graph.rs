use std::collections::HashMap;

use polars::prelude::*;

use crate::error::RdfGraphError;
use crate::meta::{DatasetMeta, FieldStats, TypeMeta};

const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// Client for reading RDF Parquet datasets.
///
/// All query methods return `LazyFrame` — zero data is read until `.collect()`.
/// Polars handles row group pushdown, predicate pushdown, and HTTP range requests
/// on cloud storage transparently.
pub struct RdfGraph {
    base: String,
    meta: DatasetMeta,
    scan_args: ScanArgsParquet,
}

impl RdfGraph {
    /// Open an RDF Parquet dataset from a local path.
    pub fn open(base: &str) -> Result<Self, RdfGraphError> {
        Self::open_with_options(base, &HashMap::new())
    }

    /// Open an RDF Parquet dataset, optionally from cloud storage with credentials.
    pub fn open_with_options(base: &str, creds: &HashMap<String, String>) -> Result<Self, RdfGraphError> {
        let scan_args = Self::build_scan_args(base, creds);
        let meta = DatasetMeta::load_with_args(base, &scan_args)?;
        Ok(Self {
            base: base.to_string(),
            meta,
            scan_args,
        })
    }

    fn build_scan_args(base: &str, creds: &HashMap<String, String>) -> ScanArgsParquet {
        let is_cloud = base.starts_with("s3://")
            || base.starts_with("gs://")
            || base.starts_with("az://")
            || base.starts_with("abfss://");

        if !is_cloud || creds.is_empty() {
            return ScanArgsParquet::default();
        }

        #[cfg(feature = "cloud")]
        {
            let mut opts = polars::io::cloud::CloudOptions::default();
            for (key, value) in creds {
                opts = opts.with_config(key.clone(), value.clone());
            }
            ScanArgsParquet {
                cloud_options: Some(opts),
                ..Default::default()
            }
        }

        #[cfg(not(feature = "cloud"))]
        ScanArgsParquet::default()
    }

    fn scan(&self, path: &str) -> Result<LazyFrame, RdfGraphError> {
        LazyFrame::scan_parquet(PlPath::from_str(path), self.scan_args.clone())
            .map_err(|e| RdfGraphError::MissingIndex(format!("{path}: {e}")))
    }

    /// Access the cached dataset metadata.
    pub fn meta(&self) -> &DatasetMeta {
        &self.meta
    }

    /// List all types in the dataset.
    pub fn types(&self) -> &[TypeMeta] {
        &self.meta.types
    }

    /// Get statistics for a field (predicate).
    pub fn field_stats(&self, predicate: &str) -> Result<FieldStats, RdfGraphError> {
        self.meta.field_stats(predicate)
    }

    /// Total triple count from `_meta.parquet` (no data scan).
    pub fn triple_count(&self) -> u64 {
        self.meta.predicate_stats
            .column("count")
            .ok()
            .and_then(|c| c.u64().ok())
            .map(|ca| ca.sum().unwrap_or(0))
            .unwrap_or(0)
    }

    /// Query all triples for a specific subject (SPO index).
    pub fn entity(&self, subject: &str) -> Result<LazyFrame, RdfGraphError> {
        let path = format!("{}/_subjects.parquet", self.base);
        let lf = self.scan(&path)?;
        Ok(lf.filter(col("subject").eq(lit(subject))))
    }

    /// Reverse lookup: find all triples that reference a given object (OPS index).
    pub fn references_to(&self, object: &str) -> Result<LazyFrame, RdfGraphError> {
        let path = format!("{}/_objects.parquet", self.base);
        let lf = self.scan(&path)?;
        Ok(lf.filter(col("object").eq(lit(object))))
    }

    /// Query all (subject, object) pairs for a predicate (PSO index).
    pub fn predicate(&self, uri: &str) -> Result<LazyFrame, RdfGraphError> {
        let name = predicate_to_filename(uri);
        let path = format!("{}/_predicates/{}.parquet", self.base, name);
        self.scan(&path)
    }

    /// Read a pre-computed join (ExtVP).
    pub fn join(&self, ref_field: &str) -> Result<LazyFrame, RdfGraphError> {
        let path = format!("{}/_joins/{}.parquet", self.base, ref_field);
        self.scan(&path)
    }

    /// Read a reverse pre-computed join (ExtVP).
    pub fn join_reverse(&self, ref_field: &str) -> Result<LazyFrame, RdfGraphError> {
        let path = format!("{}/_joins/{}.reverse.parquet", self.base, ref_field);
        self.scan(&path)
    }

    /// All triples — scan `_subjects.parquet` unfiltered.
    pub fn all_triples(&self) -> Result<LazyFrame, RdfGraphError> {
        let path = format!("{}/_subjects.parquet", self.base);
        self.scan(&path)
    }

    /// Smart pattern match — auto-picks the best index.
    pub fn triples(
        &self,
        s: Option<&str>,
        p: Option<&str>,
        o: Option<&str>,
    ) -> Result<LazyFrame, RdfGraphError> {
        let base = match (s, p, o) {
            (Some(subject), _, _) => self.entity(subject)?,
            (_, Some(predicate), _) => self.predicate(predicate)?
                .with_column(lit(predicate).alias("predicate"))
                .with_column(lit("").alias("object_datatype")),
            (_, _, Some(object)) => self.references_to(object)?,
            (None, None, None) => self.all_triples()?,
        };

        let mut lf = base;
        // Apply remaining filters
        if s.is_some() {
            if let Some(p) = p {
                lf = lf.filter(col("predicate").eq(lit(p)));
            }
            if let Some(o) = o {
                lf = lf.filter(col("object").eq(lit(o)));
            }
        } else if p.is_some() {
            if let Some(o) = o {
                lf = lf.filter(col("object").eq(lit(o)));
            }
        } else if o.is_some() {
            if let Some(p) = p {
                lf = lf.filter(col("predicate").eq(lit(p)));
            }
        }

        Ok(lf)
    }

    /// Search by `rdfs:label`. Returns collected DataFrame: `[subject, label, type, description]`.
    pub fn search(&self, query: &str, limit: u32) -> Result<DataFrame, RdfGraphError> {
        let query_lower = query.to_lowercase();

        let labels = self.predicate(RDFS_LABEL)?
            .filter(
                col("object")
                    .str()
                    .to_lowercase()
                    .str()
                    .contains_literal(lit(query_lower.as_str())),
            )
            .select([
                col("subject"),
                col("object").alias("label"),
            ])
            .limit(limit);

        // Join with rdf:type for grouping
        let types = self.predicate(RDF_TYPE)?
            .select([
                col("subject"),
                col("object").alias("type"),
            ]);

        // Join with rdfs:comment for description
        let comments = self.predicate(RDFS_COMMENT)?
            .select([
                col("subject"),
                col("object").alias("description"),
            ]);

        let result = labels
            .join(types, [col("subject")], [col("subject")], JoinType::Left.into())
            .join(comments, [col("subject")], [col("subject")], JoinType::Left.into())
            .limit(limit)
            .collect()?;

        Ok(result)
    }

    /// 1-hop neighborhood: outgoing + incoming triples.
    pub fn expand(&self, iri: &str, limit: u32) -> Result<DataFrame, RdfGraphError> {
        let outgoing = self.entity(iri)?;
        let incoming = self.references_to(iri)?;
        concat([outgoing, incoming], UnionArgs::default())?
            .limit(limit)
            .collect()
            .map_err(Into::into)
    }
}

/// Convert a predicate URI to a filename (last segment after `#` or `/`, lowercased).
fn predicate_to_filename(uri: &str) -> String {
    let name = uri
        .rsplit_once('#')
        .or_else(|| uri.rsplit_once('/'))
        .map(|(_, name)| name)
        .unwrap_or(uri);
    name.to_lowercase()
}
