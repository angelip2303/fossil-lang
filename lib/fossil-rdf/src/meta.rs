use polars::prelude::*;

use crate::error::RdfGraphError;

/// Metadata for an RDF dataset, loaded from `_meta.parquet` and `_types.parquet`.
#[derive(Debug, Clone)]
pub struct DatasetMeta {
    pub predicate_stats: DataFrame,
    pub types: Vec<TypeMeta>,
}

/// Metadata for a single RDF type.
#[derive(Debug, Clone)]
pub struct TypeMeta {
    pub iri: String,
    pub dir: String,
}

/// Statistics for a single field (predicate).
#[derive(Debug, Clone)]
pub struct FieldStats {
    pub predicate: String,
    pub count: u64,
    pub n_unique: u64,
    pub min: Option<String>,
    pub max: Option<String>,
}

impl DatasetMeta {
    pub(crate) fn load(base: &str) -> Result<Self, RdfGraphError> {
        Self::load_with_args(base, &ScanArgsParquet::default())
    }

    pub(crate) fn load_with_args(base: &str, args: &ScanArgsParquet) -> Result<Self, RdfGraphError> {
        let meta_path = format!("{base}/_meta.parquet");
        let types_path = format!("{base}/_types.parquet");

        let predicate_stats = LazyFrame::scan_parquet(PlPath::from_str(&meta_path), args.clone())
            .map_err(|e| RdfGraphError::MissingIndex(format!("_meta.parquet: {e}")))?
            .collect()?;

        let types_df = LazyFrame::scan_parquet(PlPath::from_str(&types_path), args.clone())
            .map_err(|e| RdfGraphError::MissingIndex(format!("_types.parquet: {e}")))?
            .collect()?;

        let types = Self::parse_types(&types_df)?;

        Ok(Self {
            predicate_stats,
            types,
        })
    }

    fn parse_types(df: &DataFrame) -> Result<Vec<TypeMeta>, RdfGraphError> {
        let iris = df.column("type_iri")?.str()?;
        let dirs = df.column("type_dir")?.str()?;

        Ok(iris
            .into_iter()
            .zip(dirs.into_iter())
            .filter_map(|(iri, dir)| {
                Some(TypeMeta {
                    iri: iri?.to_string(),
                    dir: dir?.to_string(),
                })
            })
            .collect())
    }

    /// Get statistics for a specific predicate.
    pub fn field_stats(&self, predicate: &str) -> Result<FieldStats, RdfGraphError> {
        let filtered = self
            .predicate_stats
            .clone()
            .lazy()
            .filter(col("predicate").eq(lit(predicate)))
            .collect()?;

        if filtered.height() == 0 {
            return Err(RdfGraphError::Other(format!(
                "predicate not found: {predicate}"
            )));
        }

        let row = filtered.get(0).unwrap();

        Ok(FieldStats {
            predicate: predicate.to_string(),
            count: row[1].try_extract::<u64>().unwrap_or(0),
            n_unique: row[2].try_extract::<u64>().unwrap_or(0),
            min: row[3].get_str().map(|s| s.to_string()),
            max: row[4].get_str().map(|s| s.to_string()),
        })
    }
}
