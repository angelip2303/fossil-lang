//! Source trait for data providers (legacy, Polars-based).
//!
//! Gated behind the `polars` feature. New code should use FunctionDef + OpImpl.

#[cfg(feature = "polars")]
use std::fmt::Debug;

#[cfg(feature = "polars")]
use polars::prelude::{LazyFrame, PolarsResult, Schema};

/// A data source that can produce a LazyFrame for lazy/streaming execution.
///
/// Implementations provide `scan()` which returns a Polars LazyFrame.
/// All downstream operations (select, filter, join) compose lazily on this frame.
///
/// # Example
///
/// ```ignore
/// struct CsvSource { path: String, delimiter: u8, has_header: bool }
///
/// impl Source for CsvSource {
///     fn scan(&self) -> PolarsResult<LazyFrame> {
///         LazyCsvReader::new(&self.path)
///             .with_separator(self.delimiter)
///             .with_has_header(self.has_header)
///             .finish()
///     }
/// }
/// ```
#[cfg(feature = "polars")]
pub trait Source: Send + Sync + Debug {
    /// Produce a LazyFrame representing this data source.
    fn scan(&self) -> PolarsResult<LazyFrame>;

    /// Infer the schema by collecting the lazy frame's schema.
    fn infer_schema(&self) -> PolarsResult<Schema> {
        self.scan()?
            .collect_schema()
            .map(|arc| arc.as_ref().clone())
    }
}
