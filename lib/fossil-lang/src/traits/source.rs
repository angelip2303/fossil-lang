//! Source trait for data providers
//!
//! This trait abstracts the data source, allowing external providers
//! (CSV, JSON, etc.) to plug into the core without coupling.

use std::fmt::Debug;

use polars::prelude::{LazyFrame, PolarsResult};

/// A data source that can produce a LazyFrame
///
/// Implementations provide the `to_lazy_frame` method to materialize
/// the source into a Polars LazyFrame for processing.
///
/// # Example
///
/// ```ignore
/// struct CsvSource { path: String, delimiter: u8, has_header: bool }
///
/// impl Source for CsvSource {
///     fn to_lazy_frame(&self) -> PolarsResult<LazyFrame> {
///         LazyCsvReader::new(&self.path)
///             .with_separator(self.delimiter)
///             .with_has_header(self.has_header)
///             .finish()
///     }
/// }
/// ```
pub trait Source: Send + Sync + Debug {
    /// Convert this source to a LazyFrame
    fn to_lazy_frame(&self) -> PolarsResult<LazyFrame>;

    /// Clone this source into a boxed trait object
    fn box_clone(&self) -> Box<dyn Source>;
}

impl Clone for Box<dyn Source> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}
