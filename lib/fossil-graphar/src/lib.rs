//! GraphAr format library — types, YAML serialization, and path helpers.
//!
//! GraphAr is a standard format for storing property graphs as columnar
//! (Parquet) files. This crate handles the FORMAT only — no I/O, no
//! execution. Pure data structures + serialization.
//!
//! Reference: <https://graphar.apache.org/docs/specification/format/>

pub mod paths;
pub mod types;
pub mod yaml;

pub use paths::*;
pub use types::*;
