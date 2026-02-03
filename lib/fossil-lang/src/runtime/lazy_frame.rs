//! SafeLazyFrame wrapper for protected lazy evaluation
//!
//! This module provides a wrapper around Polars LazyFrame that prevents
//! accidental materialization. The `.collect()` method is NOT exposed -
//! only ChunkedExecutor can materialize data through `into_inner()`.
//!
//! This design guarantees streaming/lazy evaluation throughout the pipeline,
//! making it impossible to accidentally call `.collect()` outside of the
//! designated execution point.

use polars::prelude::*;

/// Wrapper around Polars LazyFrame that prevents accidental materialization.
///
/// `.collect()` is NOT exposed - only ChunkedExecutor can materialize data
/// through the `into_inner()` method which is `pub(crate)`.
///
/// This guarantees streaming/lazy evaluation throughout the pipeline.
///
/// # Example
/// ```ignore
/// // This compiles - lazy transformations are allowed
/// let safe_lf = SafeLazyFrame::new(lf);
/// let filtered = safe_lf.filter(col("age").gt(lit(18)));
///
/// // This does NOT compile - .collect() is not exposed
/// // let df = filtered.collect(); // ERROR!
///
/// // Only ChunkedExecutor can materialize through into_inner()
/// // let lf = filtered.into_inner(); // Only works within crate
/// ```
pub struct SafeLazyFrame(LazyFrame);

impl SafeLazyFrame {
    pub(crate) fn new(lf: LazyFrame) -> Self {
        Self(lf)
    }

    pub fn select<E: AsRef<[Expr]>>(self, exprs: E) -> Self {
        Self(self.0.select(exprs))
    }

    pub fn filter(self, expr: Expr) -> Self {
        Self(self.0.filter(expr))
    }

    pub fn with_column(self, expr: Expr) -> Self {
        Self(self.0.with_column(expr))
    }

    pub fn slice(self, offset: i64, length: u32) -> Self {
        Self(self.0.slice(offset, length))
    }

    pub fn collect_schema(&mut self) -> PolarsResult<Arc<Schema>> {
        self.0.collect_schema()
    }

    pub fn inner_join(self, other: Self, left_on: Expr, right_on: Expr) -> Self {
        Self(self.0.inner_join(other.0, left_on, right_on))
    }

    /// ONLY for ChunkedExecutor - converts to inner LazyFrame for batched collection.
    ///
    /// This is the ONLY way to get access to `.collect()`, and it's restricted
    /// to within this crate. External code cannot call this method.
    pub(crate) fn into_inner(self) -> LazyFrame {
        self.0
    }
}
