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
    /// Create from a LazyFrame (internal use only)
    pub(crate) fn new(lf: LazyFrame) -> Self {
        Self(lf)
    }

    /// Select columns/expressions (stays lazy)
    pub fn select<E: AsRef<[Expr]>>(self, exprs: E) -> Self {
        Self(self.0.select(exprs))
    }

    /// Filter rows based on a predicate (stays lazy)
    pub fn filter(self, expr: Expr) -> Self {
        Self(self.0.filter(expr))
    }

    /// Add or replace a column (stays lazy)
    pub fn with_column(self, expr: Expr) -> Self {
        Self(self.0.with_column(expr))
    }

    /// Apply a slice (offset, length) to the LazyFrame (stays lazy)
    pub fn slice(self, offset: i64, length: u32) -> Self {
        Self(self.0.slice(offset, length))
    }

    /// Clone the underlying LazyFrame (for batched execution)
    pub fn clone_inner(&self) -> Self {
        Self(self.0.clone())
    }

    /// Get the schema of the LazyFrame
    pub fn collect_schema(&mut self) -> PolarsResult<Arc<Schema>> {
        self.0.collect_schema()
    }

    /// Inner join two SafeLazyFrames (stays lazy)
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

impl Clone for SafeLazyFrame {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safe_lazy_frame_transformations() {
        // Create a simple DataFrame
        let df = df! {
            "name" => &["Alice", "Bob", "Charlie"],
            "age" => &[25, 30, 35]
        }
        .unwrap();

        // Wrap in SafeLazyFrame
        let safe_lf = SafeLazyFrame::new(df.lazy());

        // Apply transformations - all stay lazy
        let filtered = safe_lf
            .filter(col("age").gt(lit(26)))
            .select([col("name")]);

        // Can only materialize through into_inner() within the crate
        let result = filtered.into_inner().collect().unwrap();
        assert_eq!(result.height(), 2); // Bob and Charlie
    }

    #[test]
    fn test_clone_inner() {
        let df = df! {
            "x" => &[1, 2, 3]
        }
        .unwrap();

        let safe_lf = SafeLazyFrame::new(df.lazy());
        let cloned = safe_lf.clone_inner();

        // Both should work independently
        let result1 = safe_lf.into_inner().collect().unwrap();
        let result2 = cloned.into_inner().collect().unwrap();

        assert_eq!(result1.height(), result2.height());
    }
}
