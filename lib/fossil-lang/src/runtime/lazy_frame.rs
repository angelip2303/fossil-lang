use polars::prelude::*;

/// Wrapper around Polars LazyFrame that prevents accidental materialization.
///
/// `.collect()` is NOT exposed - only ChunkedExecutor can materialize data
/// through the `into_inner()` method which is `pub(crate)`.
///
/// This guarantees streaming/lazy evaluation throughout the pipeline.
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

    pub fn slice(self, offset: i64, length: u32) -> Self {
        Self(self.0.slice(offset, length))
    }

    pub fn join(self, other: Self, left_on: Vec<Expr>, right_on: Vec<Expr>, args: JoinArgs) -> Self {
        Self(self.0.join(other.0, left_on, right_on, args))
    }

    pub(crate) fn into_inner(self) -> LazyFrame {
        self.0
    }
}
