//! Salsa database for the Fossil compiler.
//!
//! The database is the central state container. All compilation passes
//! are Salsa tracked queries on this database. The host (keasy) creates
//! the database, sets inputs, and calls queries.
//!
//! Reference: Salsa Calc tutorial, rust-analyzer RootDatabase.

use salsa::Database as Db;

/// The Fossil compiler database.
///
/// All compiler state lives here. Queries are memoized and incrementally
/// recomputed when inputs change.
#[salsa::db]
#[derive(Clone, Default)]
pub struct FossilDb {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for FossilDb {}

/// Source file input — the root of all compilation queries.
#[salsa::input(debug)]
pub struct SourceFile {
    /// The source text of the Fossil script.
    #[returns(ref)]
    pub text: String,

    /// Display name for error messages.
    #[returns(ref)]
    pub name: String,
}

/// Interned word — replaces the custom Interner + Symbol.
///
/// Same string → same WordId. Comparison is O(1).
/// Used for identifiers, field names, type names, etc.
#[salsa::interned(debug)]
pub struct Word<'db> {
    #[returns(ref)]
    pub text: String,
}

/// Diagnostic accumulator — replaces manual FossilErrors collection.
///
/// During any query, push diagnostics with `.accumulate(db)`.
/// Collect with `query_fn::accumulated::<Diagnostic>(&db, ...)`.
#[salsa::accumulator]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub offset: usize,
    pub len: usize,
    pub severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}
