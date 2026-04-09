//! Salsa database for the Fossil compiler.
//!
//! The database is the central state container. All compilation passes
//! are Salsa tracked queries on this database. The host (keasy) creates
//! the database, sets inputs, and calls queries.
//!
//! Reference: Salsa Calc tutorial, rust-analyzer RootDatabase.


/// Host capability: resolve source schemas at compile time.
///
/// Implement this trait on your database struct to provide schema resolution.
/// Same role as rust-analyzer's `HasLogger` or Malloy's `Connection`.
pub trait HasSchemaResolver {
    /// Resolve the schema of a data source at compile time.
    ///
    /// Returns `[(column_name, sql_type)]`. The compiler maps sql_type → PrimitiveType.
    /// For cloud sources, the host pre-resolves schemas via `schema_needs` and caches here.
    fn source_schema(&self, provider: &str, path: &str) -> Option<Vec<(String, String)>>;
}

/// Compiler database trait. All tracked functions use `&dyn Db`.
///
/// Combines Salsa's `Database` with host schema resolution.
/// Pattern: rust-analyzer `LogDatabase` = `HasLogger + Database`.
#[salsa::db]
pub trait Db: HasSchemaResolver + salsa::Database {}

#[salsa::db]
impl<T: HasSchemaResolver + salsa::Database> Db for T {}

/// Default compiler database (tests, pure compilation without I/O).
#[salsa::db]
#[derive(Clone, Default)]
pub struct FossilDb {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for FossilDb {}

impl HasSchemaResolver for FossilDb {
    fn source_schema(&self, _provider: &str, _path: &str) -> Option<Vec<(String, String)>> {
        None
    }
}

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

