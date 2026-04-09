//! Salsa database for the Fossil compiler.
//!
//! The database is the central state container. All compilation passes
//! are Salsa tracked queries on this database. The host (keasy) creates
//! the database, sets inputs, and calls queries.
//!
//! Reference: Salsa Calc tutorial, rust-analyzer RootDatabase.

use crate::context::Symbol;

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

// ── DefId: Salsa-interned definition identifier ────────────────────
//
// Replaces the old sequential DefId(u32) from context/mod.rs.
// Each definition (let, type, module, constructor) gets a globally unique
// interned id. Two DefIds with the same (namespace, name, kind) are equal.
//
// Architecture: `InternedDef<'db>` is the Salsa interned type (carries 'db
// lifetime). `DefId` is a lifetime-free wrapper around `salsa::Id` that can
// be stored freely in IR data structures. Convert between them:
//   - `DefId::from(interned_def)` — erase lifetime
//   - `def_id.lookup(db)` — restore InternedDef<'db>

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefKindTag {
    Mod,
    Let,
    Type,
    RecordConstructor,
}

#[salsa::interned]
pub struct InternedDef<'db> {
    pub namespace: Option<Symbol>,
    pub name: Symbol,
    pub kind: DefKindTag,
}

/// Lifetime-free definition identifier. Wraps a `salsa::Id` so it can be
/// stored in IR arenas, HashMaps, etc. without propagating `'db`.
///
/// Create via `DefId::new(db, namespace, name, kind)`.
/// Access fields via `def_id.name(db)`, `def_id.kind(db)`, etc.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(salsa::Id);

impl std::fmt::Debug for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DefId({:?})", self.0)
    }
}

impl DefId {
    /// Create (or intern) a definition in the Salsa database.
    pub fn new(db: &dyn Db, namespace: Option<Symbol>, name: Symbol, kind: DefKindTag) -> Self {
        let interned = InternedDef::new(db, namespace, name, kind);
        Self(salsa::plumbing::AsId::as_id(&interned))
    }

    /// Recover the Salsa interned value to access fields.
    fn lookup<'db>(self, _db: &'db dyn Db) -> InternedDef<'db> {
        // InternedDef is (Id, PhantomData) — FromId reconstructs it from the raw Id.
        // The `_db` parameter enforces the caller has a valid database reference,
        // ensuring the salsa::Id is valid for the 'db lifetime.
        <InternedDef<'db> as salsa::plumbing::FromId>::from_id(self.0)
    }

    pub fn name(self, db: &dyn Db) -> Symbol {
        self.lookup(db).name(db)
    }

    pub fn namespace(self, db: &dyn Db) -> Option<Symbol> {
        self.lookup(db).namespace(db)
    }

    pub fn kind(self, db: &dyn Db) -> DefKindTag {
        self.lookup(db).kind(db)
    }
}

impl From<InternedDef<'_>> for DefId {
    fn from(interned: InternedDef<'_>) -> Self {
        Self(salsa::plumbing::AsId::as_id(&interned))
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

