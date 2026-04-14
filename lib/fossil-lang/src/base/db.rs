//! Salsa database for the Fossil compiler.
//!
//! The database is the central state container. All compilation passes
//! are Salsa tracked queries on this database. The host (keasy) creates
//! the database, sets inputs, and calls queries.
//!
//! Reference: Salsa Calc tutorial, rust-analyzer RootDatabase, DataFusion SessionState.

use crate::registry::{
    register_defaults, FossilRegistry, SinkDef, SourceDef,
};

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

/// Host capability: provide the compiler registry (sources + sinks + attribute ops).
///
/// Same pattern as `HasSchemaResolver`. Mirror of DataFusion's `Session` trait
/// (SessionState exposes function/catalog registries via methods).
pub trait HasRegistry {
    fn registry(&self) -> &FossilRegistry;
}

/// Compiler database trait. All tracked functions use `&dyn Db`.
///
/// Combines Salsa's `Database` with host capabilities.
/// Pattern: rust-analyzer `LogDatabase` = `HasLogger + Database`.
#[salsa::db]
pub trait Db: HasSchemaResolver + HasRegistry + salsa::Database {}

#[salsa::db]
impl<T: HasSchemaResolver + HasRegistry + salsa::Database> Db for T {}

/// Default compiler database. The registry is immutable post-build:
/// changes require building a new `FossilDb` via `FossilDbBuilder`.
/// This contract preserves Salsa incrementality (no silent stale caches).
#[salsa::db]
#[derive(Clone)]
pub struct FossilDb {
    storage: salsa::Storage<Self>,
    registry: FossilRegistry,
}

#[salsa::db]
impl salsa::Database for FossilDb {}

impl FossilDb {
    /// Construct via the builder pattern. Mirror of DataFusion `SessionContext::new()`.
    pub fn builder() -> FossilDbBuilder {
        FossilDbBuilder::default()
    }

    /// Construct a `FossilDb` from a pre-built registry.
    /// Used by hosts that share a registry across multiple short-lived databases
    /// (e.g., one DB per compilation thread to avoid sharing non-Send Salsa storage).
    pub fn with_registry(registry: FossilRegistry) -> Self {
        Self {
            storage: Default::default(),
            registry,
        }
    }

    /// Get the registry. Useful for constructing other `FossilDb` instances
    /// that share the same registry.
    pub fn registry_clone(&self) -> FossilRegistry {
        self.registry.clone()
    }
}

impl Default for FossilDb {
    /// Default database with language defaults registered (Rdf.materialize sink,
    /// clean/anon attribute ops). Match DataFusion `SessionContext::new()`.
    /// For a truly empty database, use `FossilDbBuilder::new().build()`.
    fn default() -> Self {
        FossilDbBuilder::new().with_default_features().build()
    }
}

impl HasSchemaResolver for FossilDb {
    fn source_schema(&self, _provider: &str, _path: &str) -> Option<Vec<(String, String)>> {
        None
    }
}

impl HasRegistry for FossilDb {
    fn registry(&self) -> &FossilRegistry {
        &self.registry
    }
}

/// Builder for `FossilDb`. Mirror of DataFusion `SessionStateBuilder`.
#[derive(Default)]
pub struct FossilDbBuilder {
    registry: FossilRegistry,
}

impl FossilDbBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register language defaults: Rdf.materialize sink + clean/anon attribute ops.
    /// Mirror of DataFusion `SessionStateBuilder::with_default_features()`.
    pub fn with_default_features(mut self) -> Self {
        register_defaults(&mut self.registry);
        self
    }

    /// Register a data source (csv, parquet, etc.).
    pub fn with_source(mut self, def: SourceDef) -> Self {
        self.registry.sources.register(def);
        self
    }

    /// Register an additional sink.
    pub fn with_sink(mut self, def: SinkDef) -> Self {
        self.registry.sinks.register(def);
        self
    }

    /// Build the `FossilDb`. The registry is immutable from this point on.
    pub fn build(self) -> FossilDb {
        FossilDb {
            storage: Default::default(),
            registry: self.registry,
        }
    }
}

// ── Symbol: Salsa-interned string identifier ──────────────────────
//
// Same pattern as DefId: InternedSymbol<'db> is the Salsa type,
// Symbol(salsa::Id) is the lifetime-free wrapper stored in AST/IR.

#[salsa::interned]
pub struct InternedSymbol<'db> {
    #[returns(ref)]
    pub text: String,
}

/// Lifetime-free interned string. Wraps `salsa::Id` so it can be stored
/// in AST/IR arenas without propagating `'db`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(salsa::Id);

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Symbol({:?})", self.0)
    }
}

impl Symbol {
    /// Intern a string, returning a stable Symbol.
    pub fn new(db: &dyn Db, text: &str) -> Self {
        let interned = InternedSymbol::new(db, text.to_string());
        Self(salsa::plumbing::AsId::as_id(&interned))
    }

    /// Resolve this symbol to its string representation.
    pub fn text(self, db: &dyn Db) -> &str {
        let interned = <InternedSymbol<'_> as salsa::plumbing::FromId>::from_id(self.0);
        interned.text(db)
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
    /// Catalog entries (sources/sinks) registered by the host. Live in MetaNS,
    /// resolved by `MetaCall` syntax (`csv!(...)`).
    Catalog { origin: CatalogOrigin },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CatalogOrigin {
    Source,
    Sink,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_intern_and_resolve() {
        let db = FossilDb::default();
        let sym = Symbol::new(&db, "hello");
        assert_eq!(sym.text(&db), "hello");
    }

    #[test]
    fn symbol_dedup() {
        let db = FossilDb::default();
        let a = Symbol::new(&db, "foo");
        let b = Symbol::new(&db, "foo");
        assert_eq!(a, b);
    }
}

