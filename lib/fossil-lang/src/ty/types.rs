//! Interned type system — rustc `Ty<'tcx>` pattern adapted to Salsa.
//!
//! Replaces the per-IR `la_arena::Idx<Type>` representation used during type
//! checking with a globally-interned `Ty` value. Two types with identical
//! structure share the same `salsa::Id`, giving canonical-pointer (O(1))
//! equality — the same property rustc relies on when comparing types billions
//! of times.
//!
//! ## Lifetime-free wrapper
//!
//! The codebase already uses the "lifetime-free wrapper around `salsa::Id`"
//! pattern for `Symbol` and `DefId` (see `base/db.rs`). `Ty` follows the same
//! pattern: it wraps a `salsa::Id` produced by the underlying interned struct
//! `InternedTy<'db>`, but exposes no `'db` itself. This intercepts the `'db`
//! lifetime exactly at the Salsa boundary and prevents it from leaking into
//! `TypeChecker`, `InferResult`, and other consumers.
//!
//! Functionally equivalent to rustc's `Ty<'tcx>` (canonical interning, O(1)
//! comparison) without forcing every caller to carry the database lifetime.

use crate::base::common::PrimitiveType;
use crate::db::{Db, DefId, Symbol};
use crate::error::ErrorGuaranteed;

// ── TypeVar (unification variables) ────────────────────────────────

/// Hindley–Milner unification variable. Each `fresh()` produces a unique id;
/// interning a `TyKind::Var(v)` deduplicates by that id.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

// ── RecordFields ───────────────────────────────────────────────────

/// Ordered field list for record types. Newtype around `Vec<(Symbol, Ty)>`
/// so it can be stored as the canonical key inside `TyKind::Record`.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct RecordFields {
    pub fields: Vec<(Symbol, Ty)>,
}

impl RecordFields {
    pub fn from_fields(fields: Vec<(Symbol, Ty)>) -> Self {
        Self { fields }
    }

    pub fn lookup(&self, field: Symbol) -> Option<Ty> {
        self.fields
            .iter()
            .find(|(f, _)| *f == field)
            .map(|(_, ty)| *ty)
    }

    pub fn field_names(&self) -> Vec<Symbol> {
        self.fields.iter().map(|(f, _)| *f).collect()
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

// ── TyKind ─────────────────────────────────────────────────────────

/// Structural shape of a type. Stored as the canonical key of an interned
/// `Ty`. All field references are lifetime-free (`Ty`, `Symbol`, `DefId`)
/// so `TyKind` itself carries no `'db`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyKind {
    /// `Int`, `Float`, `String`, `Bool` — language primitives.
    Primitive(PrimitiveType),
    /// `()` unit type.
    Unit,
    /// Inference / generalization variable.
    Var(TypeVar),
    /// Reference to a user-defined or registered type by `DefId`.
    Named(DefId),
    /// `T?` — optional / nullable wrapper.
    Optional(Ty),
    /// Anonymous record `{f1: T1, f2: T2, ...}`.
    Record(RecordFields),
    /// Function signature `(P1, P2, ...) -> R`.
    Function(Vec<Ty>, Ty),
    /// Type-error placeholder, tagged with proof that a diagnostic was
    /// already emitted. Unifies with anything without producing further
    /// errors (rustc `TyKind::Error(ErrorGuaranteed)` pattern).
    Error(ErrorGuaranteed),
}

// ── InternedTy + lifetime-free wrapper ────────────────────────────

/// Salsa-interned type representation. `InternedTy<'db>` is the actual
/// Salsa type (deduplicated on `kind`); `Ty` below is the lifetime-free
/// wrapper that every consumer should use.
#[salsa::interned]
pub struct InternedTy<'db> {
    #[returns(ref)]
    pub kind: TyKind,
}

/// Lifetime-free interned type identifier. Wraps a `salsa::Id` — two `Ty`
/// values are equal iff their structure is equal (canonical interning).
///
/// Equivalent to rustc's `Ty<'tcx>` for our purposes: O(1) equality, globally
/// unique, but with no `'db` propagation thanks to the wrapper pattern
/// established by `Symbol` and `DefId` in `base/db.rs`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::Id);

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ty({:?})", self.0)
    }
}

impl Ty {
    /// Intern a type. Two calls with structurally equal `kind`s return the
    /// same `Ty` (canonical-id equality).
    pub fn new(db: &dyn Db, kind: TyKind) -> Self {
        let interned = InternedTy::new(db, kind);
        Self(salsa::plumbing::AsId::as_id(&interned))
    }

    fn lookup<'db>(self, _db: &'db dyn Db) -> InternedTy<'db> {
        <InternedTy<'db> as salsa::plumbing::FromId>::from_id(self.0)
    }

    /// Resolve this `Ty` back to its structural `TyKind`. Cheap: a Salsa
    /// table lookup. Returns a clone because `TyKind::Record`/`Function`
    /// own heap data; if you only need the discriminant prefer the helper
    /// predicates below.
    pub fn kind(self, db: &dyn Db) -> TyKind {
        self.lookup(db).kind(db).clone()
    }

    // ── primitive constructors ─────────────────────────────────────

    pub fn mk_int(db: &dyn Db) -> Self {
        Self::new(db, TyKind::Primitive(PrimitiveType::Int))
    }

    pub fn mk_float(db: &dyn Db) -> Self {
        Self::new(db, TyKind::Primitive(PrimitiveType::Float))
    }

    pub fn mk_string(db: &dyn Db) -> Self {
        Self::new(db, TyKind::Primitive(PrimitiveType::String))
    }

    pub fn mk_bool(db: &dyn Db) -> Self {
        Self::new(db, TyKind::Primitive(PrimitiveType::Bool))
    }

    pub fn mk_unit(db: &dyn Db) -> Self {
        Self::new(db, TyKind::Unit)
    }

    /// Build the error type. Takes an [`ErrorGuaranteed`] by value to enforce
    /// the invariant: you can only construct `TyKind::Error` if you have
    /// already emitted a diagnostic via [`crate::error::emit_error`].
    pub fn mk_error(db: &dyn Db, guarantee: ErrorGuaranteed) -> Self {
        Self::new(db, TyKind::Error(guarantee))
    }

    pub fn mk_optional(db: &dyn Db, inner: Ty) -> Self {
        Self::new(db, TyKind::Optional(inner))
    }

    pub fn mk_named(db: &dyn Db, def_id: DefId) -> Self {
        Self::new(db, TyKind::Named(def_id))
    }

    pub fn mk_var(db: &dyn Db, var: TypeVar) -> Self {
        Self::new(db, TyKind::Var(var))
    }

    pub fn mk_record(db: &dyn Db, fields: Vec<(Symbol, Ty)>) -> Self {
        Self::new(db, TyKind::Record(RecordFields::from_fields(fields)))
    }

    pub fn mk_function(db: &dyn Db, params: Vec<Ty>, ret: Ty) -> Self {
        Self::new(db, TyKind::Function(params, ret))
    }
}

// ── Polytype (HM type schemes) ────────────────────────────────────

/// `forall <vars>. ty` — Hindley–Milner type scheme. The body `ty` is a
/// lifetime-free `Ty`, so `Polytype` is also lifetime-free and can live in
/// `TypeEnv` / `InferResult` without propagating `'db`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Polytype {
    pub forall: Vec<TypeVar>,
    pub ty: Ty,
}

impl Polytype {
    pub fn mono(ty: Ty) -> Self {
        Polytype { forall: vec![], ty }
    }

    pub fn poly(forall: Vec<TypeVar>, ty: Ty) -> Self {
        Polytype { forall, ty }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;

    #[test]
    fn primitives_are_canonical() {
        let db = FossilDb::default();
        let a = Ty::mk_int(&db);
        let b = Ty::mk_int(&db);
        assert_eq!(a, b, "two mk_int calls must intern to the same Ty");
        assert_ne!(a, Ty::mk_string(&db));
    }

    #[test]
    fn optional_is_canonical() {
        let db = FossilDb::default();
        let int = Ty::mk_int(&db);
        let opt1 = Ty::mk_optional(&db, int);
        let opt2 = Ty::mk_optional(&db, Ty::mk_int(&db));
        assert_eq!(opt1, opt2);
    }

    #[test]
    fn function_is_canonical() {
        let db = FossilDb::default();
        let int = Ty::mk_int(&db);
        let str_ = Ty::mk_string(&db);
        let f1 = Ty::mk_function(&db, vec![int, str_], int);
        let f2 = Ty::mk_function(&db, vec![Ty::mk_int(&db), Ty::mk_string(&db)], Ty::mk_int(&db));
        assert_eq!(f1, f2);
    }

    #[test]
    fn record_canonical_by_field_order() {
        let db = FossilDb::default();
        let int = Ty::mk_int(&db);
        let str_ = Ty::mk_string(&db);
        let a = Symbol::new(&db, "a");
        let b = Symbol::new(&db, "b");
        let r1 = Ty::mk_record(&db, vec![(a, int), (b, str_)]);
        let r2 = Ty::mk_record(&db, vec![(a, int), (b, str_)]);
        assert_eq!(r1, r2);
        // Field reordering produces a different canonical Ty (records are
        // ordered for now — matches existing IR behaviour).
        let r3 = Ty::mk_record(&db, vec![(b, str_), (a, int)]);
        assert_ne!(r1, r3);
    }

    #[test]
    fn kind_roundtrips() {
        let db = FossilDb::default();
        let int = Ty::mk_int(&db);
        let opt = Ty::mk_optional(&db, int);
        match opt.kind(&db) {
            TyKind::Optional(inner) => assert_eq!(inner, int),
            other => panic!("expected Optional, got {:?}", other),
        }
    }
}
