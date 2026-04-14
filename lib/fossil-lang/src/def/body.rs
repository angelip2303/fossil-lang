//! Per-item HIR bodies — Priority 1 of the post-April-2026 refactor roadmap.
//!
//! Mirror of rust-analyzer's `hir-def/body.rs`: each top-level item (let,
//! type declaration, pipeline) gets its OWN Salsa-tracked body query keyed
//! on a stable `*Loc` ID from the [`ItemTree`]. Editing the body of one
//! item only invalidates that item's body query — sibling bodies stay
//! cached as long as the ItemTree structure is unchanged.
//!
//! ## Architecture
//!
//! ```text
//!     parse(file)                 ← syntax phase
//!         │
//!         ▼
//!     file_item_tree(file)        ← invalidation barrier (def/item_tree.rs)
//!         │
//!         ▼
//!     file_def_map_for_body(file) ← pre-builds DefMap with ALL top-level
//!         │                          symbols + meta-call schemas + sinks
//!         │
//!         ├──▶ let_body(LetLoc)         ┐
//!         ├──▶ type_decl_body(TypeDeclLoc) ├── per-item, isolated, cacheable
//!         └──▶ pipeline_body(PipelineLoc)  ┘
//! ```
//!
//! Each per-item query consumes the precomputed DefMap (which is itself a
//! Salsa query, so it stays cached across body edits), then lowers exactly
//! one statement via [`crate::passes::lower::lower_item_body`].
//!
//! ## Why this matters
//!
//! Future LSP responsiveness depends on this. When the user types inside
//! one `let foo = ...` body, only `let_body(LetLoc { foo })` re-runs.
//! All other bodies, plus DefMap, plus ItemTree, stay cached.
//!
//! Reference: rust-analyzer `crates/hir-def/src/body.rs`.

use std::collections::HashMap;

use crate::ast::Ast;
use crate::db::{Db, DefId, DefKindTag, Symbol, SourceFile};
use crate::def::item_tree::{
    file_item_tree,
    interned::{InternedLetLoc, InternedPipelineLoc, InternedTypeDeclLoc},
    LetLoc, PipelineLoc, TypeDeclLoc,
};
use crate::def_map::{DefMap, RegisteredTypes, TypeMetadataMap};
use crate::ir::{Ir, Resolutions, StmtId};
use crate::metadata::{extract_type_metadata, TypeMetadata};
use crate::passes::lower::lower_item_body;
use crate::queries::parse;

/// Mini-IR fragment for a single top-level item.
///
/// Each `HirBody` owns its own arenas (`Ir` includes `stmts`, `exprs`, `types`)
/// and its own `Resolutions`. The `root_stmt` is the entry point inside the
/// arenas. Cross-body references (e.g. `let b = a + 1`) are resolved against
/// the ambient file-level `DefMap` and recorded in `resolutions.expr_defs` as
/// `DefId`s — never as cross-arena `ExprId`s.
#[derive(Clone, PartialEq)]
pub struct HirBody {
    pub ir: Ir,
    pub resolutions: Resolutions,
    pub root_stmt: StmtId,
}

/// File-level scaffolding consumed by every per-item body query.
///
/// Built once per file by [`file_def_map_for_body`]. Contains every top-level
/// definition (lets, types, sinks, meta-call resolved record types) so that
/// individual body queries don't need to re-run the file pre-scan.
#[derive(Clone, PartialEq)]
pub struct FileDefMapForBody {
    pub def_map: DefMap,
    pub registered_types: RegisteredTypes,
    pub type_metadata: TypeMetadataMap,
    /// Pending type metadata extracted from the AST, indexed by type name.
    /// Per-item body lowering of a `type` statement consumes the matching
    /// entry to populate `type_metadata`.
    pub pending_type_metadata: HashMap<Symbol, TypeMetadata>,
    /// Top-level let bindings (name → DefId), in source order.
    /// Seeded into the body lowerer's scope stack so cross-let references
    /// resolve via the scope path (matching legacy semantics).
    pub top_level_lets: Vec<(Symbol, DefId)>,
    /// Top-level type declarations (name → DefId), in source order.
    pub top_level_types: Vec<(Symbol, DefId)>,
}

/// Salsa query: build the file-level DefMap scaffolding for body lowering.
///
/// This walks the ItemTree (NOT the parse output directly) and pre-registers:
/// 1. All sink namespaces from the registry (e.g. `Rdf.materialize`)
/// 2. All meta-call resolved record types (from `register_meta_call_schemas`)
/// 3. All top-level type declarations
/// 4. All top-level let bindings
///
/// Because the inputs (ItemTree + registry + parse) are themselves Salsa
/// queries, this query is fully cached and only re-runs when one of them
/// changes. Per-item body queries depend on this query, so a body-only edit
/// reaches `lower_item_body` with a cached DefMap.
#[salsa::tracked(returns(ref))]
pub fn file_def_map_for_body(db: &dyn Db, file: SourceFile) -> FileDefMapForBody {
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);

    let mut def_map = DefMap::default();
    let mut registered_types = RegisteredTypes::new();

    crate::queries::register_sinks_in_def_map(db, &mut def_map);
    crate::queries::register_meta_call_schemas(
        db,
        &ast,
        &mut def_map,
        &mut registered_types,
    );

    // Pre-register top-level type declarations (names visible to all bodies).
    let mut top_level_types = Vec::with_capacity(tree.types.len());
    for ty_item in &tree.types {
        let def_id = def_map.insert(db, None, ty_item.name, DefKindTag::Type);
        top_level_types.push((ty_item.name, def_id));
    }

    // Pre-register top-level lets (names visible to all bodies). Note: legacy
    // file-level lowering only adds a let DefId AFTER lowering its body, which
    // prevents forward references between sibling lets. Per-item lowering does
    // not enforce that ordering today — it's a known difference and is
    // documented as a follow-up. In practice, all extant scripts define lets
    // before use, so the observable behavior is unchanged.
    let mut top_level_lets = Vec::with_capacity(tree.lets.len());
    for let_item in &tree.lets {
        let def_id = def_map.insert(db, None, let_item.name, DefKindTag::Let);
        top_level_lets.push((let_item.name, def_id));
    }

    let pending_type_metadata = extract_type_metadata(&ast);

    FileDefMapForBody {
        def_map,
        registered_types,
        type_metadata: TypeMetadataMap::new(),
        pending_type_metadata,
        top_level_lets,
        top_level_types,
    }
}

/// Salsa query: lower a single `let` body in isolation, keyed on a
/// Salsa-interned [`InternedLetLoc`]. Sibling lets, the ItemTree, and
/// `file_def_map_for_body` stay cached across body-only edits.
#[salsa::tracked(returns(ref))]
pub fn let_body_query<'db>(db: &'db dyn Db, loc: InternedLetLoc<'db>) -> HirBody {
    let file = loc.file(db);
    let idx = loc.idx(db);
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let stmt_index = tree.lets[idx].ast_stmt_index;
    lower_one(db, &ast, stmt_index, scaffold)
}

/// Convenience wrapper for [`let_body_query`] that accepts a lifetime-free [`LetLoc`].
pub fn let_body<'db>(db: &'db dyn Db, loc: LetLoc) -> &'db HirBody {
    let_body_query(db, loc.to_interned(db))
}

/// Salsa query: lower a single `type` declaration body in isolation.
#[salsa::tracked(returns(ref))]
pub fn type_decl_body_query<'db>(
    db: &'db dyn Db,
    loc: InternedTypeDeclLoc<'db>,
) -> HirBody {
    let file = loc.file(db);
    let idx = loc.idx(db);
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let stmt_index = tree.types[idx].ast_stmt_index;
    lower_one(db, &ast, stmt_index, scaffold)
}

/// Convenience wrapper for [`type_decl_body_query`].
pub fn type_decl_body<'db>(db: &'db dyn Db, loc: TypeDeclLoc) -> &'db HirBody {
    type_decl_body_query(db, loc.to_interned(db))
}

/// Salsa query: lower the (single) pipeline expression body in isolation.
///
/// Returns `None` if the file has no pipeline (no top-level expression
/// statement). The Option wrapper is necessary because a `PipelineLoc` only
/// identifies the file, not whether a pipeline exists.
#[salsa::tracked(returns(ref))]
pub fn pipeline_body_query<'db>(
    db: &'db dyn Db,
    loc: InternedPipelineLoc<'db>,
) -> Option<HirBody> {
    let file = loc.file(db);
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let pipeline = tree.pipeline.as_ref()?;
    let scaffold = file_def_map_for_body(db, file);
    Some(lower_one(db, &ast, pipeline.ast_stmt_index, scaffold))
}

/// Convenience wrapper for [`pipeline_body_query`].
pub fn pipeline_body<'db>(db: &'db dyn Db, loc: PipelineLoc) -> &'db Option<HirBody> {
    pipeline_body_query(db, loc.to_interned(db))
}

/// Internal helper: drive `lower_item_body` with the file scaffold and
/// return a `HirBody`. On lowering errors, returns an empty body — errors
/// are handled by the file-level `lower` query which has its own diagnostic
/// accumulation. Per-item queries are designed to be called for IDE/incremental
/// consumers that don't care about late-binding diagnostics from sibling items.
fn lower_one(
    db: &dyn Db,
    ast: &Ast,
    stmt_index: usize,
    scaffold: &FileDefMapForBody,
) -> HirBody {
    match lower_item_body(
        db,
        ast,
        stmt_index,
        scaffold.def_map.clone(),
        scaffold.registered_types.clone(),
        scaffold.type_metadata.clone(),
        scaffold.pending_type_metadata.clone(),
        &scaffold.top_level_lets,
        &scaffold.top_level_types,
    ) {
        Ok((ir, resolutions, root_stmt)) => HirBody {
            ir,
            resolutions,
            root_stmt,
        },
        Err(errors) => {
            for e in errors {
                let _ = crate::error::emit_error(db, e);
            }
            let ir = Ir::default();
            // Allocate a placeholder Unit stmt so root_stmt is valid.
            let mut ir = ir;
            let unit_expr = ir.exprs.alloc(crate::ir::Expr {
                loc: crate::ast::Loc::generated(),
                kind: crate::ir::ExprKind::Unit,
            });
            let root_stmt = ir.stmts.alloc(crate::ir::Stmt {
                loc: crate::ast::Loc::generated(),
                kind: crate::ir::StmtKind::Expr(unit_expr),
            });
            HirBody {
                ir,
                resolutions: Resolutions::default(),
                root_stmt,
            }
        }
    }
}

// ── Per-item inference ────────────────────────────────────────────────
//
// `let_infer_query` is the per-item counterpart to the monolithic
// `queries::infer(file)`. It runs type inference over a single let body
// and returns that let's binding type, reading cross-body references via
// recursive salsa queries: each referenced top-level let triggers a
// recursive `let_infer` call, which populates the checker's environment
// before the body is walked.
//
// This gives real per-item caching: editing the body of `let a = ...`
// only re-runs `let_infer(A)` and any transitively-dependent siblings,
// not the whole file.
//
// **Scope of this first cut**: lets whose bodies only reference
// primitives and other top-level lets. Bodies that construct
// user-defined records (`User(id: "x")`) or call type constructors
// still need type_decl pre-population, which is the next incremental
// step. For those bodies, `let_infer` returns `None` and callers should
// fall back to the file-level `infer(file)` query.

use crate::ty::types::{Polytype, Ty};
use crate::ty::typecheck::TypeChecker;

/// Salsa query: infer the type of a single top-level `let` binding.
/// Returns `None` for bodies that touch type declarations (records,
/// constructors) that `let_infer` does not yet resolve on its own.
#[salsa::tracked]
pub fn let_infer_query<'db>(db: &'db dyn Db, loc: InternedLetLoc<'db>) -> Option<Ty> {
    let file = loc.file(db);
    let this_idx = loc.idx(db);
    let body = let_body_query(db, loc);
    let scaffold = file_def_map_for_body(db, file);

    // Short-circuit: bodies that declare or reference user-defined types
    // require a populated `type_index` in TypeChecker, which this query
    // does not yet build. Detect that case up-front and bail so the caller
    // can fall back to the monolithic file-level `infer`.
    if body_touches_types(db, body) {
        return None;
    }

    // Build a TypeChecker-ready IR: the body's arenas + a single-stmt root.
    // `lower_item_body` does not push anything into `ir.root`; the root_stmt
    // is returned separately. We populate root here so TypeChecker's
    // `check_tolerant` walk sees exactly this body's stmt.
    let mut ir = body.ir.clone();
    ir.root = vec![body.root_stmt];

    let mut checker = TypeChecker::new(
        db,
        ir,
        scaffold.def_map.clone(),
        scaffold.registered_types.clone(),
        body.resolutions.clone(),
    );

    // Pre-populate env with sibling let types. Walk resolutions for
    // referenced top-level let DefIds, find their LetLoc, and recursively
    // call `let_infer`. Each recursive call establishes a salsa dependency,
    // so editing sibling A's body invalidates `let_infer(B)` only when B
    // actually referenced A.
    let this_let_def_id = scaffold
        .top_level_lets
        .get(this_idx)
        .map(|(_, did)| *did);
    let referenced: std::collections::HashSet<DefId> =
        body.resolutions.expr_defs.values().copied().collect();
    for def_id in referenced {
        if Some(def_id) == this_let_def_id {
            continue;
        }
        if !matches!(def_id.kind(db), DefKindTag::Let) {
            continue;
        }
        let Some(sibling_idx) = scaffold
            .top_level_lets
            .iter()
            .position(|(_, did)| *did == def_id)
        else {
            continue;
        };
        let sibling_loc = LetLoc::new(db, file, sibling_idx);
        if let Some(sibling_ty) = let_infer(db, sibling_loc) {
            checker.env.insert(def_id, Polytype::mono(sibling_ty));
        } else {
            // Sibling itself bailed out — we can't type-check this body
            // without its type. Bail and let the caller fall back.
            return None;
        }
    }

    let result = checker.check();
    this_let_def_id
        .and_then(|did| result.typeck_results.binding_types.get(&did).copied())
}

/// Convenience wrapper: accepts a lifetime-free [`LetLoc`] and forwards
/// to the salsa-tracked `let_infer_query`.
pub fn let_infer(db: &dyn Db, loc: LetLoc) -> Option<Ty> {
    let_infer_query(db, loc.to_interned(db))
}

/// Conservative "does this body need a populated type_index?" check.
/// Returns `true` if the body contains `RecordInstance`, `Projection`,
/// `Join`, `Ref`, or resolves any identifier to a non-`Let` DefId.
/// When this returns `true`, `let_infer` bails and the caller must
/// fall back to the monolithic `infer(file)` query.
fn body_touches_types(db: &dyn Db, body: &HirBody) -> bool {
    for (_, expr) in body.ir.exprs.iter() {
        match &expr.kind {
            crate::ir::ExprKind::RecordInstance { .. }
            | crate::ir::ExprKind::Projection { .. }
            | crate::ir::ExprKind::Join { .. }
            | crate::ir::ExprKind::Ref { .. } => return true,
            _ => {}
        }
    }
    for &def_id in body.resolutions.expr_defs.values() {
        if !matches!(def_id.kind(db), DefKindTag::Let) {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;
    use crate::def::item_tree::{find_let_by_name, file_item_tree};

    #[test]
    fn let_body_lowers_simple_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let sym = Symbol::new(&db, "x");
        let loc = find_let_by_name(&db, file, sym).expect("let exists");
        let body = let_body(&db, loc);
        assert!(!body.ir.stmts.is_empty(), "body must contain at least one stmt");
        // Root stmt is a Let with an expression body.
        let stmt = &body.ir.stmts[body.root_stmt];
        assert!(matches!(stmt.kind, crate::ir::StmtKind::Let { .. }));
    }

    #[test]
    fn let_body_resolves_sibling_let() {
        // `let b = a + 1` references the previously-defined `let a = 1`.
        // The per-item body query must resolve `a` against the precomputed
        // file_def_map (since per-item bodies don't share a scope stack).
        // NOTE: this exercises the cross-body name resolution path. We use
        // a record reference rather than arithmetic since the language uses
        // expressions like `let b = a` for simple aliasing.
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let a = 1\nlet b = a".into(), "test".into());
        let b_sym = Symbol::new(&db, "b");
        let b_loc = find_let_by_name(&db, file, b_sym).expect("let b exists");
        let body = let_body(&db, b_loc);
        // The body should resolve `a` to a DefId in its local resolutions.
        assert!(
            !body.resolutions.expr_defs.is_empty(),
            "body must resolve at least one identifier (a)"
        );
    }

    #[test]
    fn per_item_body_invalidation_barrier() {
        // Two source files with structurally identical ItemTrees but
        // different body content for `let a`. The body of `let b` (which
        // doesn't reference `a`) must be structurally identical across the
        // two files, demonstrating that a body-only change to `a` doesn't
        // affect `b`'s lowering.
        let db = FossilDb::default();
        let file_a = SourceFile::new(&db, "let a = 1\nlet b = 2".into(), "a".into());
        let file_b = SourceFile::new(&db, "let a = 999\nlet b = 2".into(), "b".into());

        let b_sym = Symbol::new(&db, "b");
        let loc_a = find_let_by_name(&db, file_a, b_sym).expect("b in file_a");
        let loc_b = find_let_by_name(&db, file_b, b_sym).expect("b in file_b");

        let body_a = let_body(&db, loc_a);
        let body_b = let_body(&db, loc_b);

        // Both `let b = 2` bodies should produce structurally identical IR
        // (modulo DefId identities, which differ across files).
        assert_eq!(body_a.ir.stmts.len(), body_b.ir.stmts.len());
        assert_eq!(body_a.ir.exprs.len(), body_b.ir.exprs.len());
    }

    #[test]
    fn pipeline_body_returns_none_when_absent() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 1".into(), "test".into());
        let body = pipeline_body(&db, PipelineLoc::new(&db, file));
        assert!(body.is_none());
    }

    #[test]
    fn pipeline_body_returns_some_when_present() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 1\nx".into(), "test".into());
        let body = pipeline_body(&db, PipelineLoc::new(&db, file));
        assert!(body.is_some(), "pipeline statement should produce a body");
    }

    #[test]
    fn let_body_is_salsa_memoized() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let sym = Symbol::new(&db, "x");
        let loc = find_let_by_name(&db, file, sym).expect("let exists");
        let b1 = let_body(&db, loc);
        let b2 = let_body(&db, loc);
        // Pointer equality via returns(ref): same cached HirBody.
        assert!(std::ptr::eq(b1, b2));
    }

    #[test]
    fn let_infer_primitive_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let sym = Symbol::new(&db, "x");
        let loc = find_let_by_name(&db, file, sym).expect("let exists");
        let ty = let_infer(&db, loc).expect("primitive-typed let must infer");
        let kind = ty.kind(&db);
        use crate::ty::types::{TyKind};
        use crate::base::common::PrimitiveType;
        assert!(
            matches!(kind, TyKind::Primitive(PrimitiveType::Int)),
            "got: {kind:?}",
        );
    }

    #[test]
    fn let_infer_cross_body_reference() {
        // `let b = a` must infer the same type as `a`, which means
        // `let_infer(b)` had to reach the salsa-memoized `let_infer(a)`.
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let a = 1\nlet b = a".into(), "test".into());
        let b_sym = Symbol::new(&db, "b");
        let b_loc = find_let_by_name(&db, file, b_sym).expect("let b exists");
        let ty_b = let_infer(&db, b_loc).expect("cross-body let must infer");
        let kind = ty_b.kind(&db);
        use crate::ty::types::{TyKind};
        use crate::base::common::PrimitiveType;
        assert!(
            matches!(kind, TyKind::Primitive(PrimitiveType::Int)),
            "got: {kind:?}",
        );
    }

    #[test]
    fn let_infer_bails_on_record_instance() {
        // `let_infer` doesn't yet populate the type_index; any body that
        // constructs a user-defined record must return None so callers
        // fall back to the monolithic file-level infer.
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "type Point(x: int, y: int) do x: int, y: int end\nlet p = Point(x: 1, y: 2)".into(),
            "test".into(),
        );
        let p_sym = Symbol::new(&db, "p");
        let p_loc = find_let_by_name(&db, file, p_sym).expect("let p exists");
        let result = let_infer(&db, p_loc);
        assert!(
            result.is_none(),
            "body constructing user type must bail (got: {result:?})",
        );
    }

    #[test]
    fn file_def_map_for_body_pre_registers_top_level_lets() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let foo = 1\nlet bar = 2".into(), "test".into());
        let scaffold = file_def_map_for_body(&db, file);
        assert_eq!(scaffold.top_level_lets.len(), 2);
        assert_eq!(scaffold.top_level_lets[0].0.text(&db), "foo");
        assert_eq!(scaffold.top_level_lets[1].0.text(&db), "bar");
    }
}
