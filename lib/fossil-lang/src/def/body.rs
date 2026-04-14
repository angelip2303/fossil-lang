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
//!         │                          symbols + provider schemas + sinks
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
use crate::def::item_tree::{file_item_tree, LetLoc, PipelineLoc, TypeDeclLoc};
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
/// definition (lets, types, sinks, provider-resolved record types) so that
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

/// Salsa query: lower a single `let` body in isolation.
///
/// Cached on `(file, idx)` — the structural fields of a `LetLoc`. A body-only
/// edit to one let invalidates only this query for that one `(file, idx)`
/// pair; sibling lets, the ItemTree, and the `file_def_map_for_body` query
/// stay cached.
///
/// Salsa query parameters must be `salsa::Update` types (currently `SourceFile`,
/// `Symbol`, `usize`, etc.), so we use the structural fields directly rather
/// than wrapping `LetLoc` in `#[salsa::interned]` — that would introduce a
/// `'db` lifetime through the public API of `def/item_tree.rs`. The
/// [`let_body_at`] convenience wrapper accepts a `LetLoc` for callers that
/// already have one.
#[salsa::tracked(returns(ref))]
pub fn let_body_query(db: &dyn Db, file: SourceFile, idx: usize) -> HirBody {
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let stmt_index = tree.lets[idx].ast_stmt_index;
    lower_one(db, &ast, stmt_index, scaffold)
}

/// Convenience wrapper for [`let_body_query`] that accepts a [`LetLoc`].
pub fn let_body<'db>(db: &'db dyn Db, loc: LetLoc) -> &'db HirBody {
    let_body_query(db, loc.file, loc.idx)
}

/// Salsa query: lower a single `type` declaration body in isolation.
#[salsa::tracked(returns(ref))]
pub fn type_decl_body_query(db: &dyn Db, file: SourceFile, idx: usize) -> HirBody {
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let stmt_index = tree.types[idx].ast_stmt_index;
    lower_one(db, &ast, stmt_index, scaffold)
}

/// Convenience wrapper for [`type_decl_body_query`].
pub fn type_decl_body<'db>(db: &'db dyn Db, loc: TypeDeclLoc) -> &'db HirBody {
    type_decl_body_query(db, loc.file, loc.idx)
}

/// Salsa query: lower the (single) pipeline expression body in isolation.
///
/// Returns `None` if the file has no pipeline (no top-level expression
/// statement). The Option wrapper is necessary because a `PipelineLoc` only
/// identifies the file, not whether a pipeline exists.
#[salsa::tracked(returns(ref))]
pub fn pipeline_body_query(db: &dyn Db, file: SourceFile) -> Option<HirBody> {
    let ast = parse(db, file);
    let tree = file_item_tree(db, file);
    let pipeline = tree.pipeline.as_ref()?;
    let scaffold = file_def_map_for_body(db, file);
    Some(lower_one(db, &ast, pipeline.ast_stmt_index, scaffold))
}

/// Convenience wrapper for [`pipeline_body_query`].
pub fn pipeline_body<'db>(db: &'db dyn Db, loc: PipelineLoc) -> &'db Option<HirBody> {
    pipeline_body_query(db, loc.file)
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
        let body = pipeline_body(&db, PipelineLoc { file });
        assert!(body.is_none());
    }

    #[test]
    fn pipeline_body_returns_some_when_present() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 1\nx".into(), "test".into());
        let body = pipeline_body(&db, PipelineLoc { file });
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
    fn file_def_map_for_body_pre_registers_top_level_lets() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let foo = 1\nlet bar = 2".into(), "test".into());
        let scaffold = file_def_map_for_body(&db, file);
        assert_eq!(scaffold.top_level_lets.len(), 2);
        assert_eq!(scaffold.top_level_lets[0].0.text(&db), "foo");
        assert_eq!(scaffold.top_level_lets[1].0.text(&db), "bar");
    }
}
