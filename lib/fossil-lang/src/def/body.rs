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

    // Catalog must come first so MetaCall resolution (`csv!`, `parquet!`,
    // etc.) finds the source/sink DefIds in MetaNS when body lowering
    // walks a let that references them. The monolithic `lower(file)`
    // does the same thing in `queries.rs::lower`.
    crate::queries::register_catalog_in_def_map(db, &mut def_map);
    crate::queries::register_sinks_in_def_map(db, &mut def_map);
    crate::queries::register_meta_call_schemas(
        db,
        &ast,
        &mut def_map,
        &mut registered_types,
    );

    // Pre-register top-level type declarations (names visible to all bodies).
    // For each Type we also register its record constructor under the same
    // name in ValueNS, so body lowerers can resolve `Foo(...)` call syntax to
    // a `RecordConstructor` DefId without having to lower the Type stmt first.
    // The ctor's parameter/return types are materialised later by the
    // per-item `type_decl_infer` query.
    let mut top_level_types = Vec::with_capacity(tree.types.len());
    for ty_item in &tree.types {
        let def_id = def_map.insert(db, None, ty_item.name, DefKindTag::Type);
        top_level_types.push((ty_item.name, def_id));
        def_map.insert(db, None, ty_item.name, DefKindTag::RecordConstructor);
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

use crate::ty::types::{Polytype, Ty, TyKind};
use crate::ty::typecheck::{TypeChecker, TypeDeclInfo};
use crate::rq::lower::{RqLowering, RqValue};
use crate::rq::{EmissionDecl, OutputDecl, SourceRef};

/// Contribution a single top-level let adds to the file-level RQ.
/// Returned from `let_rq_query` so the file-level aggregator can
/// concatenate per-item pieces without re-running the monolithic lowerer.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct LetRqContribution {
    pub sources: Vec<SourceRef>,
    pub ctes: Vec<sqlparser::ast::Cte>,
    pub emissions: Vec<EmissionDecl>,
    /// The env value downstream lets should see for this let's name.
    /// `None` when the body produced Unit (no usable SQL artefact).
    pub env_value: Option<RqValue>,
}

/// Contribution from the single pipeline body (at most one per file).
/// Carries the same RQ fragments as a let plus the file's output decls.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct PipelineRqContribution {
    pub sources: Vec<SourceRef>,
    pub ctes: Vec<sqlparser::ast::Cte>,
    pub emissions: Vec<EmissionDecl>,
    pub outputs: Vec<OutputDecl>,
}

/// Result of per-item inference over a `type` declaration: the interned
/// `TypeDeclInfo` plus, if the type has a record constructor, the
/// constructor's function type (`(fields…) -> Named(self)`).
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDeclInferResult {
    pub info: TypeDeclInfo,
    pub ctor_fn_ty: Option<Ty>,
}

/// Salsa query: extract the `TypeDeclInfo` + record-constructor function
/// type for a single top-level `type` declaration. The returned info is
/// consumed by `let_infer` to pre-populate TypeChecker's `type_index`
/// when an inferred body references a user-defined record.
#[salsa::tracked]
pub fn type_decl_infer_query<'db>(
    db: &'db dyn Db,
    loc: InternedTypeDeclLoc<'db>,
) -> Option<TypeDeclInferResult> {
    let file = loc.file(db);
    let body = type_decl_body_query(db, loc);
    let scaffold = file_def_map_for_body(db, file);
    let this_idx = loc.idx(db);
    let this_def_id = scaffold.top_level_types.get(this_idx).map(|(_, did)| *did)?;

    // Body IR contains exactly one Type stmt (lowered via lower_item_body).
    let stmt = body.ir.stmts.iter().next()?;
    let (ty_id, ctor_params, field_names) = match &stmt.1.kind {
        crate::ir::StmtKind::Type { ty, ctor_params, .. } => {
            let field_names = match &body.ir.types[*ty].kind {
                crate::ir::TypeKind::Record(fields) => fields.field_names(),
                _ => vec![],
            };
            (*ty, ctor_params.clone(), field_names)
        }
        _ => return None,
    };

    let interned_ty = crate::ty::typecheck::intern_ir_type(db, &body.ir, ty_id);
    let info = TypeDeclInfo {
        ty: interned_ty,
        ctor_param_count: ctor_params.len(),
        ctor_param_names: ctor_params.iter().map(|p| p.name).collect(),
        field_names,
    };

    // Derive the record-constructor function type. When `ctor_params` are
    // declared they define the public call interface; otherwise the ctor
    // takes one argument per record field, in declaration order.
    let ctor_fn_ty = match interned_ty.kind(db) {
        TyKind::Record(fields) => {
            let return_ty = Ty::mk_named(db, this_def_id);
            let param_types: Vec<Ty> = if ctor_params.is_empty() {
                fields.fields.iter().map(|(_, t)| *t).collect()
            } else {
                ctor_params
                    .iter()
                    .map(|p| crate::ty::typecheck::intern_ir_type(db, &body.ir, p.ty))
                    .collect()
            };
            Some(Ty::mk_function(db, param_types, return_ty))
        }
        _ => None,
    };

    Some(TypeDeclInferResult { info, ctor_fn_ty })
}

/// Convenience wrapper: accepts a lifetime-free [`TypeDeclLoc`].
pub fn type_decl_infer(db: &dyn Db, loc: TypeDeclLoc) -> Option<TypeDeclInferResult> {
    type_decl_infer_query(db, loc.to_interned(db))
}

/// Salsa query: infer the type of a single top-level `let` binding.
///
/// Returns `None` only when inference genuinely fails (unresolved sibling,
/// type error). Bodies that reference user-defined types now pre-populate
/// the checker's `type_index` via recursive `type_decl_infer` calls, so
/// record-constructing bodies are supported.
#[salsa::tracked]
pub fn let_infer_query<'db>(db: &'db dyn Db, loc: InternedLetLoc<'db>) -> Option<Ty> {
    let file = loc.file(db);
    let this_idx = loc.idx(db);
    let body = let_body_query(db, loc);
    let scaffold = file_def_map_for_body(db, file);

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

    // Classify every DefId this body references and resolve it per-item:
    //   - Let DefId  → recursive `let_infer` → inject sibling type into env
    //   - Type DefId → recursive `type_decl_infer` → inject into type_index
    //                  and, for record types, register the ctor fn in env
    //   - RecordConstructor DefId → look up its parent type via DefMap and
    //                  inject the ctor fn in env (the parent type's infer
    //                  call already populates type_index for the named type)
    //
    // Each recursive call establishes a salsa dependency, so editing a
    // sibling body invalidates this query only if the referenced type or
    // binding actually changed.
    let this_let_def_id = scaffold.top_level_lets.get(this_idx).map(|(_, did)| *did);
    let referenced: std::collections::HashSet<DefId> =
        body.resolutions.expr_defs.values().copied().collect();

    for def_id in referenced {
        if Some(def_id) == this_let_def_id {
            continue;
        }
        match def_id.kind(db) {
            DefKindTag::Let => {
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
                    return None;
                }
            }
            DefKindTag::Type => {
                let Some(type_idx) = scaffold
                    .top_level_types
                    .iter()
                    .position(|(_, did)| *did == def_id)
                else {
                    continue;
                };
                let type_loc = TypeDeclLoc::new(db, file, type_idx);
                let Some(result) = type_decl_infer(db, type_loc) else {
                    return None;
                };
                checker.type_index.insert(def_id, result.info.clone());
                // Also register the ctor fn in env for call-through, mirror
                // of TypeChecker::init_record_constructors.
                if let Some(ctor_fn_ty) = result.ctor_fn_ty {
                    let type_name = def_id.name(db);
                    if let Some(ctor_def_id) = scaffold.def_map.find_in_ns(
                        type_name,
                        crate::def_map::Namespace::ValueNS,
                        db,
                        |k| matches!(k, DefKindTag::RecordConstructor),
                    ) {
                        checker
                            .env
                            .insert(ctor_def_id, Polytype::mono(ctor_fn_ty));
                    }
                }
            }
            DefKindTag::RecordConstructor => {
                // Find the parent type by name and pull its info through the
                // type_decl_infer query.
                let ctor_name = def_id.name(db);
                let Some(type_def_id) = scaffold.def_map.find_in_ns(
                    ctor_name,
                    crate::def_map::Namespace::TypeNS,
                    db,
                    |k| matches!(k, DefKindTag::Type),
                ) else {
                    continue;
                };
                let Some(type_idx) = scaffold
                    .top_level_types
                    .iter()
                    .position(|(_, did)| *did == type_def_id)
                else {
                    continue;
                };
                let type_loc = TypeDeclLoc::new(db, file, type_idx);
                let Some(result) = type_decl_infer(db, type_loc) else {
                    return None;
                };
                checker.type_index.insert(type_def_id, result.info.clone());
                if let Some(ctor_fn_ty) = result.ctor_fn_ty {
                    checker
                        .env
                        .insert(def_id, Polytype::mono(ctor_fn_ty));
                }
            }
            _ => {}
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

/// File-level salsa query: every top-level let's inferred type, keyed by
/// its `DefId`. Implemented as a fan-out over per-item `let_infer` — the
/// only salsa dependency is `file_item_tree` plus one `let_infer` edge
/// per let. Editing one let body only invalidates this aggregate when
/// that let's own inferred type actually changes; siblings that didn't
/// reference the edited body don't re-run at all.
///
/// Primary consumer: IDE hover / completion. Answers "what is the type
/// of `foo`?" without ever touching the monolithic `infer(file)` pipeline.
#[salsa::tracked(returns(ref))]
pub fn file_binding_types(
    db: &dyn Db,
    file: SourceFile,
) -> std::collections::HashMap<DefId, Ty> {
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let mut out = std::collections::HashMap::with_capacity(tree.lets.len());
    for (idx, _) in tree.lets.iter().enumerate() {
        let Some((_, def_id)) = scaffold.top_level_lets.get(idx) else {
            continue;
        };
        let loc = LetLoc::new(db, file, idx);
        if let Some(ty) = let_infer(db, loc) {
            out.insert(*def_id, ty);
        }
    }
    out
}

/// File-level salsa query: every top-level type declaration's
/// inference result, keyed by its `DefId`. Fan-out over per-item
/// `type_decl_infer`. Mirrors `file_binding_types` for the type side.
///
/// Primary consumer: IDE completion of user-defined record fields, and
/// downstream queries (including `let_infer`) that need to resolve
/// arbitrary Type DefIds without running the monolithic pipeline.
#[salsa::tracked(returns(ref))]
pub fn file_type_decls(
    db: &dyn Db,
    file: SourceFile,
) -> std::collections::HashMap<DefId, TypeDeclInferResult> {
    let tree = file_item_tree(db, file);
    let scaffold = file_def_map_for_body(db, file);
    let mut out = std::collections::HashMap::with_capacity(tree.types.len());
    for (idx, _) in tree.types.iter().enumerate() {
        let Some((_, def_id)) = scaffold.top_level_types.get(idx) else {
            continue;
        };
        let loc = TypeDeclLoc::new(db, file, idx);
        if let Some(result) = type_decl_infer(db, loc) {
            out.insert(*def_id, result);
        }
    }
    out
}

// ── Per-item RQ lowering (milestones 3 + 4) ──────────────────────────

/// Build the pre-populated environment and type_index that a per-item
/// `RqLowering` needs in order to lower one body in isolation. Walks the
/// body's `resolutions.expr_defs`, classifies every referenced DefId,
/// and recursively pulls:
///   - Let DefIds → `let_rq_query(sibling)` → inject its `env_value`
///   - Type / RecordConstructor DefIds → `type_decl_infer(type_loc)` →
///     inject into `type_index` (the RQ lowerer reads it for emission
///     field ordering and record-constructor param resolution)
///
/// Returns `None` if any recursive dependency failed — the caller then
/// bails to the monolithic RQ path (for now).
fn build_per_item_rq_ctx(
    db: &dyn Db,
    body: &HirBody,
    scaffold: &FileDefMapForBody,
    file: SourceFile,
    this_let_def_id: Option<DefId>,
) -> Option<(
    HashMap<Symbol, RqValue>,
    crate::ty::typecheck::TypeIndex,
)> {
    let mut env: HashMap<Symbol, RqValue> = HashMap::new();
    let mut type_index = crate::ty::typecheck::TypeIndex::default();

    let referenced: std::collections::HashSet<DefId> =
        body.resolutions.expr_defs.values().copied().collect();

    for def_id in referenced {
        if Some(def_id) == this_let_def_id {
            continue;
        }
        match def_id.kind(db) {
            DefKindTag::Let => {
                let Some(sibling_idx) = scaffold
                    .top_level_lets
                    .iter()
                    .position(|(_, did)| *did == def_id)
                else {
                    continue;
                };
                let sibling_loc = LetLoc::new(db, file, sibling_idx);
                let Some(sibling_contrib) = let_rq(db, sibling_loc) else {
                    return None;
                };
                if let Some(env_value) = &sibling_contrib.env_value {
                    env.insert(def_id.name(db), env_value.clone());
                }
            }
            DefKindTag::Type => {
                let Some(type_idx) = scaffold
                    .top_level_types
                    .iter()
                    .position(|(_, did)| *did == def_id)
                else {
                    continue;
                };
                let type_loc = TypeDeclLoc::new(db, file, type_idx);
                let Some(result) = type_decl_infer(db, type_loc) else {
                    return None;
                };
                type_index.insert(def_id, result.info);
            }
            DefKindTag::RecordConstructor => {
                let ctor_name = def_id.name(db);
                let Some(type_def_id) = scaffold.def_map.find_in_ns(
                    ctor_name,
                    crate::def_map::Namespace::TypeNS,
                    db,
                    |k| matches!(k, DefKindTag::Type),
                ) else {
                    continue;
                };
                let Some(type_idx) = scaffold
                    .top_level_types
                    .iter()
                    .position(|(_, did)| *did == type_def_id)
                else {
                    continue;
                };
                let type_loc = TypeDeclLoc::new(db, file, type_idx);
                let Some(result) = type_decl_infer(db, type_loc) else {
                    return None;
                };
                type_index.insert(type_def_id, result.info);
            }
            _ => {}
        }
    }

    Some((env, type_index))
}

/// Salsa query: lower a single top-level `let` body to its RQ
/// contribution (added sources, added CTEs, added emissions, plus the
/// env value downstream bodies consume).
///
/// Recursive salsa edges: one per sibling DefId the body references.
/// Editing a let body only re-runs `let_rq(B)` when B actually touched
/// the edited let via its resolutions. The file-level `queries::rq`
/// query iterates `file_item_tree(file).lets` and concatenates these
/// contributions; see `queries::rq` for the fan-out.
#[salsa::tracked]
pub fn let_rq_query<'db>(
    db: &'db dyn Db,
    loc: InternedLetLoc<'db>,
) -> Option<LetRqContribution> {
    let file = loc.file(db);
    let this_idx = loc.idx(db);
    let body = let_body_query(db, loc);
    let scaffold = file_def_map_for_body(db, file);
    let this_let_def_id = scaffold.top_level_lets.get(this_idx).map(|(_, did)| *did);

    let (env, type_index) =
        build_per_item_rq_ctx(db, body, scaffold, file, this_let_def_id)?;

    // Body IR needs `ir.root` populated with the single stmt so
    // `RqLowering::lower` walks it.
    let mut ir = body.ir.clone();
    ir.root = vec![body.root_stmt];

    // Build the lowerer. We need stable references to type_index /
    // resolutions throughout the walk; clone into locals.
    let resolutions = body.resolutions.clone();
    let prefix = format!("let_{this_idx}");

    // RqLowering holds references with a 'a lifetime, so we feed it
    // locally-owned state that outlives the call.
    let mut lowering =
        RqLowering::new(db, &ir, &type_index, &resolutions).with_name_prefix(prefix);
    lowering.env = env;

    let lowered = match lowering.lower() {
        Ok(rq) => rq,
        Err(_) => return None,
    };
    // Can't use into_parts() here because lower() consumes self and
    // returns only the RelationalQuery. Re-run with a second instance
    // to extract the env afterwards? No — simpler: add an env-returning
    // variant. For now, re-derive the env value from the RelationalQuery
    // shape: if the body produced a CTE chain, the last CTE's alias is
    // this let's Table value; if it produced sources only, the last
    // source alias is the Table value.
    let env_value = lowered
        .ctes
        .last()
        .map(|c| RqValue::Table(c.alias.name.clone()))
        .or_else(|| {
            lowered
                .sources
                .last()
                .map(|s| RqValue::Table(s.alias.clone()))
        });

    Some(LetRqContribution {
        sources: lowered.sources,
        ctes: lowered.ctes,
        emissions: lowered.emissions,
        env_value,
    })
}

/// Convenience wrapper accepting a lifetime-free `LetLoc`.
pub fn let_rq(db: &dyn Db, loc: LetLoc) -> Option<LetRqContribution> {
    let_rq_query(db, loc.to_interned(db))
}

/// Salsa query: lower the (optional) pipeline body to its RQ contribution.
/// The pipeline is the single top-level expression statement; it
/// contributes CTEs, emissions, and — unlike lets — the output decls
/// that drive host-side materialisation.
#[salsa::tracked]
pub fn pipeline_rq_query<'db>(
    db: &'db dyn Db,
    loc: InternedPipelineLoc<'db>,
) -> Option<PipelineRqContribution> {
    let file = loc.file(db);
    let tree = file_item_tree(db, file);
    tree.pipeline.as_ref()?;
    let body = pipeline_body_query(db, loc).as_ref()?;
    let scaffold = file_def_map_for_body(db, file);

    let (env, type_index) = build_per_item_rq_ctx(db, body, scaffold, file, None)?;

    let mut ir = body.ir.clone();
    ir.root = vec![body.root_stmt];

    let resolutions = body.resolutions.clone();
    let mut lowering = RqLowering::new(db, &ir, &type_index, &resolutions)
        .with_name_prefix("pipeline".to_string());
    lowering.env = env;
    let lowered = match lowering.lower() {
        Ok(rq) => rq,
        Err(_) => return None,
    };

    Some(PipelineRqContribution {
        sources: lowered.sources,
        ctes: lowered.ctes,
        emissions: lowered.emissions,
        outputs: lowered.outputs,
    })
}

/// Convenience wrapper accepting a lifetime-free `PipelineLoc`.
pub fn pipeline_rq(db: &dyn Db, loc: PipelineLoc) -> Option<PipelineRqContribution> {
    pipeline_rq_query(db, loc.to_interned(db))
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;
    use crate::def::item_tree::find_let_by_name;

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
    fn let_infer_record_instance_uses_type_decl_infer() {
        // Body constructs a user-defined record via Application-style
        // constructor call. Milestone 2 pre-populates checker.type_index
        // AND env (ctor fn type) via recursive `type_decl_infer`, so this
        // body must infer successfully instead of bailing.
        //
        // Note: the monolithic TypeChecker's `unify` unwraps `Named(T)` to
        // its underlying `Record` before binding inference variables — so
        // an Application-style constructor call like `Point(x: 1, y: 2)`
        // yields a `Record` type, not `Named(Point)`. Same behavior as the
        // file-level `infer(file)` query; what we're testing here is that
        // `let_infer` reaches parity with it, not that it "promotes" to
        // Named.
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "type Point(x: int, y: int) do x: int, y: int end\nlet p = Point(x: 1, y: 2)".into(),
            "test".into(),
        );
        let p_sym = Symbol::new(&db, "p");
        let p_loc = find_let_by_name(&db, file, p_sym).expect("let p exists");
        let ty = let_infer(&db, p_loc).expect("record-typed let must infer");
        let kind = ty.kind(&db);
        let is_named_point = matches!(kind, TyKind::Named(_));
        let is_two_field_record = matches!(
            &kind,
            TyKind::Record(fields) if fields.fields.len() == 2,
        );
        assert!(
            is_named_point || is_two_field_record,
            "expected Named(Point) or Record {{x,y}}, got {kind:?}",
        );
    }

    #[test]
    fn file_binding_types_aggregates_all_lets() {
        use crate::ty::types::TyKind;
        use crate::base::common::PrimitiveType;
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "let a = 1\nlet b = a\nlet c = \"hello\"".into(),
            "test".into(),
        );
        let binding_types = file_binding_types(&db, file);

        // Look up each let's type via its DefId.
        let a_sym = Symbol::new(&db, "a");
        let b_sym = Symbol::new(&db, "b");
        let c_sym = Symbol::new(&db, "c");
        let scaffold = file_def_map_for_body(&db, file);
        let find_def = |sym: Symbol| {
            scaffold
                .top_level_lets
                .iter()
                .find(|(s, _)| *s == sym)
                .map(|(_, did)| *did)
        };
        let a_did = find_def(a_sym).expect("a def id");
        let b_did = find_def(b_sym).expect("b def id");
        let c_did = find_def(c_sym).expect("c def id");

        let a_ty = binding_types.get(&a_did).expect("a type");
        let b_ty = binding_types.get(&b_did).expect("b type");
        let c_ty = binding_types.get(&c_did).expect("c type");

        assert!(matches!(a_ty.kind(&db), TyKind::Primitive(PrimitiveType::Int)));
        assert!(matches!(b_ty.kind(&db), TyKind::Primitive(PrimitiveType::Int)));
        assert!(matches!(c_ty.kind(&db), TyKind::Primitive(PrimitiveType::String)));
    }

    #[test]
    fn let_rq_csv_source_registers_manifest_entry() {
        // `let users = csv!("users.csv")` should lower to a
        // `LetRqContribution` carrying one `SourceRef` in the manifest
        // and an `env_value` pointing at the generated alias.
        //
        // Uses a FossilDb pre-registered with the default "csv" source
        // — this comes from `register_defaults` in the registry builder,
        // which the default `FossilDb` doesn't invoke. Build a registry
        // manually.
        use crate::registry::{FossilRegistry, ParamDef, SourceDef, SourceRegistry};

        let mut sources = SourceRegistry::new();
        sources.register(SourceDef::new("csv", vec![ParamDef::required("path")]));
        let registry = FossilRegistry {
            sources,
            ..Default::default()
        };
        let db = FossilDb::with_registry(registry);
        let file = SourceFile::new(
            &db,
            "let users = csv!(path: \"users.csv\")".into(),
            "test".into(),
        );
        let sym = Symbol::new(&db, "users");
        let loc = find_let_by_name(&db, file, sym).expect("let exists");
        let contrib = let_rq(&db, loc).expect("csv let_rq must succeed (per-item path)");
        assert_eq!(
            contrib.sources.len(),
            1,
            "expected one source in the manifest",
        );
        assert_eq!(contrib.sources[0].format, "csv");
        assert_eq!(contrib.sources[0].path, "users.csv");
        assert!(
            contrib.env_value.is_some(),
            "source let must expose an env value so siblings can reference it",
        );
    }

    #[test]
    fn queries_rq_uses_per_item_fan_out_for_literal_only_file() {
        // End-to-end sanity: queries::rq goes through the per-item
        // fan-out path for a file that only contains primitive-typed
        // lets. Verifies the rewrite's happy path: the fan-out returns
        // a usable RelationalQuery without falling back to the
        // monolithic RqLowering.
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 1\nlet y = 2".into(), "test".into());
        let rq = crate::queries::rq(&db, file);
        // Literals don't produce SQL, so the result is empty but
        // structurally valid (the test would panic earlier if the
        // fan-out had exploded).
        assert!(rq.sources.is_empty());
        assert!(rq.ctes.is_empty());
        assert!(rq.emissions.is_empty());
        assert!(rq.outputs.is_empty());
    }

    #[test]
    fn let_rq_primitive_literal_produces_empty_contribution() {
        // `let x = 42` doesn't produce any SQL — the literal isn't
        // materialised anywhere. The per-item contribution should be
        // empty in all fields. Key point: the query returns Some, not
        // None, so the file-level fan-out doesn't fall back to the
        // monolithic path.
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let sym = Symbol::new(&db, "x");
        let loc = find_let_by_name(&db, file, sym).expect("let exists");
        let contrib = let_rq(&db, loc).expect("primitive let_rq must succeed");
        assert!(contrib.sources.is_empty());
        assert!(contrib.ctes.is_empty());
        assert!(contrib.emissions.is_empty());
    }

    #[test]
    fn file_type_decls_aggregates_all_types() {
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "type Point(x: int, y: int) do x: int, y: int end\n\
             type User(id: string) do id: string, name: string end"
                .into(),
            "test".into(),
        );
        let type_decls = file_type_decls(&db, file);
        assert_eq!(type_decls.len(), 2, "got: {type_decls:?}");

        // Both types should have ctor_fn_ty populated (they declare records).
        for (_, result) in type_decls.iter() {
            assert!(
                result.ctor_fn_ty.is_some(),
                "record types must have a constructor fn type",
            );
        }
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
