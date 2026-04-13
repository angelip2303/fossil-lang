//! Type checker — Hindley–Milner inference over the resolved IR, producing
//! interned `Ty` values.
//!
//! The lowering pass ([`crate::passes::lower`]) builds an `Ir` whose type
//! arena (`ir::Type` / `ir::TypeId`) carries the AST-shaped source types.
//! At type-check entry we convert those into globally-interned `Ty` values
//! ([`Ty`]) and then operate exclusively on `Ty`. Inference results
//! (`TypeIndex`, `TypeckResults`) are likewise expressed in `Ty`.
//!
//! The IR arena stays as the lowering-side representation so the lowering
//! pass and any downstream consumer that walks `Ir.types` is unaffected.

use std::collections::HashMap;

use crate::db::{Db, DefId};
use crate::def_map::{BuiltInFieldType, DefMap, RegisteredTypes};
use crate::error::FossilError;
use crate::ir::{
    self, ExprId, Ir, PrimitiveType, StmtId, StmtKind, TypeId as IrTypeId, TypeKind as IrTypeKind,
};
use crate::passes::InferResult;
use crate::ty::types::{Polytype, Ty, TyKind, TypeVar};

pub mod infer;
pub mod results;
pub mod typeutil;
pub mod unify;

pub use results::{TypeDeclInfo, TypeIndex, TypeckResults};
pub use typeutil::{Subst, TypeEnv};

#[derive(Default)]
pub struct TypeVarGen {
    counter: usize,
}

impl TypeVarGen {
    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.counter);
        self.counter += 1;
        var
    }
}

pub struct TypeChecker<'a> {
    pub(crate) db: &'a dyn Db,
    pub(crate) ir: Ir,
    pub(crate) def_map: DefMap,
    pub(crate) registered_types: RegisteredTypes,
    pub(crate) resolutions: ir::Resolutions,
    pub(crate) tvg: TypeVarGen,
    pub(crate) env: TypeEnv,
    pub(crate) type_index: TypeIndex,
    pub(crate) typeck_results: TypeckResults,
    pub(crate) infer_cache: HashMap<ExprId, Ty>,
    pub(crate) global_subst: Subst,
    pub(crate) local_subst: Subst,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        db: &'a dyn Db,
        ir: Ir,
        def_map: DefMap,
        registered_types: RegisteredTypes,
        resolutions: ir::Resolutions,
    ) -> Self {
        let mut checker = Self {
            db,
            ir,
            def_map,
            registered_types,
            resolutions,
            tvg: TypeVarGen::default(),
            env: TypeEnv::default(),
            type_index: TypeIndex::default(),
            typeck_results: TypeckResults::default(),
            infer_cache: HashMap::new(),
            global_subst: Subst::default(),
            local_subst: Subst::default(),
        };

        checker.build_type_index();
        checker.init_record_constructors();
        checker
    }

    /// Convert an `ir::TypeId` (lowering-side arena entry) into an interned
    /// `Ty`. The lowering pass never produces `TypeKind::Var`, so a var here
    /// is a programming error.
    pub(crate) fn intern_ir_type(&self, ir_ty: IrTypeId) -> Ty {
        intern_ir_type(self.db, &self.ir, ir_ty)
    }

    fn build_type_index(&mut self) {
        for &stmt_id in &self.ir.root {
            let stmt = &self.ir.stmts[stmt_id];
            if let StmtKind::Type { ty, ctor_params, .. } = &stmt.kind {
                if let Some(&def_id) = self.resolutions.stmt_defs.get(&stmt_id) {
                    let ir_kind = self.ir.types[*ty].kind.clone();
                    let field_names = match &ir_kind {
                        IrTypeKind::Record(fields) => fields.field_names(),
                        _ => vec![],
                    };
                    let interned_ty = self.intern_ir_type(*ty);
                    self.type_index.insert(
                        def_id,
                        TypeDeclInfo {
                            ty: interned_ty,
                            ctor_param_count: ctor_params.len(),
                            ctor_param_names: ctor_params.iter().map(|p| p.name).collect(),
                            field_names,
                        },
                    );

                    // Populate registered_types for user-defined types so that
                    // downstream metadata consumers see field-shape info even
                    // when the type was declared inline (no provider source).
                    if !self.registered_types.contains_key(&def_id) {
                        if let IrTypeKind::Record(fields) = &ir_kind {
                            let field_types: Vec<_> = fields
                                .fields
                                .iter()
                                .filter_map(|(sym, ty_id)| {
                                    extract_field_type(&self.ir, *ty_id).map(|ft| (*sym, ft))
                                })
                                .collect();
                            if !field_types.is_empty() {
                                self.registered_types.insert(def_id, field_types);
                            }
                        }
                    }
                }
            }
        }

        let registered: Vec<_> = self
            .registered_types
            .iter()
            .map(|(def_id, fields)| (*def_id, fields.clone()))
            .collect();

        for (def_id, fields) in registered {
            if self.type_index.contains_key(&def_id) {
                continue;
            }
            let interned_fields = materialize_registered_fields(self.db, &fields);
            let field_names: Vec<_> = interned_fields.iter().map(|(name, _)| *name).collect();
            let record_ty = Ty::mk_record(self.db, interned_fields);
            self.type_index.insert(
                def_id,
                TypeDeclInfo {
                    ty: record_ty,
                    ctor_param_count: 0,
                    ctor_param_names: vec![],
                    field_names,
                },
            );
        }
    }

    fn init_record_constructors(&mut self) {
        use crate::db::DefKindTag;
        use crate::ir::CtorParam;

        // Collect ctor_params from IR statements for types that have them
        let mut type_ctor_params: HashMap<DefId, Vec<CtorParam>> = HashMap::new();
        for &stmt_id in &self.ir.root {
            let stmt = &self.ir.stmts[stmt_id];
            if let StmtKind::Type { ctor_params, .. } = &stmt.kind {
                if !ctor_params.is_empty() {
                    if let Some(&def_id) = self.resolutions.stmt_defs.get(&stmt_id) {
                        type_ctor_params.insert(def_id, ctor_params.clone());
                    }
                }
            }
        }

        let pairs: Vec<_> = self
            .type_index
            .keys()
            .filter_map(|&type_def_id| {
                let type_name = type_def_id.name(self.db);
                let ctor_def_id = self.def_map.find_by_symbol(type_name, self.db, |k| {
                    matches!(k, DefKindTag::RecordConstructor)
                })?;
                Some((type_def_id, ctor_def_id))
            })
            .collect();

        for (type_def_id, ctor_def_id) in pairs {
            let info = self.type_index.get(&type_def_id).unwrap();
            let ty_kind = info.ty.kind(self.db);

            if let TyKind::Record(fields) = ty_kind {
                let field_types: Vec<_> = fields.fields.iter().map(|(_, t)| *t).collect();
                let return_ty = Ty::mk_named(self.db, type_def_id);

                // The constructor fn_type registered in env is consumed
                // exclusively by Application inference (infer.rs).
                // RecordInstance inference resolves the type from expr_defs
                // without consulting this signature.
                //
                // When ctor_params are declared they define the constructor's
                // public call interface. Their types must be used regardless
                // of whether the count matches the field count — that
                // distinction is a runtime concern (identity vs full
                // construction), not a type-level concern.
                let param_types: Vec<_> = if let Some(ctor_params) =
                    type_ctor_params.get(&type_def_id)
                {
                    ctor_params
                        .iter()
                        .map(|p| intern_ir_type(self.db, &self.ir, p.ty))
                        .collect()
                } else {
                    field_types
                };

                let fn_ty = Ty::mk_function(self.db, param_types, return_ty);
                self.env.insert(ctor_def_id, Polytype::mono(fn_ty));
            }
        }
    }

    pub(crate) fn lookup_type_info(&self, def_id: DefId) -> Option<&TypeDeclInfo> {
        self.type_index.get(&def_id)
    }

    pub(crate) fn named_def_id(&self, ty: Ty) -> Option<DefId> {
        match ty.kind(self.db) {
            TyKind::Named(def_id) => Some(def_id),
            _ => None,
        }
    }

    /// Type-check the program. Diagnostics for any failure are emitted into
    /// the Salsa accumulator via [`crate::error::emit_error`], and failed
    /// `let` bindings are tainted with `Ty::Error` so downstream uses
    /// propagate without re-yelling. The returned `InferResult` is always
    /// populated (best-effort) — there is no separate "error" path; callers
    /// observe failure by reading accumulated diagnostics.
    pub fn check(self) -> InferResult {
        self.check_tolerant()
    }

    /// Type-check and always return partial results, even when there are
    /// errors. Used by the LSP to keep snapshots up-to-date for completions.
    pub fn check_tolerant(mut self) -> InferResult {
        let root_ids = self.ir.root.clone();

        for stmt_id in root_ids {
            if let Err(e) = self.check_stmt(stmt_id) {
                // Emit the diagnostic and obtain proof of emission. Taint the
                // failed Let binding with `Ty::Error` so downstream statements
                // that reference this name unify against `Error`, which
                // absorbs silently — the user sees the root cause once rather
                // than an avalanche of follow-on mismatches.
                let guarantee = crate::error::emit_error(self.db, e);
                if let Some(&def_id) = self.resolutions.stmt_defs.get(&stmt_id) {
                    let err_ty = Ty::mk_error(self.db, guarantee);
                    self.env.insert(def_id, Polytype::mono(err_ty));
                }
            }
        }

        // Finalize: populate expr_types for ALL expressions with
        // fully-resolved types.
        let cached: Vec<_> = self.infer_cache.iter().map(|(&k, &v)| (k, v)).collect();
        for (expr_id, raw_ty) in cached {
            self.typeck_results
                .expr_types
                .entry(expr_id)
                .or_insert_with(|| self.global_subst.apply(raw_ty, self.db));
        }

        // Export binding types for LSP completions.
        {
            use crate::db::DefKindTag;
            for (&def_id, poly) in &self.env.bindings {
                if matches!(def_id.kind(self.db), DefKindTag::Let) {
                    let resolved_ty = self.global_subst.apply(poly.ty, self.db);
                    self.typeck_results.binding_types.insert(def_id, resolved_ty);
                }
            }
        }

        InferResult {
            ir: self.ir,
            type_index: self.type_index,
            resolutions: self.resolutions,
            typeck_results: self.typeck_results,
        }
    }

    fn check_stmt(&mut self, stmt_id: StmtId) -> Result<(), FossilError> {
        let stmt = &self.ir.stmts[stmt_id];
        let stmt_kind = stmt.kind.clone();

        match stmt_kind {
            StmtKind::Let { value, .. } => {
                let (subst, inferred_ty) = self.infer(value)?;
                let final_ty = subst.apply(inferred_ty, self.db);

                self.global_subst = self.global_subst.compose(&subst, self.db);

                if let Some(&def_id) = self.resolutions.stmt_defs.get(&stmt_id) {
                    let poly = self.env.generalize(final_ty, self.db);
                    self.env.insert(def_id, poly);
                }

                self.typeck_results.expr_types.insert(value, final_ty);
            }

            StmtKind::Type { .. } => {}

            StmtKind::Expr(expr) => {
                let (subst, ty) = self.infer(expr)?;
                self.global_subst = self.global_subst.compose(&subst, self.db);
                self.typeck_results.expr_types.insert(expr, ty);
            }
        }

        Ok(())
    }

    pub fn instantiate(&mut self, poly: &Polytype) -> Ty {
        if poly.forall.is_empty() {
            return poly.ty;
        }

        let mut subst = Subst::default();
        for &old_var in &poly.forall {
            let fresh_ty = self.fresh_type_var_generated();
            subst.insert(old_var, fresh_ty);
        }

        subst.apply(poly.ty, self.db)
    }
}

/// Free-function `intern_ir_type`, callable when only `&Ir` and `&dyn Db` are
/// available (without taking the whole `TypeChecker`). The IR `TypeKind`
/// only contains the surface-syntax variants the lowering pass can produce
/// (no `Function`, no `Var`) so this function is total.
pub(crate) fn intern_ir_type(db: &dyn Db, ir: &Ir, ir_ty: IrTypeId) -> Ty {
    match ir.types[ir_ty].kind.clone() {
        IrTypeKind::Primitive(p) => Ty::new(db, TyKind::Primitive(p)),
        IrTypeKind::Unit => Ty::mk_unit(db),
        IrTypeKind::Named(def_id) => Ty::mk_named(db, def_id),
        IrTypeKind::Optional(inner) => {
            let inner = intern_ir_type(db, ir, inner);
            Ty::mk_optional(db, inner)
        }
        IrTypeKind::Record(fields) => {
            let fields: Vec<_> = fields
                .fields
                .into_iter()
                .map(|(sym, t)| (sym, intern_ir_type(db, ir, t)))
                .collect();
            Ty::mk_record(db, fields)
        }
    }
}

/// Extract `BuiltInFieldType` from an IR type node. Returns `None` for types
/// without a clear runtime representation (e.g. functions, records). Named
/// types map to `String` because at runtime record references become
/// string identifiers / IRIs.
fn extract_field_type(ir: &Ir, ty_id: IrTypeId) -> Option<BuiltInFieldType> {
    match &ir.types[ty_id].kind {
        IrTypeKind::Primitive(p) => Some(BuiltInFieldType::Required(*p)),
        IrTypeKind::Optional(inner_id) => match &ir.types[*inner_id].kind {
            IrTypeKind::Primitive(p) => Some(BuiltInFieldType::Optional(*p)),
            IrTypeKind::Named(_) => Some(BuiltInFieldType::Optional(PrimitiveType::String)),
            _ => None,
        },
        IrTypeKind::Named(_) => Some(BuiltInFieldType::Required(PrimitiveType::String)),
        _ => None,
    }
}

fn materialize_registered_fields(
    db: &dyn Db,
    fields: &[(crate::db::Symbol, BuiltInFieldType)],
) -> Vec<(crate::db::Symbol, Ty)> {
    fields
        .iter()
        .map(|(sym, ft)| {
            let base = match ft {
                BuiltInFieldType::Required(p) | BuiltInFieldType::Optional(p) => {
                    Ty::new(db, TyKind::Primitive(*p))
                }
            };
            let ty = match ft {
                BuiltInFieldType::Optional(_) => Ty::mk_optional(db, base),
                BuiltInFieldType::Required(_) => base,
            };
            (*sym, ty)
        })
        .collect()
}

