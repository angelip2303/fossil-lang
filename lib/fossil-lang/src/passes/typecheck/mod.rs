//! Type checking and inference module
//!
//! Implements Hindley-Milner type inference (Algorithm W).

use std::collections::HashMap;

use crate::context::DefId;
use crate::error::{CompileError, CompileErrors};
use crate::ir::{ExprId, Ir, Polytype, StmtId, StmtKind, Type, TypeId, TypeKind, TypeRef, TypeVar, Ident};
use crate::passes::{GlobalContext, IrProgram};

pub mod env;
pub mod helpers;
pub mod infer;
pub mod subst;
pub mod unify;

pub use env::TypeEnv;
pub use subst::Subst;

/// Type variable generator
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

/// Type checker implementing Algorithm W
pub struct TypeChecker {
    pub(crate) ir: Ir,
    pub(crate) gcx: GlobalContext,
    pub(crate) tvg: TypeVarGen,
    pub(crate) env: TypeEnv,
    infer_cache: HashMap<ExprId, TypeId>,
    global_subst: Subst,
    local_subst: Subst,
}

impl TypeChecker {
    pub fn new(ir: Ir, gcx: GlobalContext) -> Self {
        let mut checker = Self {
            ir,
            gcx,
            tvg: TypeVarGen::default(),
            env: TypeEnv::default(),
            infer_cache: HashMap::new(),
            global_subst: Subst::default(),
            local_subst: Subst::default(),
        };

        checker.init_builtin_functions();
        checker
    }

    fn init_builtin_functions(&mut self) {
        use crate::context::DefKind;

        for def_id in 0..self.gcx.definitions.len() {
            let def_id = DefId::new(def_id as u32);
            let def = self.gcx.definitions.get(def_id);

            if let DefKind::Func(Some(func_impl)) = &def.kind {
                let mut next_var = || self.tvg.fresh();
                let polytype = func_impl.signature(&mut self.ir, &mut next_var, &self.gcx);
                self.env.insert(def_id, polytype);
            }
        }

        self.init_record_constructors();
    }

    fn init_record_constructors(&mut self) {
        use crate::context::DefKind;

        let type_stmts: Vec<(StmtId, crate::context::Symbol, TypeId)> = self
            .ir
            .root
            .iter()
            .filter_map(|&stmt_id| {
                let stmt = self.ir.stmts.get(stmt_id);
                if let StmtKind::Type { name, ty, .. } = &stmt.kind {
                    Some((stmt_id, *name, *ty))
                } else {
                    None
                }
            })
            .collect();

        for (_stmt_id, type_name, type_id) in type_stmts {
            let ty = self.ir.types.get(type_id);
            let loc = ty.loc;

            if let TypeKind::Record(fields) = &ty.kind {
                let ctor_def_id = self
                    .gcx
                    .definitions
                    .iter()
                    .find(|def| def.name == type_name && matches!(def.kind, DefKind::Func(None)))
                    .map(|def| def.id());

                if let Some(ctor_def_id) = ctor_def_id {
                    let type_def_id = self
                        .gcx
                        .definitions
                        .iter()
                        .find(|def| def.name == type_name && matches!(def.kind, DefKind::Type))
                        .map(|def| def.id());

                    if let Some(type_def_id) = type_def_id {
                        let param_types: Vec<_> = fields.fields.iter().map(|(_, ty)| *ty).collect();

                        let return_ty = self.ir.types.alloc(Type {
                            loc,
                            kind: TypeKind::Named(Ident::Resolved(type_def_id)),
                        });

                        let fn_ty = self.ir.types.alloc(Type {
                            loc,
                            kind: TypeKind::Function(param_types, return_ty),
                        });

                        self.env.insert(ctor_def_id, Polytype::mono(fn_ty));
                    }
                }
            }
        }
    }

    pub fn check(mut self) -> Result<IrProgram, CompileErrors> {
        let root_ids = self.ir.root.clone();
        let mut errors = CompileErrors::new();

        for stmt_id in root_ids {
            if let Err(e) = self.check_stmt(stmt_id) {
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(IrProgram {
            ir: self.ir,
            gcx: self.gcx,
            env: self.env,
        })
    }

    fn check_stmt(&mut self, stmt_id: StmtId) -> Result<(), CompileError> {
        let stmt = self.ir.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let loc = stmt.loc;

        match stmt_kind {
            StmtKind::Let {
                name: _,
                def_id,
                ty: type_annotation,
                value,
            } => {
                let (mut subst, inferred_ty) = self.infer(value)?;
                let inferred_ty = subst.apply(inferred_ty, &mut self.ir);

                let final_ty = if let Some(annotation_ty) = type_annotation {
                    let s = self.unify(annotation_ty, inferred_ty, loc)?;
                    subst = subst.compose(&s, &mut self.ir);
                    subst.apply(annotation_ty, &mut self.ir)
                } else {
                    inferred_ty
                };

                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                if let Some(def_id) = def_id {
                    let poly = self.env.generalize(final_ty, &self.ir);
                    self.env.insert(def_id, poly);
                }

                self.ir.exprs.get_mut(value).ty = TypeRef::Known(final_ty);
            }

            StmtKind::Const {
                name: _,
                def_id,
                value,
            } => {
                let (subst, inferred_ty) = self.infer(value)?;
                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                if let Some(def_id) = def_id {
                    let poly = self.env.generalize(inferred_ty, &self.ir);
                    self.env.insert(def_id, poly);
                }

                self.ir.exprs.get_mut(value).ty = TypeRef::Known(inferred_ty);
            }

            StmtKind::Type { .. } => {}

            StmtKind::Expr(expr) => {
                let (subst, ty) = self.infer(expr)?;
                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);
                self.ir.exprs.get_mut(expr).ty = TypeRef::Known(ty);
            }
        }

        Ok(())
    }

    pub fn instantiate(&mut self, poly: &Polytype) -> TypeId {
        if poly.forall.is_empty() {
            return poly.ty;
        }

        let mut subst = Subst::default();
        for &old_var in &poly.forall {
            let fresh_ty = self.fresh_type_var_generated();
            subst.insert(old_var, fresh_ty);
        }

        subst.apply(poly.ty, &mut self.ir)
    }
}
