use std::collections::HashMap;

use crate::context::DefId;
use crate::context::global::BuiltInFieldType;
use crate::error::{FossilError, FossilErrors};
use crate::ir::{ExprId, Ir, Polytype, RecordFields, StmtId, StmtKind, Type, TypeId, TypeKind, TypeRef, TypeVar, Ident};
use crate::passes::{GlobalContext, IrProgram};

pub mod typeutil;
pub mod infer;
pub mod unify;

pub use typeutil::{TypeEnv, Subst};

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

pub(crate) struct TypeDeclInfo {
    pub ty: TypeId,
    pub ctor_param_count: usize,
}

pub struct TypeChecker {
    pub(crate) ir: Ir,
    pub(crate) gcx: GlobalContext,
    pub(crate) tvg: TypeVarGen,
    pub(crate) env: TypeEnv,
    pub(crate) type_index: HashMap<DefId, TypeDeclInfo>,
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
            type_index: HashMap::new(),
            infer_cache: HashMap::new(),
            global_subst: Subst::default(),
            local_subst: Subst::default(),
        };

        checker.build_type_index();
        checker.init_builtin_functions();
        checker
    }

    fn build_type_index(&mut self) {
        for &stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(stmt_id);
            if let StmtKind::Type { def_id: Some(def_id), ty, ctor_params, .. } = &stmt.kind {
                self.type_index.insert(*def_id, TypeDeclInfo {
                    ty: *ty,
                    ctor_param_count: ctor_params.len(),
                });
            }
        }

        let registered: Vec<_> = self.gcx.registered_types.iter()
            .map(|(def_id, fields)| (*def_id, fields.clone()))
            .collect();

        for (def_id, fields) in registered {
            if self.type_index.contains_key(&def_id) { continue; }
            let ir_fields = materialize_registered_fields(&fields, &mut self.ir);
            let record_ty = self.ir.alloc_type(TypeKind::Record(RecordFields::from_fields(ir_fields)));
            self.type_index.insert(def_id, TypeDeclInfo { ty: record_ty, ctor_param_count: 0 });
        }
    }

    fn init_builtin_functions(&mut self) {
        use crate::context::DefKind;

        for def_id in 0..self.gcx.definitions.len() {
            let def_id = DefId::new(def_id as u32);
            let def = self.gcx.definitions.get(def_id);

            if let DefKind::Func(func_impl) = &def.kind {
                let mut next_var = || self.tvg.fresh();
                let polytype = func_impl.signature(&mut self.ir, &mut next_var, &self.gcx);
                self.env.insert(def_id, polytype);
            }
        }

        self.init_record_constructors();
    }

    fn init_record_constructors(&mut self) {
        use crate::context::DefKind;

        let pairs: Vec<_> = self.type_index.keys().filter_map(|&type_def_id| {
            let type_def = self.gcx.definitions.get(type_def_id);
            let ctor_def_id = self.gcx.definitions.iter()
                .find(|def| def.name == type_def.name && matches!(def.kind, DefKind::RecordConstructor))
                .map(|def| def.id())?;
            Some((type_def_id, ctor_def_id))
        }).collect();

        for (type_def_id, ctor_def_id) in pairs {
            let info = &self.type_index[&type_def_id];
            let ty = self.ir.types.get(info.ty);
            let loc = ty.loc;

            if let TypeKind::Record(fields) = &ty.kind {
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

    pub(crate) fn lookup_type_info(&self, def_id: DefId) -> Option<&TypeDeclInfo> {
        self.type_index.get(&def_id)
    }

    pub fn check(mut self) -> Result<IrProgram, FossilErrors> {
        let root_ids = self.ir.root.clone();
        let mut errors = FossilErrors::new();

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

    fn check_stmt(&mut self, stmt_id: StmtId) -> Result<(), FossilError> {
        let stmt = self.ir.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();

        match stmt_kind {
            StmtKind::Let {
                name: _,
                def_id,
                value,
            } => {
                let (subst, inferred_ty) = self.infer(value)?;
                let final_ty = subst.apply(inferred_ty, &mut self.ir);

                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                if let Some(def_id) = def_id {
                    let poly = self.env.generalize(final_ty, &self.ir);
                    self.env.insert(def_id, poly);
                }

                self.ir.exprs.get_mut(value).ty = TypeRef::Known(final_ty);
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

fn materialize_registered_fields(
    fields: &[(crate::context::Symbol, BuiltInFieldType)],
    ir: &mut Ir,
) -> Vec<(crate::context::Symbol, TypeId)> {
    fields
        .iter()
        .map(|(sym, ft)| {
            let base = match ft {
                BuiltInFieldType::Required(p) | BuiltInFieldType::Optional(p) => {
                    ir.alloc_type(TypeKind::Primitive(*p))
                }
            };
            let ty = match ft {
                BuiltInFieldType::Optional(_) => ir.optional_type(base),
                BuiltInFieldType::Required(_) => base,
            };
            (*sym, ty)
        })
        .collect()
}
