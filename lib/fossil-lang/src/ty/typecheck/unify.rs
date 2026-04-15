//! Unification algorithm — operates on interned `Ty` values directly.

use crate::ast::Loc;
use crate::db::DefId;
use crate::error::FossilError;
use crate::ty::types::{RecordFields, Ty, TyKind, TypeVar};

use super::{typeutil::Subst, TypeChecker};

impl TypeChecker<'_> {
    pub fn resolve_named_type(&self, def_id: DefId) -> Option<Ty> {
        self.lookup_type_info(def_id).map(|info| info.ty)
    }

    pub fn unify(&mut self, ty1: Ty, ty2: Ty, loc: Loc) -> Result<Subst, FossilError> {
        if ty1 == ty2 {
            return Ok(Subst::default());
        }

        if let Some(def_id) = self.named_def_id(ty1) {
            return if let Some(resolved_ty) = self.resolve_named_type(def_id) {
                self.unify(resolved_ty, ty2, loc)
            } else {
                Err(FossilError::type_mismatch(
                    format!(
                        "Cannot resolve named type `{}` to unify with `{}`",
                        self.format_type(ty1),
                        self.format_type(ty2),
                    ),
                    loc,
                ))
            };
        }

        if let Some(def_id) = self.named_def_id(ty2) {
            return if let Some(resolved_ty) = self.resolve_named_type(def_id) {
                self.unify(ty1, resolved_ty, loc)
            } else {
                Err(FossilError::type_mismatch(
                    format!(
                        "Cannot resolve named type `{}` to unify with `{}`",
                        self.format_type(ty2),
                        self.format_type(ty1),
                    ),
                    loc,
                ))
            };
        }

        let k1 = ty1.kind(self.db);
        let k2 = ty2.kind(self.db);

        match (k1, k2) {
            // Tainted compilation: an `Error` type unifies with anything
            // without producing a new diagnostic. The original error has
            // already been emitted (proven by the `ErrorGuaranteed` payload),
            // so re-yelling here would just spam the user.
            (TyKind::Error(_), _) | (_, TyKind::Error(_)) => Ok(Subst::default()),

            (TyKind::Var(v1), TyKind::Var(v2)) if v1 == v2 => Ok(Subst::default()),
            (TyKind::Var(v), _) => self.bind_var(v, ty2, loc),
            (_, TyKind::Var(v)) => self.bind_var(v, ty1, loc),

            (TyKind::Unit, TyKind::Unit) => Ok(Subst::default()),
            (TyKind::Primitive(p1), TyKind::Primitive(p2)) if p1 == p2 => Ok(Subst::default()),
            (TyKind::Optional(inner1), TyKind::Optional(inner2)) => {
                self.unify(inner1, inner2, loc)
            }

            // T? expected, T provided → widening (safe coercion)
            (TyKind::Optional(inner1), _) => self.unify(inner1, ty2, loc),

            (TyKind::Record(fields1), TyKind::Record(fields2)) => {
                self.unify_records(fields1, fields2, loc)
            }

            (TyKind::Function(params1, ret1), TyKind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(FossilError::arity_mismatch(params1.len(), params2.len(), loc));
                }

                let mut subst = Subst::default();
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    let p1_applied = subst.apply(*p1, self.db);
                    let p2_applied = subst.apply(*p2, self.db);
                    let s = self.unify(p1_applied, p2_applied, loc)?;
                    subst = subst.compose(&s, self.db);
                }

                let r1 = subst.apply(ret1, self.db);
                let r2 = subst.apply(ret2, self.db);
                let s = self.unify(r1, r2, loc)?;

                Ok(subst.compose(&s, self.db))
            }

            _ => Err(FossilError::type_mismatch(
                format!(
                    "Expected type `{}`, but found `{}`",
                    self.format_type(ty1),
                    self.format_type(ty2),
                ),
                loc,
            )),
        }
    }

    pub fn bind_var(&mut self, var: TypeVar, ty: Ty, loc: Loc) -> Result<Subst, FossilError> {
        if matches!(ty.kind(self.db), TyKind::Var(v) if v == var) {
            return Ok(Subst::default());
        }

        if self.occurs_in(var, ty) {
            let error_var = crate::error::TypeVar(var.0);
            return Err(FossilError::infinite_type(error_var, loc));
        }

        let mut subst = Subst::default();
        subst.insert(var, ty);
        Ok(subst)
    }

    pub fn occurs_in(&self, var: TypeVar, ty: Ty) -> bool {
        match ty.kind(self.db) {
            TyKind::Var(v) => v == var,
            TyKind::Record(fields) => fields.fields.iter().any(|(_, t)| self.occurs_in(var, *t)),
            TyKind::Function(params, ret) => {
                params.iter().any(|p| self.occurs_in(var, *p)) || self.occurs_in(var, ret)
            }
            TyKind::Optional(inner) => self.occurs_in(var, inner),
            TyKind::Primitive(_)
            | TyKind::Named(_)
            | TyKind::Unit
            | TyKind::Error(_) => false,
        }
    }

    pub fn unify_records(
        &mut self,
        fields1: RecordFields,
        fields2: RecordFields,
        loc: Loc,
    ) -> Result<Subst, FossilError> {
        if fields1.len() != fields2.len() {
            return Err(FossilError::record_size_mismatch(
                fields2.len(),
                fields1.len(),
                loc,
            ));
        }

        let mut subst = Subst::default();
        for (name1, ty1) in &fields1.fields {
            if let Some(ty2) = fields2.lookup(*name1) {
                let ty1_applied = subst.apply(*ty1, self.db);
                let ty2_applied = subst.apply(ty2, self.db);
                let s = self.unify(ty1_applied, ty2_applied, loc)?;
                subst = subst.compose(&s, self.db);
            } else {
                let field_str = name1.text(self.db);
                return Err(FossilError::field_not_found(field_str, loc));
            }
        }

        Ok(subst)
    }
}
