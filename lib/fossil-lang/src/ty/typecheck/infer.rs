//! Hindley–Milner inference over the resolved IR, producing interned `Ty`
//! values.

use std::collections::HashSet;

use crate::db::Symbol;
use crate::error::FossilError;
use crate::ir::{ExprId, ExprKind, Literal};
use crate::ty::types::{Polytype, Ty, TyKind};

use super::{typeutil::Subst, TypeChecker};

impl TypeChecker<'_> {
    pub fn infer(&mut self, expr_id: ExprId) -> Result<(Subst, Ty), FossilError> {
        let expr_id = self
            .resolutions
            .expr_rewrites
            .get(&expr_id)
            .copied()
            .unwrap_or(expr_id);

        if let Some(&cached_ty) = self.infer_cache.get(&expr_id) {
            let resolved_ty = self.global_subst.apply(cached_ty, self.db);
            return Ok((Subst::default(), resolved_ty));
        }

        let expr = &self.ir.exprs[expr_id];
        let expr_kind = expr.kind.clone();
        let loc = expr.loc;

        let result = match &expr_kind {
            ExprKind::Unit => Ok((Subst::default(), Ty::mk_unit(self.db))),

            ExprKind::Literal(lit) => {
                let ty = match lit {
                    Literal::Integer(_) => Ty::mk_int(self.db),
                    Literal::String(_) => Ty::mk_string(self.db),
                    Literal::Boolean(_) => Ty::mk_bool(self.db),
                };
                Ok((Subst::default(), ty))
            }

            ExprKind::Identifier(_) => {
                let def_id = self.resolutions.expr_defs.get(&expr_id).ok_or_else(|| {
                    FossilError::internal(
                        "typecheck",
                        "Unresolved identifier reached type checker",
                        loc,
                    )
                })?;

                let poly = self
                    .env
                    .lookup(*def_id)
                    .ok_or_else(|| {
                        let name_str = def_id.name(self.db).text(self.db);
                        FossilError::undefined_variable(name_str, loc)
                    })?
                    .clone();

                let ty = self.instantiate(&poly);
                let ty = if poly.forall.is_empty() {
                    self.local_subst.apply(ty, self.db)
                } else {
                    ty
                };

                Ok((Subst::default(), ty))
            }

            ExprKind::RecordInstance { ctor_args, spread, fields, .. } => {
                let type_def_id = *self.resolutions.expr_defs.get(&expr_id).ok_or_else(|| {
                    FossilError::internal("typecheck", "Unresolved type in record instance", loc)
                })?;

                let mut subst = Subst::default();

                for arg in ctor_args {
                    let (s, _arg_ty) = self.infer(arg.value())?;
                    subst = subst.compose(&s, self.db);
                }

                if let Some(spread_expr) = spread {
                    let (s, _spread_ty) = self.infer(*spread_expr)?;
                    subst = subst.compose(&s, self.db);
                }

                for (_name, field_expr) in fields {
                    let (s, _field_ty) = self.infer(*field_expr)?;
                    subst = subst.compose(&s, self.db);
                }

                self.check_ctor_arg_count(type_def_id, ctor_args.len(), loc)?;

                if let Some(info) = self.lookup_type_info(type_def_id) {
                    let valid_fields = &info.field_names;
                    for (field_name, _) in fields {
                        if !valid_fields.contains(field_name) {
                            let name = field_name.text(self.db);
                            return Err(FossilError::field_not_found(name, loc));
                        }
                    }
                }

                let named_ty = Ty::mk_named(self.db, type_def_id);
                Ok((subst, named_ty))
            }

            ExprKind::Projection { source, outputs, .. } => {
                let (mut subst, source_ty) = self.infer(*source)?;
                let binding_ty = subst.apply(source_ty, self.db);

                if let Some(&binding_def_id) = self.resolutions.expr_defs.get(&expr_id) {
                    self.env.insert(binding_def_id, Polytype::mono(binding_ty));
                }

                let mut last_ty = self.fresh_type_var(loc);
                for &output in outputs {
                    let (s, ty) = self.infer(output)?;
                    subst = subst.compose(&s, self.db);
                    last_ty = ty;
                }

                Ok((subst, last_ty))
            }

            ExprKind::Join { left, right, left_on, right_on, suffix } => {
                let (mut subst, left_ty) = self.infer(*left)?;
                let (s, right_ty) = self.infer(*right)?;
                subst = subst.compose(&s, self.db);

                let left_ty_applied = subst.apply(left_ty, self.db);
                let right_ty_applied = subst.apply(right_ty, self.db);

                let left_fields = self.get_record_fields(left_ty_applied, loc)?;
                let right_fields = self.get_record_fields(right_ty_applied, loc)?;

                for on_sym in left_on {
                    if !left_fields.iter().any(|(f, _)| f == on_sym) {
                        let name = on_sym.text(self.db);
                        return Err(FossilError::field_not_found(name, loc));
                    }
                }

                for on_sym in right_on {
                    if !right_fields.iter().any(|(f, _)| f == on_sym) {
                        let name = on_sym.text(self.db);
                        return Err(FossilError::field_not_found(name, loc));
                    }
                }

                let suffix_str = suffix
                    .map(|s| s.text(self.db).to_string())
                    .unwrap_or_else(|| "_right".to_string());

                let left_names: HashSet<_> = left_fields.iter().map(|(n, _)| *n).collect();

                let mut merged: Vec<(Symbol, Ty)> = left_fields.clone();
                for (name, ty) in &right_fields {
                    if left_names.contains(name) {
                        let name_str = name.text(self.db);
                        let suffixed = format!("{}{}", name_str, suffix_str);
                        let suffixed_sym = Symbol::new(self.db, &suffixed);
                        merged.push((suffixed_sym, *ty));
                    } else {
                        merged.push((*name, *ty));
                    }
                }

                let merged_ty = Ty::mk_record(self.db, merged);
                Ok((subst, merged_ty))
            }

            ExprKind::Application { callee, args, .. } => {
                let saved_local_subst = std::mem::take(&mut self.local_subst);

                let (mut subst, callee_ty) = self.infer(*callee)?;
                self.local_subst = self.local_subst.compose(&subst, self.db);

                let mut arg_types = Vec::new();
                for arg in args {
                    let arg_expr_id = arg.value();
                    let (s, arg_ty) = self.infer(arg_expr_id)?;
                    subst = subst.compose(&s, self.db);
                    self.local_subst = self.local_subst.compose(&s, self.db);
                    let arg_ty = subst.apply(arg_ty, self.db);
                    arg_types.push(arg_ty);
                }

                let ret_ty = self.fresh_type_var(loc);
                let callee_ty_applied = subst.apply(callee_ty, self.db);
                let expected_ty = Ty::mk_function(self.db, arg_types, ret_ty);

                let s = self.unify(callee_ty_applied, expected_ty, loc)?;
                subst = subst.compose(&s, self.db);

                self.local_subst = saved_local_subst;

                let final_ret_ty = subst.apply(ret_ty, self.db);
                Ok((subst, final_ret_ty))
            }

            ExprKind::FieldAccess { expr, field } => {
                let (subst, expr_ty) = self.infer(*expr)?;
                let expr_ty = subst.apply(expr_ty, self.db);

                if let Some(def_id) = self.named_def_id(expr_ty) {
                    if let Some(underlying_ty) = self.resolve_named_type(def_id) {
                        if let TyKind::Record(fields) = underlying_ty.kind(self.db) {
                            if let Some(field_ty) = fields.lookup(*field) {
                                return Ok((subst, field_ty));
                            }
                        }
                    }
                    let field_str = field.text(self.db);
                    return Err(FossilError::field_not_found(field_str, loc));
                }

                match expr_ty.kind(self.db) {
                    TyKind::Record(fields) => {
                        if let Some(field_ty) = fields.lookup(*field) {
                            Ok((subst, field_ty))
                        } else {
                            let field_str = field.text(self.db);
                            Err(FossilError::field_not_found(field_str, loc))
                        }
                    }

                    TyKind::Var(_) => {
                        let field_ty = self.fresh_type_var(loc);
                        Ok((subst, field_ty))
                    }

                    _ => {
                        let field_str = field.text(self.db);
                        Err(FossilError::field_not_found(field_str, loc))
                    }
                }
            }

            ExprKind::StringInterpolation { parts: _, exprs } => {
                let mut subst = Subst::default();
                for &expr in exprs {
                    let (s, _) = self.infer(expr)?;
                    subst = subst.compose(&s, self.db);
                }
                Ok((subst, Ty::mk_string(self.db)))
            }

            ExprKind::Coalesce { value, default } => {
                let (mut subst, value_ty) = self.infer(*value)?;
                let (s, default_ty) = self.infer(*default)?;
                subst = subst.compose(&s, self.db);

                // If value is T?, result is T (unwrap optional via default).
                // If value is T (non-optional), result is T.
                let value_ty_applied = subst.apply(value_ty, self.db);
                let inner_ty = match value_ty_applied.kind(self.db) {
                    TyKind::Optional(inner) => inner,
                    _ => value_ty_applied,
                };

                let s = self.unify(inner_ty, default_ty, loc)?;
                subst = subst.compose(&s, self.db);

                let result_ty = subst.apply(inner_ty, self.db);
                Ok((subst, result_ty))
            }

            ExprKind::Ref { args, .. } => {
                let mut subst = Subst::default();
                for &arg in args {
                    let (s, _) = self.infer(arg)?;
                    subst = subst.compose(&s, self.db);
                }
                let type_def_id = *self.resolutions.expr_defs.get(&expr_id).ok_or_else(|| {
                    FossilError::internal("typecheck", "Unresolved type in ref expression", loc)
                })?;
                Ok((subst, Ty::mk_named(self.db, type_def_id)))
            }
        };

        if let Ok((_, ty)) = &result {
            self.infer_cache.insert(expr_id, *ty);
        }

        result
    }

    /// Returns the field list of a record-like `Ty`, resolving `Named` types
    /// through the type index. Returns the fields as `Vec<(Symbol, Ty)>` so
    /// the caller can iterate without having to walk `TyKind` again.
    fn get_record_fields(
        &self,
        ty: Ty,
        loc: crate::ast::Loc,
    ) -> Result<Vec<(Symbol, Ty)>, FossilError> {
        if let Some(def_id) = self.named_def_id(ty) {
            if let Some(underlying) = self.resolve_named_type(def_id) {
                if let TyKind::Record(fields) = underlying.kind(self.db) {
                    return Ok(fields.fields);
                }
            }
        }
        if let TyKind::Record(fields) = ty.kind(self.db) {
            return Ok(fields.fields);
        }
        Err(FossilError::internal(
            "typecheck",
            "Join requires record types",
            loc,
        ))
    }
}
