//! HIR to THIR conversion
//!
//! This module handles conversion from HIR (High-level IR with resolved names)
//! to THIR (Typed HIR with type annotations).

use crate::ast::{hir, thir};
use crate::error::CompileError;

use super::TypeChecker;

impl TypeChecker {
    /// Convert HIR expression to THIR with type annotation
    pub fn fold_expr_to_thir(
        &mut self,
        hir_expr: hir::ExprId,
        ty: thir::TypeId,
    ) -> Result<thir::ExprId, CompileError> {
        // Check cache
        if let Some(&cached) = self.expr_cache.get(&hir_expr) {
            return Ok(cached);
        }

        // Clone what we need before any mutable operations
        let expr = self.source.exprs.get(hir_expr);
        let expr_kind = expr.kind.clone();
        let loc = expr.loc.clone();

        let thir_kind = match &expr_kind {
            hir::ExprKind::Identifier(def_id) => thir::ExprKind::Identifier(*def_id),
            hir::ExprKind::Unit => thir::ExprKind::Unit,
            hir::ExprKind::Literal(lit) => thir::ExprKind::Literal(lit.clone()),

            hir::ExprKind::List(items) => {
                let thir_items: Result<Vec<_>, CompileError> = items
                    .iter()
                    .map(|&item| {
                        let (subst, item_ty) = self.infer(item)?;
                        self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                        self.fold_expr_to_thir(item, item_ty)
                    })
                    .collect();
                thir::ExprKind::List(thir_items?)
            }

            hir::ExprKind::Record(fields) => {
                let thir_fields: Result<Vec<_>, CompileError> = fields
                    .iter()
                    .map(|(name, field_expr)| {
                        let (subst, field_ty) = self.infer(*field_expr)?;
                        self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                        let thir_field = self.fold_expr_to_thir(*field_expr, field_ty)?;
                        Ok((*name, thir_field))
                    })
                    .collect();
                thir::ExprKind::Record(thir_fields?)
            }

            hir::ExprKind::Function { params, body } => {
                // Extract parameter types and return type from the inferred function type
                // Apply global_subst to get resolved types
                let resolved_ty = self.global_subst.apply(ty, &mut self.target);
                let func_type = self.target.types.get(resolved_ty);

                let (param_types, ret_ty) = match &func_type.kind {
                    thir::TypeKind::Function(param_tys, ret) => {
                        (param_tys.clone(), *ret)
                    }
                    _ => {
                        // Fallback: if not a function type, infer fresh types
                        let param_types: Vec<_> = params.iter()
                            .map(|_| self.fresh_type_var_generated())
                            .collect();
                        let ret_ty = self.fresh_type_var_generated();
                        (param_types, ret_ty)
                    }
                };

                // Build the local env with the actual inferred parameter types
                let mut local_env = self.env.clone();
                for (param, &param_ty) in params.iter().zip(param_types.iter()) {
                    local_env.insert(param.def_id, thir::Polytype::mono(param_ty));
                }

                let saved_env = std::mem::replace(&mut self.env, local_env);
                let thir_body = self.fold_expr_to_thir(*body, ret_ty)?;
                self.env = saved_env;

                // Convert params with defaults
                let thir_params: Result<Vec<_>, CompileError> = params
                    .iter()
                    .map(|p| {
                        let default = if let Some(default_expr) = p.default {
                            let (subst, default_ty) = self.infer(default_expr)?;
                            self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                            Some(self.fold_expr_to_thir(default_expr, default_ty)?)
                        } else {
                            None
                        };
                        Ok(thir::Param {
                            name: p.name,
                            def_id: p.def_id,
                            default,
                        })
                    })
                    .collect();

                thir::ExprKind::Function {
                    params: thir_params?,
                    body: thir_body,
                }
            }

            hir::ExprKind::Application { callee, args } => {
                let (subst, callee_ty) = self.infer(*callee)?;
                self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                let thir_callee = self.fold_expr_to_thir(*callee, callee_ty)?;

                // Convert arguments, preserving named/positional distinction
                let thir_args: Result<Vec<_>, CompileError> = args
                    .iter()
                    .map(|arg| {
                        match arg {
                            hir::Argument::Positional(expr_id) => {
                                let (subst, arg_ty) = self.infer(*expr_id)?;
                                self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                                let thir_expr = self.fold_expr_to_thir(*expr_id, arg_ty)?;
                                Ok(thir::Argument::Positional(thir_expr))
                            }
                            hir::Argument::Named { name, value } => {
                                let (subst, arg_ty) = self.infer(*value)?;
                                self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                                let thir_expr = self.fold_expr_to_thir(*value, arg_ty)?;
                                Ok(thir::Argument::Named {
                                    name: *name,
                                    value: thir_expr,
                                })
                            }
                        }
                    })
                    .collect();

                thir::ExprKind::Application {
                    callee: thir_callee,
                    args: thir_args?,
                }
            }

            hir::ExprKind::FieldAccess { expr, field } => {
                // Check if this is a field selector (_.field)
                let expr_kind = &self.source.exprs.get(*expr).kind;
                if matches!(expr_kind, hir::ExprKind::Placeholder) {
                    // This is a field selector: _.field
                    // Get record_ty from the inferred FieldSelector type
                    let resolved_ty = self.global_subst.apply(ty, &mut self.target);
                    let record_ty = match &self.target.types.get(resolved_ty).kind {
                        thir::TypeKind::FieldSelector { record_ty, .. } => *record_ty,
                        _ => resolved_ty, // Fallback
                    };

                    thir::ExprKind::FieldSelector {
                        field: *field,
                        record_ty,
                    }
                } else {
                    // Normal field access
                    let (subst, expr_ty) = self.infer(*expr)?;
                    self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                    let thir_expr = self.fold_expr_to_thir(*expr, expr_ty)?;
                    thir::ExprKind::FieldAccess {
                        expr: thir_expr,
                        field: *field,
                    }
                }
            }

            hir::ExprKind::Block { stmts } => {
                // Convert all statements
                let thir_stmts: Result<Vec<_>, _> = stmts
                    .iter()
                    .map(|&stmt_id| self.check_stmt(stmt_id))
                    .collect();

                thir::ExprKind::Block {
                    stmts: thir_stmts?,
                }
            }

            hir::ExprKind::StringInterpolation { parts, exprs } => {
                // Convert all interpolated expressions
                let thir_exprs: Result<Vec<_>, CompileError> = exprs
                    .iter()
                    .map(|&expr_id| {
                        let (subst, expr_ty) = self.infer(expr_id)?;
                        self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                        self.fold_expr_to_thir(expr_id, expr_ty)
                    })
                    .collect();

                thir::ExprKind::StringInterpolation {
                    parts: parts.clone(),
                    exprs: thir_exprs?,
                }
            }

            hir::ExprKind::Placeholder => {
                // Standalone placeholder should have been rejected by the type checker
                // This is only reached when _.field is converted, which is handled in FieldAccess
                unreachable!("Placeholder should be handled in FieldAccess context")
            }
        };

        // Apply global substitution to resolve any remaining type variables
        let resolved_ty = self.global_subst.apply(ty, &mut self.target);

        let thir_id = self.target.exprs.alloc(thir::Expr {
            loc,
            kind: thir_kind,
            ty: resolved_ty,
        });

        self.expr_cache.insert(hir_expr, thir_id);
        Ok(thir_id)
    }

    /// Convert HIR type to THIR type
    pub fn fold_type_to_thir(&mut self, hir_ty: hir::TypeId) -> Result<thir::TypeId, CompileError> {
        // Check cache
        if let Some(&cached) = self.type_cache.get(&hir_ty) {
            return Ok(cached);
        }

        // Clone what we need before any mutable operations
        let ty = self.source.types.get(hir_ty);
        let type_kind = ty.kind.clone();
        let loc = ty.loc.clone();

        let thir_kind = match &type_kind {
            hir::TypeKind::Named(def_id) => thir::TypeKind::Named(*def_id),
            hir::TypeKind::Primitive(prim) => thir::TypeKind::Primitive(*prim),

            hir::TypeKind::Function(params, ret) => {
                let thir_params: Result<Vec<_>, CompileError> =
                    params.iter().map(|&p| self.fold_type_to_thir(p)).collect();
                let thir_ret = self.fold_type_to_thir(*ret)?;
                thir::TypeKind::Function(thir_params?, thir_ret)
            }

            hir::TypeKind::App { ctor, args } => {
                let thir_args: Result<Vec<_>, CompileError> = args
                    .iter()
                    .map(|&arg| self.fold_type_to_thir(arg))
                    .collect();
                thir::TypeKind::App {
                    ctor: *ctor,
                    args: thir_args?,
                }
            }

            hir::TypeKind::Record(fields) => {
                let thir_fields: Result<Vec<_>, CompileError> = fields
                    .iter()
                    .map(|field| Ok((field.name, self.fold_type_to_thir(field.ty)?)))
                    .collect();
                // Convert Vec to RecordRow
                let row = thir::RecordRow::from_fields(thir_fields?);
                thir::TypeKind::Record(row)
            }
        };

        let thir_id = self.target.types.alloc(thir::Type {
            loc,
            kind: thir_kind,
        });
        self.type_cache.insert(hir_ty, thir_id);
        Ok(thir_id)
    }
}
