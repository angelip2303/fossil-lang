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
                        let (_, item_ty) = self.infer(item)?;
                        self.fold_expr_to_thir(item, item_ty)
                    })
                    .collect();
                thir::ExprKind::List(thir_items?)
            }

            hir::ExprKind::Record(fields) => {
                let thir_fields: Result<Vec<_>, CompileError> = fields
                    .iter()
                    .map(|(name, field_expr)| {
                        let (_, field_ty) = self.infer(*field_expr)?;
                        let thir_field = self.fold_expr_to_thir(*field_expr, field_ty)?;
                        Ok((*name, thir_field))
                    })
                    .collect();
                thir::ExprKind::Record(thir_fields?)
            }

            hir::ExprKind::Function { params, body } => {
                // Need to restore local env for folding the body
                // Build the local env again from params
                let mut local_env = self.env.clone();
                for param in params {
                    let param_ty = self.fresh_type_var_generated();
                    local_env.insert(param.def_id, thir::Polytype::mono(param_ty));
                }

                let saved_env = std::mem::replace(&mut self.env, local_env);
                let (_, body_ty) = self.infer(*body)?;
                let thir_body = self.fold_expr_to_thir(*body, body_ty)?;
                self.env = saved_env;

                thir::ExprKind::Function {
                    params: params.clone(),
                    body: thir_body,
                }
            }

            hir::ExprKind::Application { callee, args } => {
                let (_, callee_ty) = self.infer(*callee)?;
                let thir_callee = self.fold_expr_to_thir(*callee, callee_ty)?;

                let thir_args: Result<Vec<_>, CompileError> = args
                    .iter()
                    .map(|&arg| {
                        let (_, arg_ty) = self.infer(arg)?;
                        self.fold_expr_to_thir(arg, arg_ty)
                    })
                    .collect();

                thir::ExprKind::Application {
                    callee: thir_callee,
                    args: thir_args?,
                }
            }

            hir::ExprKind::FieldAccess { expr, field } => {
                let (_, expr_ty) = self.infer(*expr)?;
                let thir_expr = self.fold_expr_to_thir(*expr, expr_ty)?;
                thir::ExprKind::FieldAccess {
                    expr: thir_expr,
                    field: *field,
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
        };

        let thir_id = self.target.exprs.alloc(thir::Expr {
            loc,
            kind: thir_kind,
            ty,
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
