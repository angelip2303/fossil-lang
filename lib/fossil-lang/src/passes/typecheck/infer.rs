//! Type inference (Algorithm W)
//!
//! This module contains the core type inference logic implementing Algorithm W
//! from Hindley-Milner type system with support for row polymorphism.

use crate::ast::ast::{Literal, PrimitiveType};
use crate::ast::{hir, thir};
use crate::error::{CompileError, CompileErrorKind};

use super::{TypeChecker, subst::Subst};

impl TypeChecker {
    /// Algorithm W: infer the type of an expression
    /// Returns (substitution, type)
    pub fn infer(&mut self, expr_id: hir::ExprId) -> Result<(Subst, thir::TypeId), CompileError> {
        // Clone what we need before any mutable operations
        let expr = self.source.exprs.get(expr_id);
        let expr_kind = expr.kind.clone();
        let loc = expr.loc.clone();

        match &expr_kind {
            hir::ExprKind::Unit => {
                let ty = self.primitive_type(PrimitiveType::Unit, loc.clone());
                Ok((Subst::default(), ty))
            }

            hir::ExprKind::Literal(lit) => {
                let prim = match lit {
                    Literal::Integer(_) => PrimitiveType::Int,
                    Literal::String(_) => PrimitiveType::String,
                    Literal::Boolean(_) => PrimitiveType::Bool,
                };
                let ty = self.primitive_type(prim, loc.clone());
                Ok((Subst::default(), ty))
            }

            hir::ExprKind::Identifier(def_id) => {
                // Look up the polytype from the environment
                let poly = self
                    .env
                    .lookup(*def_id)
                    .ok_or_else(|| {
                        let def = self.gcx.definitions.get(*def_id);
                        CompileError::new(
                            CompileErrorKind::UndefinedVariable {
                                name: def.name,
                            },
                            loc.clone(),
                        )
                        .with_context(format!(
                            "Variable '{}' is used before it has been assigned a type",
                            self.gcx.interner.resolve(def.name)
                        ))
                    })?
                    .clone();

                // Instantiate with fresh type variables
                let ty = self.instantiate(&poly);
                Ok((Subst::default(), ty))
            }

            hir::ExprKind::List(items) => {
                if items.is_empty() {
                    // Empty list: infer as [α] where α is fresh
                    let elem_ty = self.fresh_type_var(loc.clone());
                    let list_ty = self.list_type(elem_ty, loc.clone());
                    Ok((Subst::default(), list_ty))
                } else {
                    // Infer type of first element
                    let (mut subst, elem_ty) = self.infer(items[0])?;

                    // Unify with all other elements
                    for &item in &items[1..] {
                        let (s1, item_ty) = self.infer(item)?;
                        subst = subst.compose(&s1, &mut self.target);

                        let elem_ty_applied = subst.apply(elem_ty, &mut self.target);
                        let s2 = self.unify(elem_ty_applied, item_ty, loc.clone())?;
                        subst = subst.compose(&s2, &mut self.target);
                    }

                    let final_elem_ty = subst.apply(elem_ty, &mut self.target);
                    let list_ty = self.list_type(final_elem_ty, loc.clone());
                    Ok((subst, list_ty))
                }
            }

            hir::ExprKind::Record(fields) => {
                let mut subst = Subst::default();
                let mut field_types = Vec::new();

                for (name, field_expr) in fields {
                    let (s, field_ty) = self.infer(*field_expr)?;
                    subst = subst.compose(&s, &mut self.target);
                    let field_ty = subst.apply(field_ty, &mut self.target);
                    field_types.push((*name, field_ty));
                }

                // Convert to RecordRow
                let row = thir::RecordRow::from_fields(field_types);
                let record_ty = self.record_type(row, loc.clone());

                Ok((subst, record_ty))
            }

            hir::ExprKind::Function { params, body } => {
                // Create fresh type variables for parameters
                let mut param_types = Vec::new();
                let mut local_env = self.env.clone();

                for param in params {
                    let param_ty = self.fresh_type_var(loc.clone());

                    // Add parameter to local type environment with its DefId
                    local_env.insert(param.def_id, thir::Polytype::mono(param_ty));
                    param_types.push(param_ty);
                }

                // Infer body with extended environment
                let saved_env = std::mem::replace(&mut self.env, local_env.clone());
                let (subst, ret_ty) = self.infer(*body)?;

                // Apply substitution to parameter types
                let param_types: Vec<_> = param_types
                    .into_iter()
                    .map(|ty| subst.apply(ty, &mut self.target))
                    .collect();

                let func_ty = self.function_type(param_types, ret_ty, loc.clone());

                // Restore environment after inference
                self.env = saved_env;

                Ok((subst, func_ty))
            }

            hir::ExprKind::Application { callee, args } => {
                // Infer type of callee
                let (mut subst, callee_ty) = self.infer(*callee)?;

                // Infer types of arguments
                let mut arg_types = Vec::new();
                for arg in args {
                    let (s, arg_ty) = self.infer(*arg)?;
                    subst = subst.compose(&s, &mut self.target);
                    let arg_ty = subst.apply(arg_ty, &mut self.target);
                    arg_types.push(arg_ty);
                }

                // Create fresh variable for return type
                let ret_ty = self.fresh_type_var(loc.clone());

                // Expected function type
                let expected_ty = self.function_type(arg_types, ret_ty, loc.clone());

                // Unify callee with expected function type
                let callee_ty = subst.apply(callee_ty, &mut self.target);
                let s = self.unify(callee_ty, expected_ty, loc)?;
                subst = subst.compose(&s, &mut self.target);

                let final_ret_ty = subst.apply(ret_ty, &mut self.target);
                Ok((subst, final_ret_ty))
            }

            hir::ExprKind::FieldAccess { expr, field } => {
                // Infer type of the expression
                let (mut subst, expr_ty) = self.infer(*expr)?;
                let expr_ty = subst.apply(expr_ty, &mut self.target);

                // Check the type
                let ty = self.target.types.get(expr_ty);
                match &ty.kind {
                    thir::TypeKind::Record(row) => {
                        // Try to lookup field in row
                        if let Some(field_ty) = row.lookup(*field) {
                            Ok((subst, field_ty))
                        } else {
                            // Row might be extensible with a var
                            // Create fresh type variable for field type
                            let field_ty = self.fresh_type_var(loc.clone());

                            // Create fresh row variable for rest
                            let rest_var = self.tvg.fresh();

                            // Expected row: (| field :: field_ty | rest_var |)
                            let expected_row = thir::RecordRow::Extend {
                                field: *field,
                                ty: field_ty,
                                rest: Box::new(thir::RecordRow::Var(rest_var)),
                            };

                            let expected_ty = self.record_type(expected_row, loc.clone());

                            // Unify
                            let s = self.unify(expr_ty, expected_ty, loc)?;
                            subst = subst.compose(&s, &mut self.target);

                            Ok((subst, field_ty))
                        }
                    }

                    thir::TypeKind::Var(_) => {
                        // Polymorphic access on type variable
                        // Create fresh variables
                        let field_ty = self.fresh_type_var(loc.clone());
                        let rest_var = self.tvg.fresh();

                        // Create row: (| field :: field_ty | rest_var |)
                        let row = thir::RecordRow::Extend {
                            field: *field,
                            ty: field_ty,
                            rest: Box::new(thir::RecordRow::Var(rest_var)),
                        };

                        let record_ty = self.record_type(row, loc.clone());

                        // Unify expr_ty with record_ty
                        let s = self.unify(expr_ty, record_ty, loc)?;
                        subst = subst.compose(&s, &mut self.target);

                        Ok((subst, field_ty))
                    }

                    _ => Err(CompileError::new(
                        CompileErrorKind::UndefinedVariable { name: *field },
                        loc.clone(),
                    )
                    .with_context(format!(
                        "Cannot access field '{}' on a non-record type",
                        self.gcx.interner.resolve(*field)
                    ))),
                }
            }

            hir::ExprKind::Block { stmts } => {
                let mut subst = Subst::default();

                // Type-check all statements in the block (except possibly the last)
                let num_stmts = stmts.len();
                for (i, &stmt_id) in stmts.iter().enumerate() {
                    let is_last = i == num_stmts - 1;
                    let stmt = self.source.stmts.get(stmt_id);

                    // For the last statement, we handle it specially if it's an Expr
                    if is_last {
                        match &stmt.kind {
                            hir::StmtKind::Expr(expr_id) => {
                                // Infer the type of the last expression
                                let (s, ty) = self.infer(*expr_id)?;
                                subst = subst.compose(&s, &mut self.target);
                                return Ok((subst, ty));
                            }
                            _ => {
                                // Last statement is not an expression - just check it
                                self.check_stmt(stmt_id)?;
                            }
                        }
                    } else {
                        // Not the last statement - just check it
                        self.check_stmt(stmt_id)?;
                    }
                }

                // Empty block or block ending with non-expression statement returns Unit
                let unit_ty = self.primitive_type(PrimitiveType::Unit, loc);
                Ok((subst, unit_ty))
            }
        }
    }

    /// Instantiate a polytype with fresh type variables
    pub fn instantiate(&mut self, poly: &thir::Polytype) -> thir::TypeId {
        if poly.forall.is_empty() {
            return poly.ty;
        }

        // Create substitution mapping quantified vars to fresh vars
        let mut subst = Subst::default();
        for &old_var in &poly.forall {
            let fresh_ty = self.fresh_type_var_generated();
            subst.insert(old_var, fresh_ty);
        }

        subst.apply(poly.ty, &mut self.target)
    }
}
