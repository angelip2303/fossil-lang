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
        // Check cache first - if we've already inferred this expression, return cached type
        // Apply global_subst to resolve any type variables that were resolved after caching
        if let Some(&cached_ty) = self.infer_cache.get(&expr_id) {
            let resolved_ty = self.global_subst.apply(cached_ty, &mut self.target);
            return Ok((Subst::default(), resolved_ty));
        }

        // Clone what we need before any mutable operations
        let expr = self.source.exprs.get(expr_id);
        let expr_kind = expr.kind.clone();
        let loc = expr.loc.clone();

        let result = match &expr_kind {
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

                // For monomorphic types (no forall vars), apply local_subst to
                // resolve type variables that have been constrained by earlier
                // subexpressions (e.g., previous field accesses on the same var).
                // Don't apply to polymorphic types to preserve let-polymorphism.
                let ty = if poly.forall.is_empty() {
                    self.local_subst.apply(ty, &mut self.target)
                } else {
                    ty
                };

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
                // Save local_subst so we can restore it after this expression
                let saved_local_subst = std::mem::take(&mut self.local_subst);

                // Infer type of callee
                let (mut subst, callee_ty) = self.infer(*callee)?;
                // Update local_subst so subsequent lookups see constraints
                self.local_subst = self.local_subst.compose(&subst, &mut self.target);

                // Infer types of arguments (extracting the value from each Argument)
                // TODO: Full named parameter support would require looking up param names
                // from callee's signature and reordering. For now, we just use positional order.
                let mut arg_types = Vec::new();
                for arg in args {
                    let arg_expr_id = arg.value();
                    let (s, arg_ty) = self.infer(arg_expr_id)?;
                    subst = subst.compose(&s, &mut self.target);
                    // Update local_subst after each argument so subsequent arguments
                    // can see constraints from earlier ones (e.g., multiple field
                    // accesses on the same variable)
                    self.local_subst = self.local_subst.compose(&s, &mut self.target);
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

                // Restore local_subst - the constraints from this expression
                // will be propagated via the returned subst
                self.local_subst = saved_local_subst;

                let final_ret_ty = subst.apply(ret_ty, &mut self.target);
                Ok((subst, final_ret_ty))
            }

            hir::ExprKind::Placeholder => {
                // Standalone placeholder is invalid - only valid in _.field context
                Err(CompileError::new(
                    CompileErrorKind::InvalidPlaceholder,
                    loc,
                )
                .with_context("The placeholder `_` must be followed by a field access like `_.field`")
                .with_suggestion(crate::error::ErrorSuggestion::Help(
                    "Use `_.field` to create a type-safe field selector, e.g., `List::join(a, b, left_on: _.id, right_on: _.customer_id)`".to_string()
                )))
            }

            hir::ExprKind::FieldAccess { expr, field } => {
                // Check if this is a field selector (_.field)
                let expr_kind = &self.source.exprs.get(*expr).kind;
                if matches!(expr_kind, hir::ExprKind::Placeholder) {
                    // This is a field selector: _.field
                    // Create fresh type variables for record type and field type
                    let record_ty = self.fresh_type_var(loc.clone());
                    let field_ty = self.fresh_type_var(loc.clone());

                    // Constraint: record_ty must have the field
                    let rest_var = self.tvg.fresh();
                    let expected_row = thir::RecordRow::Extend {
                        field: *field,
                        ty: field_ty,
                        rest: Box::new(thir::RecordRow::Var(rest_var)),
                    };
                    let expected_record = self.record_type(expected_row, loc.clone());
                    let s1 = self.unify(record_ty, expected_record, loc.clone())?;

                    // Apply substitution before allocating to avoid borrow issues
                    let resolved_record_ty = s1.apply(record_ty, &mut self.target);
                    let resolved_field_ty = s1.apply(field_ty, &mut self.target);

                    // Create type FieldSelector<record_ty, field_ty>
                    let selector_ty = self.target.types.alloc(thir::Type {
                        loc: loc.clone(),
                        kind: thir::TypeKind::FieldSelector {
                            record_ty: resolved_record_ty,
                            field_ty: resolved_field_ty,
                            field: *field,
                        },
                    });

                    return Ok((s1, selector_ty));
                }

                // Normal field access (existing code)
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

                    thir::TypeKind::FieldSelector { field: selector_field, .. } => {
                        // Trying to access a field on a FieldSelector (e.g., _.foo.bar)
                        let selector_name = self.gcx.interner.resolve(*selector_field);
                        let field_name = self.gcx.interner.resolve(*field);
                        Err(CompileError::new(
                            CompileErrorKind::UndefinedVariable { name: *field },
                            loc.clone(),
                        )
                        .with_context(format!(
                            "Cannot access field '{}' on a field selector `_.{}`",
                            field_name, selector_name
                        ))
                        .with_suggestion(crate::error::ErrorSuggestion::Help(
                            "Field selectors (_.field) are references to column names, not records. \
                             To select a nested field, use a separate field selector or access the field directly on the record.".to_string()
                        )))
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
                let mut block_ty = None;

                // Type-check all statements in the block (except possibly the last)
                let num_stmts = stmts.len();
                for (i, &stmt_id) in stmts.iter().enumerate() {
                    let is_last = i == num_stmts - 1;
                    let stmt = self.source.stmts.get(stmt_id);

                    // For the last statement, we handle it specially if it's an Expr
                    if is_last {
                        match &stmt.kind {
                            hir::StmtKind::Expr(inner_expr_id) => {
                                // Infer the type of the last expression
                                let (s, ty) = self.infer(*inner_expr_id)?;
                                subst = subst.compose(&s, &mut self.target);
                                block_ty = Some(ty);
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

                // Return the block type or Unit if empty/non-expression ending
                let final_ty = block_ty.unwrap_or_else(|| self.primitive_type(PrimitiveType::Unit, loc));
                Ok((subst, final_ty))
            }

            hir::ExprKind::StringInterpolation { parts: _, exprs } => {
                // String interpolation always produces a string
                // Type-check all interpolated expressions (they will be converted to strings)
                let mut subst = Subst::default();
                for &expr in exprs {
                    let (s, _) = self.infer(expr)?;
                    subst = subst.compose(&s, &mut self.target);
                    // Note: We don't require any specific type for interpolated expressions
                    // They will be converted to strings at runtime
                }
                let ty = self.primitive_type(PrimitiveType::String, loc);
                Ok((subst, ty))
            }
        };

        // Cache the result before returning
        if let Ok((_, ty)) = &result {
            self.infer_cache.insert(expr_id, *ty);
        }

        result
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
