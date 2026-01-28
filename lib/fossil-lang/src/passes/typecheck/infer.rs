//! Type inference (Algorithm W)
//!
//! This module contains the core type inference logic implementing Algorithm W
//! from Hindley-Milner type system with support for row polymorphism.
//!
//! The key difference from the previous HIR/THIR-based implementation is that
//! we now work directly on the unified IR, inferring types in-place.

use crate::error::{CompileError, CompileErrorKind};
use crate::ir::{
    ExprId, ExprKind, Ident, Literal, Polytype, PrimitiveType, RecordRow, StmtKind, Type, TypeId,
    TypeKind,
};

use super::{subst::Subst, TypeChecker};

impl TypeChecker {
    /// Algorithm W: infer the type of an expression
    ///
    /// Returns (substitution, inferred_type_id)
    ///
    /// This works directly on IR expressions. The IR expression's `ty` field
    /// may be `TypeRef::Unknown` when we start; after inference we update it
    /// to `TypeRef::Known(inferred_type_id)`.
    pub fn infer(&mut self, expr_id: ExprId) -> Result<(Subst, TypeId), CompileError> {
        // Check cache first
        if let Some(&cached_ty) = self.infer_cache.get(&expr_id) {
            let resolved_ty = self.global_subst.apply(cached_ty, &mut self.ir);
            return Ok((Subst::default(), resolved_ty));
        }

        let expr = self.ir.exprs.get(expr_id);
        let expr_kind = expr.kind.clone();
        let loc = expr.loc.clone();

        let result = match &expr_kind {
            ExprKind::Unit => {
                let ty = self.ir.types.alloc(Type {
                    loc: loc.clone(),
                    kind: TypeKind::Unit,
                });
                Ok((Subst::default(), ty))
            }

            ExprKind::Literal(lit) => {
                let prim = match lit {
                    Literal::Integer(_) => PrimitiveType::Int,
                    Literal::String(_) => PrimitiveType::String,
                    Literal::Boolean(_) => PrimitiveType::Bool,
                };
                let ty = self.primitive_type(prim, loc.clone());
                Ok((Subst::default(), ty))
            }

            ExprKind::Identifier(ident) => {
                match ident {
                    Ident::Resolved(def_id) => {
                        // Look up the polytype from the environment
                        let poly = self
                            .env
                            .lookup(*def_id)
                            .ok_or_else(|| {
                                let def = self.gcx.definitions.get(*def_id);
                                CompileError::new(
                                    CompileErrorKind::UndefinedVariable { name: def.name },
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

                        // For monomorphic types, apply local_subst
                        let ty = if poly.forall.is_empty() {
                            self.local_subst.apply(ty, &mut self.ir)
                        } else {
                            ty
                        };

                        Ok((Subst::default(), ty))
                    }
                    Ident::Unresolved(path) => {
                        // Should not happen after resolution
                        Err(CompileError::new(
                            CompileErrorKind::Runtime(format!("Unresolved identifier: {:?}", path)),
                            loc,
                        ))
                    }
                }
            }

            ExprKind::List(items) => {
                if items.is_empty() {
                    // Empty list: [α] where α is fresh
                    let elem_ty = self.fresh_type_var(loc.clone());
                    let list_ty = self.list_type(elem_ty, loc.clone());
                    Ok((Subst::default(), list_ty))
                } else {
                    // Infer first element
                    let (mut subst, elem_ty) = self.infer(items[0])?;

                    // Unify with all other elements
                    for &item in &items[1..] {
                        let (s1, item_ty) = self.infer(item)?;
                        subst = subst.compose(&s1, &mut self.ir);

                        let elem_ty_applied = subst.apply(elem_ty, &mut self.ir);
                        let s2 = self.unify(elem_ty_applied, item_ty, loc.clone())?;
                        subst = subst.compose(&s2, &mut self.ir);
                    }

                    let final_elem_ty = subst.apply(elem_ty, &mut self.ir);
                    let list_ty = self.list_type(final_elem_ty, loc.clone());
                    Ok((subst, list_ty))
                }
            }

            ExprKind::Record(fields) => {
                let mut subst = Subst::default();
                let mut field_types = Vec::new();

                for (name, field_expr) in fields {
                    let (s, field_ty) = self.infer(*field_expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                    let field_ty = subst.apply(field_ty, &mut self.ir);
                    field_types.push((*name, field_ty));
                }

                // Convert to RecordRow
                let row = RecordRow::from_fields(field_types);
                let record_ty = self.record_type(row, loc.clone());

                Ok((subst, record_ty))
            }

            ExprKind::Function { params, body } => {
                // Create type variables (or use annotations) for parameters
                let mut param_types = Vec::new();
                let mut local_env = self.env.clone();

                for param in params {
                    let param_ty = if let Some(self_ty) = self.self_type {
                        // Check if this param is named "self"
                        let self_sym = self.gcx.interner.intern("self");
                        if param.name == self_sym {
                            self_ty
                        } else if let Some(annotation_ty) = param.ty {
                            annotation_ty
                        } else {
                            self.fresh_type_var(loc.clone())
                        }
                    } else if let Some(annotation_ty) = param.ty {
                        annotation_ty
                    } else {
                        self.fresh_type_var(loc.clone())
                    };

                    // Add parameter to local type environment (if it has a DefId)
                    if let Some(def_id) = param.def_id {
                        local_env.insert(def_id, Polytype::mono(param_ty));
                    }
                    param_types.push(param_ty);
                }

                // Infer body with extended environment
                let saved_env = std::mem::replace(&mut self.env, local_env);
                let (subst, ret_ty) = self.infer(*body)?;

                // Apply substitution to parameter types
                let param_types: Vec<_> = param_types
                    .into_iter()
                    .map(|ty| subst.apply(ty, &mut self.ir))
                    .collect();

                let func_ty = self.function_type(param_types, ret_ty, loc.clone());

                // Restore environment
                self.env = saved_env;

                Ok((subst, func_ty))
            }

            ExprKind::Application { callee, args } => {
                // Save local_subst
                let saved_local_subst = std::mem::take(&mut self.local_subst);

                // Infer callee type
                let (mut subst, callee_ty) = self.infer(*callee)?;
                self.local_subst = self.local_subst.compose(&subst, &mut self.ir);

                // Infer argument types
                let mut arg_types = Vec::new();
                for arg in args {
                    let arg_expr_id = arg.value();
                    let (s, arg_ty) = self.infer(arg_expr_id)?;
                    subst = subst.compose(&s, &mut self.ir);
                    self.local_subst = self.local_subst.compose(&s, &mut self.ir);
                    let arg_ty = subst.apply(arg_ty, &mut self.ir);
                    arg_types.push(arg_ty);
                }

                // Create fresh return type
                let ret_ty = self.fresh_type_var(loc.clone());

                // Expected function type
                let expected_ty = self.function_type(arg_types, ret_ty, loc.clone());

                // Unify callee with expected function type
                let callee_ty = subst.apply(callee_ty, &mut self.ir);
                let s = self.unify(callee_ty, expected_ty, loc)?;
                subst = subst.compose(&s, &mut self.ir);

                // Restore local_subst
                self.local_subst = saved_local_subst;

                let final_ret_ty = subst.apply(ret_ty, &mut self.ir);
                Ok((subst, final_ret_ty))
            }

            ExprKind::FieldAccess { expr, field } => {
                // Normal field access
                let (mut subst, expr_ty) = self.infer(*expr)?;
                let expr_ty = subst.apply(expr_ty, &mut self.ir);

                let ty = self.ir.types.get(expr_ty);
                match &ty.kind {
                    TypeKind::Record(row) => {
                        if let Some(field_ty) = row.lookup(*field) {
                            Ok((subst, field_ty))
                        } else {
                            // Row might be extensible
                            let field_ty = self.fresh_type_var(loc.clone());
                            let rest_var = self.tvg.fresh();
                            let expected_row = RecordRow::Extend {
                                field: *field,
                                ty: field_ty,
                                rest: Box::new(RecordRow::Var(rest_var)),
                            };
                            let expected_ty = self.record_type(expected_row, loc.clone());
                            let s = self.unify(expr_ty, expected_ty, loc)?;
                            subst = subst.compose(&s, &mut self.ir);
                            Ok((subst, field_ty))
                        }
                    }

                    TypeKind::Named(Ident::Resolved(def_id)) => {
                        // Named type - resolve to underlying record type
                        if let Some(underlying_ty) = self.resolve_named_type(*def_id) {
                            let underlying = self.ir.types.get(underlying_ty);
                            if let TypeKind::Record(row) = &underlying.kind {
                                if let Some(field_ty) = row.lookup(*field) {
                                    return Ok((subst, field_ty));
                                }
                            }
                        }

                        // Fallback: treat as type variable
                        let field_ty = self.fresh_type_var(loc.clone());
                        let rest_var = self.tvg.fresh();
                        let row = RecordRow::Extend {
                            field: *field,
                            ty: field_ty,
                            rest: Box::new(RecordRow::Var(rest_var)),
                        };
                        let record_ty = self.record_type(row, loc.clone());
                        let s = self.unify(expr_ty, record_ty, loc)?;
                        subst = subst.compose(&s, &mut self.ir);
                        Ok((subst, field_ty))
                    }

                    TypeKind::Var(_) => {
                        // Polymorphic access on type variable
                        let field_ty = self.fresh_type_var(loc.clone());
                        let rest_var = self.tvg.fresh();
                        let row = RecordRow::Extend {
                            field: *field,
                            ty: field_ty,
                            rest: Box::new(RecordRow::Var(rest_var)),
                        };
                        let record_ty = self.record_type(row, loc.clone());
                        let s = self.unify(expr_ty, record_ty, loc)?;
                        subst = subst.compose(&s, &mut self.ir);
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

            ExprKind::Block { stmts } => {
                let mut subst = Subst::default();
                let mut block_ty = None;

                let num_stmts = stmts.len();
                for (i, &stmt_id) in stmts.iter().enumerate() {
                    let is_last = i == num_stmts - 1;
                    let stmt = self.ir.stmts.get(stmt_id);

                    if is_last {
                        if let StmtKind::Expr(inner_expr_id) = &stmt.kind {
                            let (s, ty) = self.infer(*inner_expr_id)?;
                            subst = subst.compose(&s, &mut self.ir);
                            block_ty = Some(ty);
                        } else {
                            self.check_stmt(stmt_id)?;
                        }
                    } else {
                        self.check_stmt(stmt_id)?;
                    }
                }

                let final_ty = block_ty.unwrap_or_else(|| {
                    self.ir.types.alloc(Type {
                        loc: loc.clone(),
                        kind: TypeKind::Unit,
                    })
                });
                Ok((subst, final_ty))
            }

            ExprKind::StringInterpolation { parts: _, exprs } => {
                // String interpolation always produces a string
                let mut subst = Subst::default();
                for &expr in exprs {
                    let (s, inferred_ty) = self.infer(expr)?;
                    subst = subst.compose(&s, &mut self.ir);

                    // Record ToString constraint for deferred verification
                    if let Some(to_string_trait) = self.gcx.builtin_traits.to_string {
                        let expr_loc = self.ir.exprs.get(expr).loc.clone();
                        let resolved_ty = subst.apply(inferred_ty, &mut self.ir);
                        self.trait_constraints
                            .push((resolved_ty, to_string_trait, expr_loc));
                    }
                }
                let ty = self.primitive_type(PrimitiveType::String, loc);
                Ok((subst, ty))
            }

            ExprKind::Pipe { lhs, rhs } => {
                // Pipe should have been desugared during resolution
                // But if it reaches here, we can handle it:
                // lhs |> rhs  desugars to  rhs(lhs)
                let (s1, left_ty) = self.infer(*lhs)?;
                let (s2, right_ty) = self.infer(*rhs)?;
                let subst = s1.compose(&s2, &mut self.ir);

                // Create fresh return type
                let ret_ty = self.fresh_type_var(loc.clone());

                // Expected: right should be a function taking left's type
                let expected_fn = self.function_type(vec![left_ty], ret_ty, loc.clone());
                let s3 = self.unify(right_ty, expected_fn, loc)?;
                let subst = subst.compose(&s3, &mut self.ir);

                let final_ret = subst.apply(ret_ty, &mut self.ir);
                Ok((subst, final_ret))
            }
        };

        // Cache the result
        if let Ok((_, ty)) = &result {
            self.infer_cache.insert(expr_id, *ty);
        }

        result
    }
}
