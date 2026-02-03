//! Type inference (Algorithm W)
//!
//! This module contains the core type inference logic implementing Algorithm W
//! from Hindley-Milner type system with support for row polymorphism.
//!
//! The key difference from the previous HIR/THIR-based implementation is that
//! we now work directly on the unified IR, inferring types in-place.

use crate::error::FossilError;
use crate::ir::{
    ExprId, ExprKind, Ident, Literal, Polytype, PrimitiveType, StmtKind, Type, TypeId, TypeKind,
};

use super::{TypeChecker, subst::Subst};

impl TypeChecker {
    /// Algorithm W: infer the type of an expression
    ///
    /// Returns (substitution, inferred_type_id)
    ///
    /// This works directly on IR expressions. The IR expression's `ty` field
    /// may be `TypeRef::Unknown` when we start; after inference we update it
    /// to `TypeRef::Known(inferred_type_id)`.
    pub fn infer(&mut self, expr_id: ExprId) -> Result<(Subst, TypeId), FossilError> {
        // Check cache first
        if let Some(&cached_ty) = self.infer_cache.get(&expr_id) {
            let resolved_ty = self.global_subst.apply(cached_ty, &mut self.ir);
            return Ok((Subst::default(), resolved_ty));
        }

        let expr = self.ir.exprs.get(expr_id);
        let expr_kind = expr.kind.clone();
        let loc = expr.loc;

        let result = match &expr_kind {
            ExprKind::Unit => {
                let ty = self.ir.types.alloc(Type {
                    loc,
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
                let ty = self.ir.types.alloc(Type {
                    loc,
                    kind: TypeKind::Primitive(prim),
                });
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
                                let name_str = self.gcx.interner.resolve(def.name).to_string();
                                FossilError::undefined_variable(name_str, loc)
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
                        // Should not happen after resolution - this is an internal error
                        Err(FossilError::internal(
                            "typecheck",
                            format!("Unresolved identifier reached type checker: {:?}", path),
                            loc,
                        ))
                    }
                }
            }

            ExprKind::List(items) => {
                if items.is_empty() {
                    // Empty list: [α] where α is fresh
                    let elem_ty = self.fresh_type_var(loc);
                    let list_ty = self.ir.list_type(elem_ty);
                    Ok((Subst::default(), list_ty))
                } else {
                    // Infer first element
                    let (mut subst, elem_ty) = self.infer(items[0])?;

                    // Unify with all other elements
                    for &item in &items[1..] {
                        let (s1, item_ty) = self.infer(item)?;
                        subst = subst.compose(&s1, &mut self.ir);

                        let elem_ty_applied = subst.apply(elem_ty, &mut self.ir);
                        let s2 = self.unify(elem_ty_applied, item_ty, loc)?;
                        subst = subst.compose(&s2, &mut self.ir);
                    }

                    let final_elem_ty = subst.apply(elem_ty, &mut self.ir);
                    let list_ty = self.ir.list_type(final_elem_ty);
                    Ok((subst, list_ty))
                }
            }

            ExprKind::NamedRecordConstruction {
                type_ident,
                fields,
                meta_fields,
            } => {
                // Get the type from the resolved identifier
                let type_def_id = type_ident.def_id();

                // Infer types for all fields
                let mut subst = Subst::default();

                for (_name, field_expr) in fields {
                    let (s, _field_ty) = self.infer(*field_expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                // Infer meta-field expressions
                for (_name, meta_expr) in meta_fields {
                    let (s, _) = self.infer(*meta_expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                // The type is the named type from the identifier
                let named_ty = self.ir.named_type(type_def_id);

                Ok((subst, named_ty))
            }

            ExprKind::Function { params, body, .. } => {
                let mut param_types = Vec::new();
                let mut local_env = self.env.clone();

                for param in params {
                    let param_ty = if let Some(annotation_ty) = param.ty {
                        annotation_ty
                    } else {
                        self.fresh_type_var(loc)
                    };

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

                let func_ty = self.ir.fn_type(param_types, ret_ty);

                // Restore environment
                self.env = saved_env;

                Ok((subst, func_ty))
            }

            ExprKind::Application { callee, args } => {
                // Save local_subst
                let saved_local_subst = std::mem::take(&mut self.local_subst);

                // Check if callee is a variadic function
                let callee_expr = self.ir.exprs.get(*callee);
                let is_variadic =
                    if let ExprKind::Identifier(Ident::Resolved(def_id)) = &callee_expr.kind {
                        self.gcx.is_variadic(*def_id)
                    } else {
                        false
                    };

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
                let ret_ty = self.fresh_type_var(loc);

                // For variadic functions, only unify with declared parameters
                let callee_ty_applied = subst.apply(callee_ty, &mut self.ir);
                let callee_type_kind = self.ir.types.get(callee_ty_applied).kind.clone();

                let args_for_unification = if is_variadic {
                    // Get the number of declared parameters from the function type
                    if let TypeKind::Function(params, _) = &callee_type_kind {
                        // Only use first N argument types for unification
                        arg_types.iter().take(params.len()).cloned().collect()
                    } else {
                        arg_types.clone()
                    }
                } else {
                    arg_types.clone()
                };

                // Expected function type (with possibly fewer args for variadic)
                let expected_ty = self.ir.fn_type(args_for_unification, ret_ty);

                // Unify callee with expected function type
                let s = self.unify(callee_ty_applied, expected_ty, loc)?;
                subst = subst.compose(&s, &mut self.ir);

                // Restore local_subst
                self.local_subst = saved_local_subst;

                let final_ret_ty = subst.apply(ret_ty, &mut self.ir);
                Ok((subst, final_ret_ty))
            }

            ExprKind::FieldAccess { expr, field } => {
                // Normal field access
                let (subst, expr_ty) = self.infer(*expr)?;
                let expr_ty = subst.apply(expr_ty, &mut self.ir);

                let ty = self.ir.types.get(expr_ty);
                match &ty.kind {
                    TypeKind::Record(fields) => {
                        if let Some(field_ty) = fields.lookup(*field) {
                            Ok((subst, field_ty))
                        } else {
                            // Field not found - error (no row polymorphism)
                            let field_str = self.gcx.interner.resolve(*field).to_string();
                            Err(FossilError::field_not_found(field_str, loc))
                        }
                    }

                    TypeKind::Named(Ident::Resolved(def_id)) => {
                        // Named type - resolve to underlying record type
                        if let Some(underlying_ty) = self.resolve_named_type(*def_id)
                            && let TypeKind::Record(fields) =
                                &self.ir.types.get(underlying_ty).kind
                            && let Some(field_ty) = fields.lookup(*field)
                        {
                            return Ok((subst, field_ty));
                        }

                        // Field not found - error
                        let field_str = self.gcx.interner.resolve(*field).to_string();
                        Err(FossilError::field_not_found(field_str, loc))
                    }

                    TypeKind::Var(_) => {
                        // Type variable - create a fresh type for the field
                        // (simplified: just return a fresh type var without constraining the record)
                        let field_ty = self.fresh_type_var(loc);
                        Ok((subst, field_ty))
                    }

                    _ => {
                        let field_str = self.gcx.interner.resolve(*field).to_string();
                        Err(FossilError::field_not_found(field_str, loc))
                    }
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
                        loc,
                        kind: TypeKind::Unit,
                    })
                });
                Ok((subst, final_ty))
            }

            ExprKind::StringInterpolation { parts: _, exprs } => {
                // String interpolation always produces a string
                // All types are serializable via Polars - no trait check needed
                let mut subst = Subst::default();
                for &expr in exprs {
                    let (s, _) = self.infer(expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                }
                let ty = self.ir.string_type();
                Ok((subst, ty))
            }
        };

        // Cache the result
        if let Ok((_, ty)) = &result {
            self.infer_cache.insert(expr_id, *ty);
        }

        result
    }
}
