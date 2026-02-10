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

            ExprKind::NamedRecordConstruction { type_ident, fields } => {
                // Get the type from the resolved identifier
                let type_def_id = type_ident.def_id();

                // Infer types for all fields
                let mut subst = Subst::default();

                for (_name, field_expr) in fields {
                    let (s, _field_ty) = self.infer(*field_expr)?;
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
                    let param_ty = self.fresh_type_var(loc);

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

#[cfg(test)]
mod tests {
    use crate::ir::{PrimitiveType, StmtKind, TypeKind, TypeRef};

    /// Compile source and return the IrProgram
    fn compile_ok(src: &str) -> crate::passes::IrProgram {
        let parsed = crate::passes::parse::Parser::parse(src, 0).expect("parse failed");
        let expand_result = crate::passes::expand::ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = crate::context::extract_type_metadata(&expand_result.ast);
        let ir = crate::passes::convert::ast_to_ir(expand_result.ast);
        let (ir, gcx) = crate::passes::resolve::IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()
            .expect("resolve failed");
        super::TypeChecker::new(ir, gcx)
            .check()
            .expect("typecheck failed")
    }

    /// Compile and expect a type error
    fn compile_type_err(src: &str) {
        let parsed = crate::passes::parse::Parser::parse(src, 0).expect("parse failed");
        let expand_result = crate::passes::expand::ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = crate::context::extract_type_metadata(&expand_result.ast);
        let ir = crate::passes::convert::ast_to_ir(expand_result.ast);
        let (ir, gcx) = crate::passes::resolve::IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()
            .expect("resolve failed");
        assert!(
            super::TypeChecker::new(ir, gcx).check().is_err(),
            "Expected a type error but compilation succeeded"
        );
    }

    /// Get the type of the value expression of a let binding at a given root stmt index
    fn get_let_value_type(prog: &crate::passes::IrProgram, stmt_idx: usize) -> &TypeKind {
        let stmt_id = prog.ir.root[stmt_idx];
        let stmt = prog.ir.stmts.get(stmt_id);
        if let StmtKind::Let { value, .. } = &stmt.kind {
            let expr = prog.ir.exprs.get(*value);
            if let TypeRef::Known(ty_id) = &expr.ty {
                return &prog.ir.types.get(*ty_id).kind;
            }
        }
        panic!("Expected let with known type at index {}", stmt_idx);
    }

    // ---------------------------------------------------------------
    // Literals
    // ---------------------------------------------------------------

    #[test]
    fn infer_integer_literal() {
        let prog = compile_ok("let x = 42");
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Int)),
            "Expected Primitive(Int), got {:?}",
            ty
        );
    }

    #[test]
    fn infer_string_literal() {
        let prog = compile_ok(r#"let x = "hello""#);
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::String)),
            "Expected Primitive(String), got {:?}",
            ty
        );
    }

    #[test]
    fn infer_bool_literal() {
        let prog = compile_ok("let x = true");
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Bool)),
            "Expected Primitive(Bool), got {:?}",
            ty
        );
    }

    // ---------------------------------------------------------------
    // Variables
    // ---------------------------------------------------------------

    #[test]
    fn infer_variable_reference() {
        let prog = compile_ok("let x = 42\nlet y = x");
        let ty = get_let_value_type(&prog, 1);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Int)),
            "Expected Primitive(Int), got {:?}",
            ty
        );
    }

    #[test]
    fn infer_let_binding_propagates_type() {
        let prog = compile_ok("let x = \"hi\"\nlet y = x");
        let ty = get_let_value_type(&prog, 1);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::String)),
            "Expected Primitive(String), got {:?}",
            ty
        );
    }

    // ---------------------------------------------------------------
    // Lists
    // ---------------------------------------------------------------

    #[test]
    fn infer_homogeneous_list() {
        let prog = compile_ok("let x = [1, 2, 3]");
        let ty = get_let_value_type(&prog, 0);
        match ty {
            TypeKind::List(elem_ty_id) => {
                let elem_ty = &prog.ir.types.get(*elem_ty_id).kind;
                assert!(
                    matches!(elem_ty, TypeKind::Primitive(PrimitiveType::Int)),
                    "Expected list element Primitive(Int), got {:?}",
                    elem_ty
                );
            }
            _ => panic!("Expected List type, got {:?}", ty),
        }
    }

    #[test]
    fn infer_empty_list() {
        let prog = compile_ok("let x = []");
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::List(_)),
            "Expected List type, got {:?}",
            ty
        );
    }

    #[test]
    fn infer_heterogeneous_list_error() {
        compile_type_err("let x = [1, \"hello\"]");
    }

    // ---------------------------------------------------------------
    // Functions
    // ---------------------------------------------------------------

    #[test]
    fn infer_function() {
        let prog = compile_ok("let f = fn(x) -> x");
        let ty = get_let_value_type(&prog, 0);
        match ty {
            TypeKind::Function(params, _ret) => {
                assert_eq!(params.len(), 1, "Expected 1 parameter, got {}", params.len());
            }
            _ => panic!("Expected Function type, got {:?}", ty),
        }
    }

    #[test]
    fn infer_function_application() {
        let prog = compile_ok("let f = fn(x) -> x\nlet y = f(42)");
        let ty = get_let_value_type(&prog, 1);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Int)),
            "Expected Primitive(Int), got {:?}",
            ty
        );
    }

    #[test]
    fn infer_function_application_arity_error() {
        compile_type_err("let f = fn(x) -> x\nf(1, 2)");
    }

    // ---------------------------------------------------------------
    // Records and Fields
    // ---------------------------------------------------------------

    #[test]
    fn infer_record_construction() {
        let prog = compile_ok("type T = { Name: string }\nlet x = T { Name = \"hi\" }");
        // stmt 0 is the type, stmt 1 is the let
        let ty = get_let_value_type(&prog, 1);
        assert!(
            matches!(ty, TypeKind::Named(_)),
            "Expected Named type, got {:?}",
            ty
        );
    }

    #[test]
    fn infer_field_access() {
        let prog = compile_ok(
            "type T = { Name: string }\nlet x = T { Name = \"hi\" }\nlet y = x.Name",
        );
        // stmt 0 = type, stmt 1 = let x, stmt 2 = let y
        let ty = get_let_value_type(&prog, 2);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::String)),
            "Expected Primitive(String), got {:?}",
            ty
        );
    }

    #[test]
    fn infer_field_not_found_error() {
        compile_type_err(
            "type T = { Name: string }\nlet x = T { Name = \"hi\" }\nlet y = x.Age",
        );
    }

    // ---------------------------------------------------------------
    // Blocks
    // ---------------------------------------------------------------

    #[test]
    fn infer_block_returns_last() {
        let prog = compile_ok("let x = {\n  let y = 42\n  y\n}");
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Int)),
            "Expected Primitive(Int), got {:?}",
            ty
        );
    }

    // ---------------------------------------------------------------
    // String interpolation
    // ---------------------------------------------------------------

    #[test]
    fn infer_string_interpolation() {
        let prog = compile_ok("let name = \"world\"\nlet x = \"hello ${name}\"");
        let ty = get_let_value_type(&prog, 1);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::String)),
            "Expected Primitive(String), got {:?}",
            ty
        );
    }

    // ---------------------------------------------------------------
    // Unit rejection
    // ---------------------------------------------------------------

    #[test]
    fn infer_unit_literal() {
        // Unit is a valid expression; binding it to a let is allowed in the current type system
        let prog = compile_ok("let x = ()");
        let ty = get_let_value_type(&prog, 0);
        assert!(
            matches!(ty, TypeKind::Unit),
            "Expected Unit type, got {:?}",
            ty
        );
    }

    // ---------------------------------------------------------------
    // Additional tests
    // ---------------------------------------------------------------

    #[test]
    fn infer_list_of_strings() {
        let prog = compile_ok(r#"let x = ["a", "b", "c"]"#);
        let ty = get_let_value_type(&prog, 0);
        match ty {
            TypeKind::List(elem_ty_id) => {
                let elem_ty = &prog.ir.types.get(*elem_ty_id).kind;
                assert!(
                    matches!(elem_ty, TypeKind::Primitive(PrimitiveType::String)),
                    "Expected list element Primitive(String), got {:?}",
                    elem_ty
                );
            }
            _ => panic!("Expected List type, got {:?}", ty),
        }
    }

    #[test]
    fn infer_function_identity_param_return_match() {
        // Identity function: param type and return type should unify
        let prog = compile_ok("let f = fn(x) -> x");
        let ty = get_let_value_type(&prog, 0);
        match ty {
            TypeKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                // The param type and return type should be the same type id
                assert_eq!(params[0], *ret, "Identity function param and return should unify");
            }
            _ => panic!("Expected Function type, got {:?}", ty),
        }
    }

    #[test]
    fn infer_nested_field_access() {
        // Access a field on a record obtained from another record's field
        let prog = compile_ok(
            "type Inner = { Value: int }\n\
             type Outer = { Child: Inner }\n\
             let inner = Inner { Value = 42 }\n\
             let outer = Outer { Child = inner }\n\
             let v = outer.Child.Value",
        );
        // stmt 0 = type Inner, 1 = type Outer, 2 = let inner, 3 = let outer, 4 = let v
        let ty = get_let_value_type(&prog, 4);
        assert!(
            matches!(ty, TypeKind::Primitive(PrimitiveType::Int)),
            "Expected Primitive(Int), got {:?}",
            ty
        );
    }
}
