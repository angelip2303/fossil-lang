use crate::error::FossilError;
use crate::ir::{
    ExprId, ExprKind, Literal, Polytype, PrimitiveType, Type, TypeId, TypeKind,
};

use super::{TypeChecker, typeutil::Subst};

impl TypeChecker {
    pub fn infer(&mut self, expr_id: ExprId) -> Result<(Subst, TypeId), FossilError> {
        let expr_id = self.resolutions.expr_rewrites.get(&expr_id).copied().unwrap_or(expr_id);

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

            ExprKind::Identifier(_) => {
                let def_id = self.resolutions.expr_defs.get(&expr_id).ok_or_else(|| {
                    FossilError::internal("typecheck", "Unresolved identifier reached type checker", loc)
                })?;

                let poly = self
                    .env
                    .lookup(*def_id)
                    .ok_or_else(|| {
                        let def = self.gcx.definitions.get(*def_id);
                        let name_str = self.gcx.interner.resolve(def.name).to_string();
                        FossilError::undefined_variable(name_str, loc)
                    })?
                    .clone();

                let ty = self.instantiate(&poly);
                let ty = if poly.forall.is_empty() {
                    self.local_subst.apply(ty, &mut self.ir)
                } else {
                    ty
                };

                Ok((Subst::default(), ty))
            }

            ExprKind::RecordInstance { ctor_args, fields, .. } => {
                let type_def_id = *self.resolutions.expr_defs.get(&expr_id).ok_or_else(|| {
                    FossilError::internal("typecheck", "Unresolved type in record instance", loc)
                })?;

                let mut subst = Subst::default();

                for arg in ctor_args {
                    let (s, _arg_ty) = self.infer(arg.value())?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                for (_name, field_expr) in fields {
                    let (s, _field_ty) = self.infer(*field_expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                self.check_ctor_arg_count(type_def_id, ctor_args.len(), loc)?;

                let named_ty = self.ir.named_type(type_def_id);
                Ok((subst, named_ty))
            }

            ExprKind::Projection { source, outputs, .. } => {
                let (mut subst, source_ty) = self.infer(*source)?;

                let binding_ty = subst.apply(source_ty, &mut self.ir);

                if let Some(&binding_def_id) = self.resolutions.expr_defs.get(&expr_id) {
                    self.env.insert(binding_def_id, Polytype::mono(binding_ty));
                }

                let mut last_ty = self.fresh_type_var(loc);
                for &output in outputs {
                    let (s, ty) = self.infer(output)?;
                    subst = subst.compose(&s, &mut self.ir);
                    last_ty = ty;
                }

                Ok((subst, last_ty))
            }

            ExprKind::Application { callee, args } => {
                let saved_local_subst = std::mem::take(&mut self.local_subst);

                let (mut subst, callee_ty) = self.infer(*callee)?;
                self.local_subst = self.local_subst.compose(&subst, &mut self.ir);

                let mut arg_types = Vec::new();
                for arg in args {
                    let arg_expr_id = arg.value();
                    let (s, arg_ty) = self.infer(arg_expr_id)?;
                    subst = subst.compose(&s, &mut self.ir);
                    self.local_subst = self.local_subst.compose(&s, &mut self.ir);
                    let arg_ty = subst.apply(arg_ty, &mut self.ir);
                    arg_types.push(arg_ty);
                }

                let ret_ty = self.fresh_type_var(loc);
                let callee_ty_applied = subst.apply(callee_ty, &mut self.ir);
                let expected_ty = self.ir.fn_type(arg_types, ret_ty);

                let s = self.unify(callee_ty_applied, expected_ty, loc)?;
                subst = subst.compose(&s, &mut self.ir);

                self.local_subst = saved_local_subst;

                let final_ret_ty = subst.apply(ret_ty, &mut self.ir);
                Ok((subst, final_ret_ty))
            }

            ExprKind::FieldAccess { expr, field } => {
                let (subst, expr_ty) = self.infer(*expr)?;
                let expr_ty = subst.apply(expr_ty, &mut self.ir);

                if let Some(def_id) = self.named_def_id(expr_ty) {
                    if let Some(underlying_ty) = self.resolve_named_type(def_id)
                        && let TypeKind::Record(fields) = &self.ir.types.get(underlying_ty).kind
                        && let Some(field_ty) = fields.lookup(*field)
                    {
                        return Ok((subst, field_ty));
                    }
                    let field_str = self.gcx.interner.resolve(*field).to_string();
                    return Err(FossilError::field_not_found(field_str, loc));
                }

                let ty = self.ir.types.get(expr_ty);
                match &ty.kind {
                    TypeKind::Record(fields) => {
                        if let Some(field_ty) = fields.lookup(*field) {
                            Ok((subst, field_ty))
                        } else {
                            let field_str = self.gcx.interner.resolve(*field).to_string();
                            Err(FossilError::field_not_found(field_str, loc))
                        }
                    }

                    TypeKind::Var(_) => {
                        let field_ty = self.fresh_type_var(loc);
                        Ok((subst, field_ty))
                    }

                    _ => {
                        let field_str = self.gcx.interner.resolve(*field).to_string();
                        Err(FossilError::field_not_found(field_str, loc))
                    }
                }
            }

            ExprKind::StringInterpolation { parts: _, exprs } => {
                let mut subst = Subst::default();
                for &expr in exprs {
                    let (s, _) = self.infer(expr)?;
                    subst = subst.compose(&s, &mut self.ir);
                }
                let ty = self.ir.string_type();
                Ok((subst, ty))
            }
        };

        if let Ok((_, ty)) = &result {
            self.infer_cache.insert(expr_id, *ty);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::{PrimitiveType, StmtKind, TypeKind};

    fn compile_ok(src: &str) -> crate::passes::IrProgram {
        let parsed = crate::passes::parse::Parser::parse(src, 0).expect("parse failed");
        let expand_result = crate::passes::expand::ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = crate::context::extract_type_metadata(&expand_result.ast);
        let ir = crate::passes::convert::ast_to_ir(expand_result.ast);
        let (ir, gcx, resolutions) = crate::passes::resolve::IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()
            .expect("resolve failed");
        super::TypeChecker::new(ir, gcx, resolutions)
            .check()
            .expect("typecheck failed")
    }

    fn get_let_value_type(prog: &crate::passes::IrProgram, stmt_idx: usize) -> &TypeKind {
        let stmt_id = prog.ir.root[stmt_idx];
        let stmt = prog.ir.stmts.get(stmt_id);
        if let StmtKind::Let { value, .. } = &stmt.kind {
            if let Some(&ty_id) = prog.typeck_results.expr_types.get(value) {
                return &prog.ir.types.get(ty_id).kind;
            }
        }
        panic!("Expected let with known type at index {}", stmt_idx);
    }

    #[test]
    fn infer_nested_field_access() {
        let prog = compile_ok(
            "type Inner do Value: int end\n\
             type Outer do Child: Inner end\n\
             let inner = Inner { Value = 42 }\n\
             let outer = Outer { Child = inner }\n\
             let v = outer.Child.Value",
        );
        let ty = get_let_value_type(&prog, 4);
        assert!(matches!(ty, TypeKind::Primitive(PrimitiveType::Int)));
    }
}
