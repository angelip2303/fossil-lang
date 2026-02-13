use crate::ast;
use crate::context::Symbol;
use crate::ir::{
    Argument, Expr, ExprKind, Ident, Ir, RecordFields, Stmt, StmtKind, Type, TypeKind,
    TypeRef,
};

pub fn ast_to_ir(ast: ast::Ast) -> Ir {
    let converter = AstToIrConverter::new();
    converter.convert(ast)
}

struct AstToIrConverter {
    ir: Ir,
}

impl AstToIrConverter {
    fn new() -> Self {
        Self { ir: Ir::default() }
    }

    fn convert(mut self, ast: ast::Ast) -> Ir {
        for &stmt_id in &ast.root {
            let ir_stmt_id = self.convert_stmt(&ast, stmt_id);
            self.ir.root.push(ir_stmt_id);
        }
        self.ir
    }

    fn convert_stmt(&mut self, ast: &ast::Ast, stmt_id: ast::StmtId) -> crate::ir::StmtId {
        let stmt = ast.stmts.get(stmt_id);
        let loc = stmt.loc;

        let kind = match &stmt.kind {
            ast::StmtKind::Let { name, value } => {
                let ir_value = self.convert_expr(ast, *value);
                StmtKind::Let {
                    name: *name,
                    def_id: None,
                    value: ir_value,
                }
            }

            ast::StmtKind::Type {
                name,
                ty,
                attrs,
                ctor_params,
            } => {
                let ir_ty = self.convert_type(ast, *ty);
                let ir_ctor_params = ctor_params
                    .iter()
                    .map(|p| crate::ir::CtorParam {
                        name: p.name,
                        ty: self.convert_type(ast, p.ty),
                    })
                    .collect();
                StmtKind::Type {
                    name: *name,
                    def_id: None,
                    ty: ir_ty,
                    attrs: attrs.clone(),
                    ctor_params: ir_ctor_params,
                }
            }

            ast::StmtKind::Expr(expr) => {
                let ir_expr = self.convert_expr(ast, *expr);
                StmtKind::Expr(ir_expr)
            }
        };

        self.ir.stmts.alloc(Stmt { loc, kind })
    }

    fn convert_expr(&mut self, ast: &ast::Ast, expr_id: ast::ExprId) -> crate::ir::ExprId {
        let expr = ast.exprs.get(expr_id);
        let loc = expr.loc;

        let kind = match &expr.kind {
            ast::ExprKind::Identifier(path) => {
                ExprKind::Identifier(Ident::Unresolved(path.clone()))
            }

            ast::ExprKind::Unit => ExprKind::Unit,

            ast::ExprKind::Literal(lit) => ExprKind::Literal(lit.clone()),

            ast::ExprKind::RecordInstance { type_path, ctor_args, fields } => {
                let ir_ctor_args = self.convert_args(ast, ctor_args);
                let ir_fields = fields
                    .iter()
                    .map(|(name, expr)| (*name, self.convert_expr(ast, *expr)))
                    .collect();
                ExprKind::RecordInstance {
                    type_ident: Ident::Unresolved(type_path.clone()),
                    ctor_args: ir_ctor_args,
                    fields: ir_fields,
                }
            }

            ast::ExprKind::Application { callee, args } => {
                let ir_callee = self.convert_expr(ast, *callee);
                let ir_args = self.convert_args(ast, args);
                ExprKind::Application {
                    callee: ir_callee,
                    args: ir_args,
                }
            }

            ast::ExprKind::Pipe { lhs, rhs } => {
                // Desugar pipe: lhs |> rhs
                let ir_lhs = self.convert_expr(ast, *lhs);

                let rhs_expr = ast.exprs.get(*rhs);
                match &rhs_expr.kind {
                    ast::ExprKind::Application { callee, args } => {
                        // rhs is already f(args...), so result is f(lhs, args...)
                        let ir_callee = self.convert_expr(ast, *callee);
                        let mut new_args = vec![Argument::Positional(ir_lhs)];
                        new_args.extend(self.convert_args(ast, args));
                        ExprKind::Application {
                            callee: ir_callee,
                            args: new_args,
                        }
                    }
                    _ => {
                        // rhs is just f, so result is f(lhs)
                        let ir_rhs = self.convert_expr(ast, *rhs);
                        ExprKind::Application {
                            callee: ir_rhs,
                            args: vec![Argument::Positional(ir_lhs)],
                        }
                    }
                }
            }

            ast::ExprKind::Projection { source, param, output } => {
                let ir_source = self.convert_expr(ast, *source);
                let ir_output = self.convert_expr(ast, *output);
                ExprKind::Projection {
                    source: ir_source,
                    binding: *param,
                    binding_def: None,
                    outputs: vec![ir_output],
                }
            }

            ast::ExprKind::AddOutput { .. } => {
                // Flatten the entire AddOutput chain into a single IR Projection.
                let (original_source, binding, output_exprs) =
                    self.collect_output_chain(ast, expr_id);
                let ir_source = self.convert_expr(ast, original_source);
                let ir_outputs = output_exprs
                    .iter()
                    .map(|&out| self.convert_expr(ast, out))
                    .collect();
                ExprKind::Projection {
                    source: ir_source,
                    binding,
                    binding_def: None,
                    outputs: ir_outputs,
                }
            }

            ast::ExprKind::FieldAccess { expr, field } => {
                let ir_expr = self.convert_expr(ast, *expr);
                ExprKind::FieldAccess {
                    expr: ir_expr,
                    field: *field,
                }
            }

            ast::ExprKind::StringInterpolation { parts, exprs } => {
                let ir_exprs = exprs.iter().map(|&e| self.convert_expr(ast, e)).collect();
                ExprKind::StringInterpolation {
                    parts: parts.clone(),
                    exprs: ir_exprs,
                }
            }

            ast::ExprKind::ProviderInvocation { .. } => {
                unreachable!("ProviderInvocation should be expanded before conversion to IR")
            }
        };

        self.ir.exprs.alloc(Expr {
            loc,
            kind,
            ty: TypeRef::Unknown,
        })
    }

    fn convert_args(&mut self, ast: &ast::Ast, args: &[ast::Argument]) -> Vec<Argument> {
        args.iter()
            .map(|arg| match arg {
                ast::Argument::Positional(e) => Argument::Positional(self.convert_expr(ast, *e)),
                ast::Argument::Named { name, value } => Argument::Named {
                    name: *name,
                    value: self.convert_expr(ast, *value),
                },
            })
            .collect()
    }

    /// Walk back through an AddOutput chain to find the original Projection source,
    /// the shared binding name, and all output expressions in order.
    ///
    /// All lambdas in a `+>` chain must use the same binding name since they all
    /// operate on the same source. Different names would be confusing and produce
    /// undefined variable errors at resolve time.
    fn collect_output_chain(
        &self,
        ast: &ast::Ast,
        start: ast::ExprId,
    ) -> (ast::ExprId, Symbol, Vec<ast::ExprId>) {
        let mut outputs = Vec::new();
        let mut binding = None;
        let mut current = start;

        loop {
            let expr = ast.exprs.get(current);
            match &expr.kind {
                ast::ExprKind::AddOutput { source, param, output } => {
                    binding = Some(*param);
                    outputs.push(*output);
                    current = *source;
                }
                ast::ExprKind::Projection { source, param, output } => {
                    outputs.push(*output);
                    outputs.reverse();
                    return (*source, *param, outputs);
                }
                _ => {
                    outputs.reverse();
                    return (current, binding.unwrap(), outputs);
                }
            }
        }
    }

    fn convert_type(&mut self, ast: &ast::Ast, type_id: ast::TypeId) -> crate::ir::TypeId {
        let ty = ast.types.get(type_id);
        let loc = ty.loc;

        let kind = match &ty.kind {
            ast::TypeKind::Named(path) => TypeKind::Named(Ident::Unresolved(path.clone())),

            ast::TypeKind::Unit => TypeKind::Unit,

            ast::TypeKind::Primitive(prim) => TypeKind::Primitive(*prim),

            ast::TypeKind::Provider { .. } => {
                unreachable!("Providers should be expanded before conversion to IR")
            }

            ast::TypeKind::Optional(inner) => {
                let ir_inner = self.convert_type(ast, *inner);
                TypeKind::Optional(ir_inner)
            }

            ast::TypeKind::Record(fields) => {
                let ir_fields: Vec<_> = fields
                    .iter()
                    .map(|f| (f.name, self.convert_type(ast, f.ty)))
                    .collect();
                TypeKind::Record(RecordFields::from_fields(ir_fields))
            }

        };

        self.ir.types.alloc(Type { loc, kind })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Argument, ExprKind, Ident, Literal, Path, StmtKind, TypeKind, TypeRef};
    use crate::passes::expand::ProviderExpander;
    use crate::passes::parse::Parser;

    fn parse_and_convert(src: &str) -> crate::ir::Ir {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        ast_to_ir(expand_result.ast)
    }

    fn get_let_value_expr<'a>(ir: &'a crate::ir::Ir, root_idx: usize) -> &'a crate::ir::Expr {
        let stmt = ir.stmts.get(ir.root[root_idx]);
        match &stmt.kind {
            StmtKind::Let { value, .. } => ir.exprs.get(*value),
            other => panic!("expected Let stmt at root[{}], got {:?}", root_idx, other),
        }
    }

    fn get_expr_stmt<'a>(ir: &'a crate::ir::Ir, root_idx: usize) -> &'a crate::ir::Expr {
        let stmt = ir.stmts.get(ir.root[root_idx]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => ir.exprs.get(*expr_id),
            other => panic!("expected Expr stmt at root[{}], got {:?}", root_idx, other),
        }
    }

    #[test]
    fn convert_let_stmt() {
        let ir = parse_and_convert("let x = 42");
        assert_eq!(ir.root.len(), 1);
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Let { def_id, value, .. } => {
                assert_eq!(*def_id, None, "def_id should be None after conversion");
                let val_expr = ir.exprs.get(*value);
                assert!(
                    matches!(val_expr.kind, ExprKind::Literal(Literal::Integer(42))),
                    "expected integer literal 42, got {:?}",
                    val_expr.kind
                );
            }
            other => panic!("expected Let stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_type_stmt() {
        let ir = parse_and_convert("type T do Name: string end");
        assert_eq!(ir.root.len(), 1);
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Type {
                ty, ctor_params, ..
            } => {
                assert!(ctor_params.is_empty(), "no ctor params expected");
                let ty_node = ir.types.get(*ty);
                match &ty_node.kind {
                    TypeKind::Record(fields) => {
                        assert_eq!(fields.len(), 1, "expected 1 field in record");
                        let (_, field_ty_id) = &fields.fields[0];
                        let field_ty = ir.types.get(*field_ty_id);
                        assert!(
                            matches!(
                                field_ty.kind,
                                TypeKind::Primitive(crate::ir::PrimitiveType::String)
                            ),
                            "expected string primitive type, got {:?}",
                            field_ty.kind
                        );
                    }
                    other => panic!("expected Record type kind, got {:?}", other),
                }
            }
            other => panic!("expected Type stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_expr_stmt() {
        let ir = parse_and_convert("42");
        assert_eq!(ir.root.len(), 1);
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let expr = ir.exprs.get(*expr_id);
                assert!(
                    matches!(expr.kind, ExprKind::Literal(Literal::Integer(42))),
                    "expected integer literal 42, got {:?}",
                    expr.kind
                );
            }
            other => panic!("expected Expr stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_identifiers_unresolved() {
        let ir = parse_and_convert("let x = 42\nlet y = x");
        assert_eq!(ir.root.len(), 2);
        let val_expr = get_let_value_expr(&ir, 1);
        assert!(
            matches!(&val_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
            "expected Unresolved identifier, got {:?}",
            val_expr.kind
        );
    }

    #[test]
    fn convert_types_unknown() {
        let ir = parse_and_convert("let x = 42");
        let val_expr = get_let_value_expr(&ir, 0);
        assert_eq!(
            val_expr.ty,
            TypeRef::Unknown,
            "expression type should be Unknown after conversion"
        );
    }

    #[test]
    fn pipe_application_prepends_source() {
        // x |> f(1) desugars to f(x, 1) — f is just an identifier, no need to define it
        let ir = parse_and_convert("let x = 42\nlet r = x |> f(1)");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(
                        &callee_expr.kind,
                        ExprKind::Identifier(Ident::Unresolved(_))
                    ),
                    "callee should be unresolved identifier, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(
                    args.len(),
                    2,
                    "expected 2 args (source prepended + original)"
                );
                // First arg is source (x)
                let first_arg = ir.exprs.get(args[0].value());
                assert!(
                    matches!(&first_arg.kind, ExprKind::Identifier(Ident::Unresolved(_))),
                    "first arg should be source identifier, got {:?}",
                    first_arg.kind
                );
                // Second arg is literal 1
                let second_arg = ir.exprs.get(args[1].value());
                assert!(
                    matches!(second_arg.kind, ExprKind::Literal(Literal::Integer(1))),
                    "second arg should be literal 1, got {:?}",
                    second_arg.kind
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_identifier_to_application() {
        // x |> f desugars to f(x)
        let ir = parse_and_convert("let x = 42\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(
                        &callee_expr.kind,
                        ExprKind::Identifier(Ident::Unresolved(_))
                    ),
                    "callee should be unresolved identifier, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 1, "expected 1 arg (just source)");
                let arg_expr = ir.exprs.get(args[0].value());
                assert!(
                    matches!(&arg_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
                    "arg should be source identifier, got {:?}",
                    arg_expr.kind
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_chained() {
        // x |> f |> g desugars to g(f(x))
        let ir = parse_and_convert("let x = 42\nlet y = x |> f |> g");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(
                        &callee_expr.kind,
                        ExprKind::Identifier(Ident::Unresolved(_))
                    ),
                    "outer callee should be g"
                );
                assert_eq!(args.len(), 1, "outer application has 1 arg");
                let inner = ir.exprs.get(args[0].value());
                match &inner.kind {
                    ExprKind::Application {
                        callee: inner_callee,
                        args: inner_args,
                    } => {
                        let inner_callee_expr = ir.exprs.get(*inner_callee);
                        assert!(
                            matches!(
                                &inner_callee_expr.kind,
                                ExprKind::Identifier(Ident::Unresolved(_))
                            ),
                            "inner callee should be f"
                        );
                        assert_eq!(inner_args.len(), 1, "inner application has 1 arg");
                        let x_expr = ir.exprs.get(inner_args[0].value());
                        assert!(
                            matches!(&x_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
                            "innermost arg should be x"
                        );
                    }
                    other => panic!("expected inner Application, got {:?}", other),
                }
            }
            other => panic!("expected outer Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_preserves_named_args() {
        // x |> f(name: 1) desugars to f(x, name: 1)
        let ir = parse_and_convert("let x = 42\nlet y = x |> f(name: 1)");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { args, .. } => {
                assert_eq!(args.len(), 2, "expected 2 args");
                assert!(
                    matches!(&args[0], Argument::Positional(_)),
                    "first arg should be positional (source)"
                );
                assert!(
                    matches!(&args[1], Argument::Named { .. }),
                    "second arg should be named"
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_source_is_positional() {
        let ir = parse_and_convert("let x = 42\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { args, .. } => {
                assert_eq!(args.len(), 1);
                assert!(
                    matches!(&args[0], Argument::Positional(_)),
                    "piped source should be Positional argument"
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_literal_source() {
        // A literal can be the pipe source: 42 |> f
        let ir = parse_and_convert("let y = 42 |> f");
        let val_expr = get_let_value_expr(&ir, 0);
        match &val_expr.kind {
            ExprKind::Application { args, .. } => {
                assert_eq!(args.len(), 1);
                let source_expr = ir.exprs.get(args[0].value());
                assert!(
                    matches!(source_expr.kind, ExprKind::Literal(Literal::Integer(42))),
                    "source should be literal 42, got {:?}",
                    source_expr.kind
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_projection() {
        // x |> fn row -> row end → IR Projection
        let ir = parse_and_convert("let x = 42\nlet y = x |> fn row -> row end");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Projection { outputs, binding_def, .. } => {
                assert_eq!(outputs.len(), 1, "expected 1 output");
                assert_eq!(*binding_def, None, "binding_def should be None after convert");
            }
            other => panic!("expected Projection, got {:?}", other),
        }
    }

    #[test]
    fn add_output_flattens_to_single_projection() {
        // x |> fn row -> row end +> fn row -> row end → single IR Projection with 2 outputs
        let ir = parse_and_convert("let x = 42\nlet y = x |> fn row -> row end +> fn row -> row end");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Projection { outputs, .. } => {
                assert_eq!(outputs.len(), 2, "expected 2 outputs from flattened +> chain");
            }
            other => panic!("expected Projection, got {:?}", other),
        }
    }

    #[test]
    fn triple_add_output_chain() {
        // Three outputs via +> chain → single IR Projection with 3 outputs
        let ir = parse_and_convert(
            "let x = 42\nlet y = x |> fn a -> a end +> fn b -> b end +> fn c -> c end"
        );
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Projection { outputs, .. } => {
                assert_eq!(outputs.len(), 3, "expected 3 outputs from flattened +> chain");
            }
            other => panic!("expected Projection, got {:?}", other),
        }
    }

    #[test]
    fn convert_record_with_ctor_args() {
        // Person(42) { Name = "hi" } → RecordInstance with ctor_args
        let ir = parse_and_convert("type T do Name: string end\nT(42) { Name = \"hi\" }");
        let expr = get_expr_stmt(&ir, 1);
        match &expr.kind {
            ExprKind::RecordInstance { ctor_args, fields, .. } => {
                assert_eq!(ctor_args.len(), 1, "expected 1 ctor arg");
                assert_eq!(fields.len(), 1, "expected 1 field");
            }
            other => panic!("expected RecordInstance, got {:?}", other),
        }
    }

    #[test]
    fn convert_string_interpolation() {
        let ir = parse_and_convert("let name = \"world\"\n\"hello ${name}\"");
        let stmt = ir.stmts.get(ir.root[1]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let expr = ir.exprs.get(*expr_id);
                match &expr.kind {
                    ExprKind::StringInterpolation { parts, exprs } => {
                        assert_eq!(parts.len(), 2, "expected 2 string parts");
                        assert_eq!(exprs.len(), 1, "expected 1 interpolated expr");
                        let interp_expr = ir.exprs.get(exprs[0]);
                        assert!(
                            matches!(
                                &interp_expr.kind,
                                ExprKind::Identifier(Ident::Unresolved(_))
                            ),
                            "interpolated expr should be identifier, got {:?}",
                            interp_expr.kind
                        );
                    }
                    other => panic!("expected StringInterpolation, got {:?}", other),
                }
            }
            other => panic!("expected Expr stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_dot_access_is_qualified_path() {
        // After convert, x.name is a Qualified path — disambiguation happens in resolve
        let ir = parse_and_convert("let x = 42\nlet y = x.name");
        let val_expr = get_let_value_expr(&ir, 1);
        assert!(
            matches!(
                &val_expr.kind,
                ExprKind::Identifier(Ident::Unresolved(Path::Qualified(_)))
            ),
            "x.name should be Qualified path after convert, got {:?}",
            val_expr.kind
        );
    }
}
