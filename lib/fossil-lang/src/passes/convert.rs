use crate::ast;
use crate::ir::{
    Argument, Expr, ExprKind, Ir, RecordFields, Stmt, StmtKind, Type, TypeKind,
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
            ast::ExprKind::Identifier(path) => ExprKind::Identifier(path.clone()),

            ast::ExprKind::Unit => ExprKind::Unit,

            ast::ExprKind::Literal(lit) => ExprKind::Literal(lit.clone()),

            ast::ExprKind::RecordInstance { type_path, ctor_args, fields } => {
                let ir_ctor_args = self.convert_args(ast, ctor_args);
                let ir_fields = fields
                    .iter()
                    .map(|(name, expr)| (*name, self.convert_expr(ast, *expr)))
                    .collect();
                ExprKind::RecordInstance {
                    type_name: type_path.clone(),
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

            ast::ExprKind::Projection { source, param, outputs } => {
                let ir_source = self.convert_expr(ast, *source);
                let ir_outputs = outputs
                    .iter()
                    .map(|&out| self.convert_expr(ast, out))
                    .collect();
                ExprKind::Projection {
                    source: ir_source,
                    binding: *param,
                    outputs: ir_outputs,
                }
            }

            ast::ExprKind::Join { left, right, left_on, right_on, how, suffix } => {
                let ir_left = self.convert_expr(ast, *left);
                let ir_right = self.convert_expr(ast, *right);
                ExprKind::Join {
                    left: ir_left,
                    right: ir_right,
                    left_on: left_on.clone(),
                    right_on: right_on.clone(),
                    how: *how,
                    suffix: *suffix,
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

            ast::ExprKind::Reference { type_path, ctor_args } => {
                let ir_ctor_args = self.convert_args(ast, ctor_args);
                ExprKind::Reference {
                    type_name: type_path.clone(),
                    ctor_args: ir_ctor_args,
                }
            }

            ast::ExprKind::ProviderInvocation { .. } => {
                unreachable!("ProviderInvocation should be expanded before conversion to IR")
            }
        };

        self.ir.exprs.alloc(Expr { loc, kind })
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

    fn convert_type(&mut self, ast: &ast::Ast, type_id: ast::TypeId) -> crate::ir::TypeId {
        let ty = ast.types.get(type_id);
        let loc = ty.loc;

        let kind = match &ty.kind {
            ast::TypeKind::Named(path) => TypeKind::Unresolved(path.clone()),

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
    use crate::ir::{Argument, ExprKind, Literal, Path, StmtKind, TypeKind};
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
            StmtKind::Let { value, .. } => {
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
            matches!(&val_expr.kind, ExprKind::Identifier(Path::Simple(_))),
            "expected Simple path identifier, got {:?}",
            val_expr.kind
        );
    }

    #[test]
    fn pipe_application_prepends_source() {
        let ir = parse_and_convert("let x = 42\nlet r = x |> f(1)");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                    "callee should be simple path, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 2, "expected 2 args (source prepended + original)");
                let first_arg = ir.exprs.get(args[0].value());
                assert!(
                    matches!(&first_arg.kind, ExprKind::Identifier(Path::Simple(_))),
                    "first arg should be source identifier, got {:?}",
                    first_arg.kind
                );
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
        let ir = parse_and_convert("let x = 42\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                    "callee should be simple path, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 1, "expected 1 arg (just source)");
                let arg_expr = ir.exprs.get(args[0].value());
                assert!(
                    matches!(&arg_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                    "arg should be source identifier, got {:?}",
                    arg_expr.kind
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_chained() {
        let ir = parse_and_convert("let x = 42\nlet y = x |> f |> g");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
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
                            matches!(&inner_callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                            "inner callee should be f"
                        );
                        assert_eq!(inner_args.len(), 1, "inner application has 1 arg");
                        let x_expr = ir.exprs.get(inner_args[0].value());
                        assert!(
                            matches!(&x_expr.kind, ExprKind::Identifier(Path::Simple(_))),
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
        let ir = parse_and_convert("let x = 42\nlet y = x |> each row -> row");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::Projection { outputs, .. } => {
                assert_eq!(outputs.len(), 1, "expected 1 output");
            }
            other => panic!("expected Projection, got {:?}", other),
        }
    }

    #[test]
    fn add_output_flattens_to_single_projection() {
        let ir = parse_and_convert("let x = 42\nlet y = x |> each row -> row +> each row -> row");
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
        let ir = parse_and_convert(
            "let x = 42\nlet y = x |> each a -> a +> each b -> b +> each c -> c"
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
    fn join_basic() {
        let ir = parse_and_convert(
            "let a = 1\nlet b = 2\nlet c = a |> join b on X = Y"
        );
        let val = get_let_value_expr(&ir, 2);
        assert!(matches!(&val.kind, ExprKind::Join { .. }));
    }

    #[test]
    fn left_join_basic() {
        let ir = parse_and_convert(
            "let a = 1\nlet b = 2\nlet c = a |> left_join b on X = Y"
        );
        let val = get_let_value_expr(&ir, 2);
        match &val.kind {
            ExprKind::Join { how, .. } => {
                assert_eq!(*how, crate::common::JoinHow::Left);
            }
            other => panic!("expected Join, got {:?}", other),
        }
    }

    #[test]
    fn join_multi_column() {
        let ir = parse_and_convert(
            "let a = 1\nlet b = 2\nlet c = a |> join b on (X, Y) = (A, B)"
        );
        let val = get_let_value_expr(&ir, 2);
        match &val.kind {
            ExprKind::Join { left_on, right_on, .. } => {
                assert_eq!(left_on.len(), 2);
                assert_eq!(right_on.len(), 2);
            }
            other => panic!("expected Join, got {:?}", other),
        }
    }

    #[test]
    fn join_with_suffix() {
        let ir = parse_and_convert(
            "let a = 1\nlet b = 2\nlet c = a |> join b on X = Y suffix \"_room\""
        );
        let val = get_let_value_expr(&ir, 2);
        match &val.kind {
            ExprKind::Join { suffix, .. } => {
                assert!(suffix.is_some(), "suffix should be present");
            }
            other => panic!("expected Join, got {:?}", other),
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
                            matches!(&interp_expr.kind, ExprKind::Identifier(Path::Simple(_))),
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
        let ir = parse_and_convert("let x = 42\nlet y = x.name");
        let val_expr = get_let_value_expr(&ir, 1);
        assert!(
            matches!(
                &val_expr.kind,
                ExprKind::Identifier(Path::Qualified(_))
            ),
            "x.name should be Qualified path after convert, got {:?}",
            val_expr.kind
        );
    }
}
