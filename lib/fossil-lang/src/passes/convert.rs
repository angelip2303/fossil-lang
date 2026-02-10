//! Conversion from AST to IR
//!
//! This module handles the conversion from the parsed AST to the unified IR.
//! After conversion, identifiers are Ident::Unresolved and types are TypeRef::Unknown.

use crate::ast::ast;
use crate::ir::{
    Argument, Expr, ExprKind, Ident, Ir, Literal, Param, Path, PrimitiveType,
    ProviderArgument, RecordFields, Stmt, StmtKind, Type, TypeKind, TypeRef,
};

/// Convert AST to IR
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
        // Convert all statements
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

            ast::StmtKind::Const { name, value } => {
                let ir_value = self.convert_expr(ast, *value);
                StmtKind::Const {
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
                ExprKind::Identifier(Ident::Unresolved(convert_path(path)))
            }

            ast::ExprKind::Unit => ExprKind::Unit,

            ast::ExprKind::Literal(lit) => ExprKind::Literal(convert_literal(lit)),

            ast::ExprKind::List(items) => {
                let ir_items = items.iter().map(|&e| self.convert_expr(ast, e)).collect();
                ExprKind::List(ir_items)
            }

            ast::ExprKind::NamedRecordConstruction { type_path, fields } => {
                let ir_fields = fields
                    .iter()
                    .map(|(name, expr)| (*name, self.convert_expr(ast, *expr)))
                    .collect();
                ExprKind::NamedRecordConstruction {
                    type_ident: Ident::Unresolved(convert_path(type_path)),
                    fields: ir_fields,
                }
            }

            ast::ExprKind::Function {
                params,
                body,
                attrs,
            } => {
                let ir_params = params
                    .iter()
                    .map(|p| Param {
                        name: p.name,
                        def_id: None,
                        default: p.default.map(|e| self.convert_expr(ast, e)),
                    })
                    .collect();
                let ir_body = self.convert_expr(ast, *body);
                ExprKind::Function {
                    params: ir_params,
                    body: ir_body,
                    attrs: attrs.clone(),
                }
            }

            ast::ExprKind::Application { callee, args } => {
                let ir_callee = self.convert_expr(ast, *callee);
                let ir_args = args
                    .iter()
                    .map(|arg| match arg {
                        ast::Argument::Positional(e) => {
                            Argument::Positional(self.convert_expr(ast, *e))
                        }
                        ast::Argument::Named { name, value } => Argument::Named {
                            name: *name,
                            value: self.convert_expr(ast, *value),
                        },
                    })
                    .collect();
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
                        new_args.extend(args.iter().map(|arg| match arg {
                            ast::Argument::Positional(e) => {
                                Argument::Positional(self.convert_expr(ast, *e))
                            }
                            ast::Argument::Named { name, value } => Argument::Named {
                                name: *name,
                                value: self.convert_expr(ast, *value),
                            },
                        }));
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

            ast::ExprKind::FieldAccess { expr, field } => {
                let ir_expr = self.convert_expr(ast, *expr);
                ExprKind::FieldAccess {
                    expr: ir_expr,
                    field: *field,
                }
            }

            ast::ExprKind::Block { stmts } => {
                let ir_stmts = stmts.iter().map(|&s| self.convert_stmt(ast, s)).collect();
                ExprKind::Block { stmts: ir_stmts }
            }

            ast::ExprKind::StringInterpolation { parts, exprs } => {
                let ir_exprs = exprs.iter().map(|&e| self.convert_expr(ast, e)).collect();
                ExprKind::StringInterpolation {
                    parts: parts.clone(),
                    exprs: ir_exprs,
                }
            }
        };

        self.ir.exprs.alloc(Expr {
            loc,
            kind,
            ty: TypeRef::Unknown,
        })
    }

    fn convert_type(&mut self, ast: &ast::Ast, type_id: ast::TypeId) -> crate::ir::TypeId {
        let ty = ast.types.get(type_id);
        let loc = ty.loc;

        let kind = match &ty.kind {
            ast::TypeKind::Named(path) => TypeKind::Named(Ident::Unresolved(convert_path(path))),

            ast::TypeKind::Unit => TypeKind::Unit,

            ast::TypeKind::Primitive(prim) => TypeKind::Primitive(convert_primitive(prim)),

            ast::TypeKind::Provider { provider, args } => {
                let ir_args = args
                    .iter()
                    .map(|arg| match arg {
                        ast::ProviderArgument::Positional(lit) => {
                            ProviderArgument::Positional(convert_literal(lit))
                        }
                        ast::ProviderArgument::Named { name, value } => ProviderArgument::Named {
                            name: *name,
                            value: convert_literal(value),
                        },
                    })
                    .collect();
                TypeKind::Provider {
                    provider: Ident::Unresolved(convert_path(provider)),
                    args: ir_args,
                }
            }

            ast::TypeKind::Function(params, ret) => {
                let ir_params = params.iter().map(|&t| self.convert_type(ast, t)).collect();
                let ir_ret = self.convert_type(ast, *ret);
                TypeKind::Function(ir_params, ir_ret)
            }

            ast::TypeKind::List(inner) => {
                let ir_inner = self.convert_type(ast, *inner);
                TypeKind::List(ir_inner)
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

            ast::TypeKind::App { ctor, args } => {
                let ir_args = args.iter().map(|&t| self.convert_type(ast, t)).collect();
                TypeKind::App {
                    ctor: Ident::Unresolved(convert_path(ctor)),
                    args: ir_args,
                }
            }
        };

        self.ir.types.alloc(Type { loc, kind })
    }

}

fn convert_path(path: &ast::Path) -> Path {
    match path {
        ast::Path::Simple(sym) => Path::Simple(*sym),
        ast::Path::Qualified(parts) => Path::Qualified(parts.clone()),
        ast::Path::Relative { dots, components } => Path::Relative {
            dots: *dots,
            components: components.clone(),
        },
    }
}

fn convert_literal(lit: &ast::Literal) -> Literal {
    match lit {
        ast::Literal::Integer(i) => Literal::Integer(*i),
        ast::Literal::String(s) => Literal::String(*s),
        ast::Literal::Boolean(b) => Literal::Boolean(*b),
    }
}

fn convert_primitive(prim: &ast::PrimitiveType) -> PrimitiveType {
    match prim {
        ast::PrimitiveType::Unit => PrimitiveType::Unit,
        ast::PrimitiveType::Int => PrimitiveType::Int,
        ast::PrimitiveType::Float => PrimitiveType::Float,
        ast::PrimitiveType::String => PrimitiveType::String,
        ast::PrimitiveType::Bool => PrimitiveType::Bool,
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
        let expand_result =
            ProviderExpander::new((parsed.ast, parsed.gcx)).expand().expect("expand failed");
        ast_to_ir(expand_result.ast)
    }

    /// Helper: get the expression from a let statement at a given root index.
    fn get_let_value_expr<'a>(ir: &'a crate::ir::Ir, root_idx: usize) -> &'a crate::ir::Expr {
        let stmt = ir.stmts.get(ir.root[root_idx]);
        match &stmt.kind {
            StmtKind::Let { value, .. } => ir.exprs.get(*value),
            StmtKind::Const { value, .. } => ir.exprs.get(*value),
            other => panic!("expected Let or Const stmt at root[{}], got {:?}", root_idx, other),
        }
    }

    // ---------------------------------------------------------------
    // Basic conversions
    // ---------------------------------------------------------------

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
        let ir = parse_and_convert("type T = { Name: string }");
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

    // ---------------------------------------------------------------
    // Pipe desugaring
    // ---------------------------------------------------------------

    #[test]
    fn pipe_application_prepends_source() {
        // x |> f(1) desugars to f(x, 1)
        let ir = parse_and_convert("let x = 42\nlet f = fn(a, b) -> a\nlet y = x |> f(1)");
        let val_expr = get_let_value_expr(&ir, 2);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
                    "callee should be unresolved identifier, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 2, "expected 2 args (source prepended + original)");
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
        let ir = parse_and_convert("let x = 42\nlet f = fn(a) -> a\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 2);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
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
        let ir = parse_and_convert(
            "let x = 42\nlet f = fn(a) -> a\nlet g = fn(a) -> a\nlet y = x |> f |> g",
        );
        let val_expr = get_let_value_expr(&ir, 3);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
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
        let ir = parse_and_convert("let x = 42\nlet f = fn(a, b) -> a\nlet y = x |> f(name: 1)");
        let val_expr = get_let_value_expr(&ir, 2);
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
        let ir = parse_and_convert("let x = 42\nlet f = fn(a) -> a\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 2);
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
        let ir = parse_and_convert("let f = fn(a) -> a\nlet y = 42 |> f");
        let val_expr = get_let_value_expr(&ir, 1);
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

    // ---------------------------------------------------------------
    // Other conversions
    // ---------------------------------------------------------------

    #[test]
    fn convert_list() {
        let ir = parse_and_convert("[1, 2, 3]");
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let expr = ir.exprs.get(*expr_id);
                match &expr.kind {
                    ExprKind::List(items) => {
                        assert_eq!(items.len(), 3, "expected 3 list items");
                        for (i, item_id) in items.iter().enumerate() {
                            let item = ir.exprs.get(*item_id);
                            let expected = i as i64 + 1;
                            assert!(
                                matches!(&item.kind, ExprKind::Literal(Literal::Integer(v)) if *v == expected),
                                "expected integer {}, got {:?}",
                                expected,
                                item.kind
                            );
                        }
                    }
                    other => panic!("expected List, got {:?}", other),
                }
            }
            other => panic!("expected Expr stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_function() {
        let ir = parse_and_convert("let f = fn(x) -> x");
        let val_expr = get_let_value_expr(&ir, 0);
        match &val_expr.kind {
            ExprKind::Function { params, body, .. } => {
                assert_eq!(params.len(), 1, "expected 1 param");
                assert_eq!(params[0].def_id, None, "param def_id should be None");
                let body_expr = ir.exprs.get(*body);
                assert!(
                    matches!(&body_expr.kind, ExprKind::Identifier(Ident::Unresolved(_))),
                    "body should be unresolved identifier, got {:?}",
                    body_expr.kind
                );
            }
            other => panic!("expected Function, got {:?}", other),
        }
    }

    #[test]
    fn convert_block() {
        let ir = parse_and_convert("{ let x = 1\n x }");
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let expr = ir.exprs.get(*expr_id);
                match &expr.kind {
                    ExprKind::Block { stmts } => {
                        assert_eq!(stmts.len(), 2, "expected 2 stmts in block");
                        let first = ir.stmts.get(stmts[0]);
                        assert!(
                            matches!(&first.kind, StmtKind::Let { .. }),
                            "first stmt should be Let, got {:?}",
                            first.kind
                        );
                        let second = ir.stmts.get(stmts[1]);
                        assert!(
                            matches!(&second.kind, StmtKind::Expr(_)),
                            "second stmt should be Expr, got {:?}",
                            second.kind
                        );
                    }
                    other => panic!("expected Block, got {:?}", other),
                }
            }
            other => panic!("expected Expr stmt, got {:?}", other),
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
    fn convert_const_stmt() {
        let ir = parse_and_convert("const x = 42");
        assert_eq!(ir.root.len(), 1);
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Const { def_id, value, .. } => {
                assert_eq!(*def_id, None, "def_id should be None after conversion");
                let val_expr = ir.exprs.get(*value);
                assert!(
                    matches!(val_expr.kind, ExprKind::Literal(Literal::Integer(42))),
                    "expected integer literal 42, got {:?}",
                    val_expr.kind
                );
            }
            other => panic!("expected Const stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_field_access() {
        let ir = parse_and_convert("let x = 42\nlet y = x.name");
        let val_expr = get_let_value_expr(&ir, 1);
        match &val_expr.kind {
            ExprKind::FieldAccess { expr, .. } => {
                let inner = ir.exprs.get(*expr);
                assert!(
                    matches!(
                        &inner.kind,
                        ExprKind::Identifier(Ident::Unresolved(Path::Simple(_)))
                    ),
                    "field access target should be simple identifier, got {:?}",
                    inner.kind
                );
            }
            other => panic!("expected FieldAccess, got {:?}", other),
        }
    }
}
