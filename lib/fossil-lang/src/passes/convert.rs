//! Conversion from AST to IR
//!
//! This module handles the conversion from the parsed AST to the unified IR.
//! After conversion, identifiers are Ident::Unresolved and types are TypeRef::Unknown.

use crate::ast::ast;
use crate::ir::{
    Argument, Expr, ExprKind, Ident, Ir, Literal, Param, Path, PrimitiveType, ProviderArgument,
    RecordRow, Stmt, StmtKind, TraitMethod, Type, TypeKind, TypeRef,
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
        let loc = stmt.loc.clone();

        let kind = match &stmt.kind {
            ast::StmtKind::Let { name, ty, value } => {
                let ir_ty = ty.map(|t| self.convert_type(ast, t));
                let ir_value = self.convert_expr(ast, *value);
                StmtKind::Let {
                    name: *name,
                    def_id: None,
                    ty: ir_ty,
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

            ast::StmtKind::Type { name, ty, attrs } => {
                let ir_ty = self.convert_type(ast, *ty);
                StmtKind::Type {
                    name: *name,
                    ty: ir_ty,
                    attrs: attrs.clone(),
                }
            }

            ast::StmtKind::Trait { name, methods } => {
                let ir_methods = methods
                    .iter()
                    .map(|m| TraitMethod {
                        name: m.name,
                        def_id: None,
                        ty: self.convert_type(ast, m.ty),
                    })
                    .collect();
                StmtKind::Trait {
                    name: *name,
                    def_id: None,
                    methods: ir_methods,
                }
            }

            ast::StmtKind::Impl {
                trait_name,
                type_name,
                methods,
            } => {
                let ir_methods = methods
                    .iter()
                    .map(|(name, expr)| (*name, self.convert_expr(ast, *expr)))
                    .collect();
                StmtKind::Impl {
                    trait_name: Ident::Unresolved(convert_path(trait_name)),
                    type_name: Ident::Unresolved(convert_path(type_name)),
                    methods: ir_methods,
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
        let loc = expr.loc.clone();

        let kind = match &expr.kind {
            ast::ExprKind::Identifier(path) => ExprKind::Identifier(Ident::Unresolved(convert_path(path))),

            ast::ExprKind::Unit => ExprKind::Unit,

            ast::ExprKind::Literal(lit) => ExprKind::Literal(convert_literal(lit)),

            ast::ExprKind::List(items) => {
                let ir_items = items.iter().map(|&e| self.convert_expr(ast, e)).collect();
                ExprKind::List(ir_items)
            }

            ast::ExprKind::Record(fields) => {
                let ir_fields = fields
                    .iter()
                    .map(|(name, expr)| (*name, self.convert_expr(ast, *expr)))
                    .collect();
                ExprKind::Record(ir_fields)
            }

            ast::ExprKind::Function { params, body } => {
                let ir_params = params
                    .iter()
                    .map(|p| Param {
                        name: p.name,
                        def_id: None,
                        ty: p.ty.map(|t| self.convert_type(ast, t)),
                        default: p.default.map(|e| self.convert_expr(ast, e)),
                    })
                    .collect();
                let ir_body = self.convert_expr(ast, *body);
                ExprKind::Function {
                    params: ir_params,
                    body: ir_body,
                }
            }

            ast::ExprKind::Application { callee, args } => {
                let ir_callee = self.convert_expr(ast, *callee);
                let ir_args = args
                    .iter()
                    .map(|arg| match arg {
                        ast::Argument::Positional(e) => Argument::Positional(self.convert_expr(ast, *e)),
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
                let ir_lhs = self.convert_expr(ast, *lhs);
                let ir_rhs = self.convert_expr(ast, *rhs);
                ExprKind::Pipe {
                    lhs: ir_lhs,
                    rhs: ir_rhs,
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
        let loc = ty.loc.clone();

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
                        ast::ProviderArgument::ConstRef(sym) => ProviderArgument::ConstRef(*sym),
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

            ast::TypeKind::Record(fields) => {
                let ir_fields: Vec<_> = fields
                    .iter()
                    .map(|f| (f.name, self.convert_type(ast, f.ty)))
                    .collect();
                TypeKind::Record(RecordRow::from_fields(ir_fields))
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
