use std::collections::HashMap;

use crate::ast::{Loc, ast, hir};
use crate::context::DefId;
use crate::error::CompileError;
use crate::passes::HirProgram;
use crate::passes::resolve::ResolvedAst;

/// HIR lowering pass
pub struct HirLowering {
    resolved: ResolvedAst,
    hir: hir::Hir,
    expr_map: HashMap<ast::ExprId, hir::ExprId>,
    type_map: HashMap<ast::TypeId, hir::TypeId>,
    /// Stack of local variable indices for lambda parameters
    local_stack: Vec<HashMap<DefId, u32>>,
}

impl HirLowering {
    pub fn new(resolved: ResolvedAst) -> Self {
        Self {
            resolved,
            hir: hir::Hir::default(),
            expr_map: HashMap::new(),
            type_map: HashMap::new(),
            local_stack: vec![],
        }
    }

    /// Lower resolved AST to HIR
    pub fn lower(mut self) -> Result<HirProgram, CompileError> {
        // Lower all statements
        let stmt_ids: Vec<_> = self.resolved.ast.stmts.iter().map(|(id, _)| id).collect();
        for stmt_id in stmt_ids {
            self.lower_stmt(stmt_id)?;
        }

        Ok(HirProgram {
            hir: self.hir,
            gcx: self.resolved.gcx,
        })
    }

    /// Lower a statement
    fn lower_stmt(&mut self, stmt_id: ast::StmtId) -> Result<hir::StmtId, CompileError> {
        let stmt_kind = self.resolved.ast.stmts.get(stmt_id).kind.clone();
        let loc = self.resolved.ast.stmts.get(stmt_id).loc.clone();

        let hir_kind = match stmt_kind {
            ast::StmtKind::Import { module, alias } => hir::StmtKind::Import { module, alias },

            ast::StmtKind::Let { name, value } => {
                let hir_value = self.lower_expr(value)?;
                hir::StmtKind::Let {
                    name,
                    value: hir_value,
                }
            }

            ast::StmtKind::Type { name, ty } => {
                let hir_ty = self.lower_type(ty)?;
                hir::StmtKind::Type { name, ty: hir_ty }
            }

            ast::StmtKind::Expr(expr) => {
                let hir_expr = self.lower_expr(expr)?;
                hir::StmtKind::Expr(hir_expr)
            }
        };

        Ok(self.hir.stmts.alloc(hir::Stmt {
            loc,
            kind: hir_kind,
        }))
    }

    /// Lower an expression
    fn lower_expr(&mut self, expr_id: ast::ExprId) -> Result<hir::ExprId, CompileError> {
        // Check cache
        if let Some(&cached) = self.expr_map.get(&expr_id) {
            return Ok(cached);
        }

        let expr_kind = self.resolved.ast.exprs.get(expr_id).kind.clone();
        let loc = self.resolved.ast.exprs.get(expr_id).loc.clone();

        let hir_kind = match expr_kind {
            ast::ExprKind::Identifier(_path) => {
                // Look up the resolved definition
                let def_id = self
                    .resolved
                    .resolutions
                    .expr_to_def
                    .get(&expr_id)
                    .copied()
                    .expect("Name resolution should have resolved this identifier");

                hir::ExprKind::Identifier(def_id)
            }

            ast::ExprKind::Unit => hir::ExprKind::Unit,

            ast::ExprKind::Literal(lit) => hir::ExprKind::Literal(lit),

            ast::ExprKind::List(items) => {
                let hir_items: Result<Vec<_>, _> =
                    items.iter().map(|&id| self.lower_expr(id)).collect();
                hir::ExprKind::List(hir_items?)
            }

            ast::ExprKind::Record(fields) => {
                let hir_fields: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|(name, expr)| Ok((*name, self.lower_expr(*expr)?)))
                    .collect();
                hir::ExprKind::Record(hir_fields?)
            }

            ast::ExprKind::Function { params, body } => {
                // Create a new local variable scope
                let mut locals = HashMap::new();

                // Map parameters to local indices
                let hir_params: Vec<_> = params
                    .iter()
                    .enumerate()
                    .map(|(idx, param)| {
                        // Get the DefId for this parameter from resolution
                        // Parameters are treated as local let bindings
                        let def_id = self
                            .resolved
                            .gcx
                            .definitions
                            .insert(crate::context::DefKind::Let { name: param.name });
                        locals.insert(def_id, idx as u32);

                        ast::Param { name: param.name }
                    })
                    .collect();

                // Push the locals scope
                self.local_stack.push(locals);

                // Lower the body
                let hir_body = self.lower_expr(body)?;

                // Pop the locals scope
                self.local_stack.pop();

                hir::ExprKind::Function {
                    params: hir_params,
                    body: hir_body,
                }
            }

            ast::ExprKind::Application { callee, args } => {
                let hir_callee = self.lower_expr(callee)?;
                let hir_args: Result<Vec<_>, _> =
                    args.iter().map(|&id| self.lower_expr(id)).collect();
                hir::ExprKind::Application {
                    callee: hir_callee,
                    args: hir_args?,
                }
            }

            // Desugar pipe: a |> b  =>  b(a)
            ast::ExprKind::Pipe { lhs, rhs } => {
                let hir_arg = self.lower_expr(lhs)?;
                let hir_func = self.lower_expr(rhs)?;
                hir::ExprKind::Application {
                    callee: hir_func,
                    args: vec![hir_arg],
                }
            }
        };

        let hir_id = self.hir.exprs.alloc(hir::Expr {
            loc,
            kind: hir_kind,
        });
        self.expr_map.insert(expr_id, hir_id);
        Ok(hir_id)
    }

    /// Lower a type
    fn lower_type(&mut self, type_id: ast::TypeId) -> Result<hir::TypeId, CompileError> {
        // Check cache
        if let Some(&cached) = self.type_map.get(&type_id) {
            return Ok(cached);
        }

        let type_kind = self.resolved.ast.types.get(type_id).kind.clone();
        let loc = self.resolved.ast.types.get(type_id).loc.clone();

        let hir_kind = match type_kind {
            ast::TypeKind::Named(_path) => {
                // Look up the resolved definition
                let def_id = self
                    .resolved
                    .resolutions
                    .type_to_def
                    .get(&type_id)
                    .copied()
                    .expect("Name resolution should have resolved this type");

                hir::TypeKind::Named(def_id)
            }

            ast::TypeKind::Unit => hir::TypeKind::Primitive(ast::PrimitiveType::Unit),

            ast::TypeKind::Primitive(prim) => hir::TypeKind::Primitive(prim),

            ast::TypeKind::Provider { .. } => {
                // Providers should have been expanded by the provider expansion pass
                panic!("Provider found during lowering - should have been expanded")
            }

            ast::TypeKind::Function(params, ret) => {
                let hir_params: Result<Vec<_>, _> =
                    params.iter().map(|&id| self.lower_type(id)).collect();
                let hir_ret = self.lower_type(ret)?;
                hir::TypeKind::Function(hir_params?, hir_ret)
            }

            ast::TypeKind::List(inner) => {
                let hir_inner = self.lower_type(inner)?;
                hir::TypeKind::List(hir_inner)
            }

            ast::TypeKind::Record(fields) => {
                let hir_fields: Result<Vec<_>, _> = fields
                    .iter()
                    .map(|(name, ty)| Ok((*name, self.lower_type(*ty)?)))
                    .collect();
                hir::TypeKind::Record(hir_fields?)
            }
        };

        let hir_id = self.hir.types.alloc(hir::Type {
            loc,
            kind: hir_kind,
        });
        self.type_map.insert(type_id, hir_id);
        Ok(hir_id)
    }
}
