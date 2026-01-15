use std::collections::HashMap;

use crate::ast::{ast, hir};
use crate::error::{CompileError, CompileErrors};
use crate::passes::HirProgram;
use crate::passes::resolve::ResolvedAst;

/// HIR lowering pass
pub struct HirLowering {
    resolved: ResolvedAst,
    hir: hir::Hir,
    expr_map: HashMap<ast::ExprId, hir::ExprId>,
    type_map: HashMap<ast::TypeId, hir::TypeId>,
}

impl HirLowering {
    pub fn new(resolved: ResolvedAst) -> Self {
        Self {
            resolved,
            hir: hir::Hir::default(),
            expr_map: HashMap::new(),
            type_map: HashMap::new(),
        }
    }

    /// Lower resolved AST to HIR
    pub fn lower(mut self) -> Result<HirProgram, CompileErrors> {
        // Lower root statements, accumulating errors
        let root_stmt_ids = self.resolved.ast.root.clone();
        let mut hir_root_ids = Vec::new();
        let mut errors = CompileErrors::new();

        for stmt_id in root_stmt_ids {
            match self.lower_stmt(stmt_id) {
                Ok(hir_stmt_id) => hir_root_ids.push(hir_stmt_id),
                Err(e) => errors.push(e),
            }
        }

        // If errors occurred, return them
        if !errors.is_empty() {
            return Err(errors);
        }

        self.hir.root = hir_root_ids;

        Ok(HirProgram {
            hir: self.hir,
            gcx: self.resolved.gcx,
            resolutions: self.resolved.resolutions,
        })
    }

    /// Lower a statement
    fn lower_stmt(&mut self, stmt_id: ast::StmtId) -> Result<hir::StmtId, CompileError> {
        let stmt = self.resolved.ast.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let loc = stmt.loc.clone();

        let hir_kind = match &stmt_kind {
            ast::StmtKind::Import { module, alias } => {
                // If no alias is provided, use the module name
                let alias = alias.unwrap_or_else(|| match module {
                    ast::Path::Simple(name) => *name,
                    ast::Path::Qualified(parts) => *parts
                        .last()
                        .expect("SAFETY: Qualified paths are validated to be non-empty during parsing. \
                                 A Qualified path with zero parts cannot be constructed."),
                    ast::Path::Relative { components, .. } => *components
                        .last()
                        .expect("SAFETY: Relative paths are validated to have at least one component during parsing. \
                                 A Relative path with zero components cannot be constructed."),
                });
                hir::StmtKind::Import {
                    module: module.clone(),
                    alias
                }
            }

            ast::StmtKind::Let { name, ty, value } => {
                let hir_value = self.lower_expr(*value)?;

                // Lower the type annotation if present
                let hir_ty = if let Some(ty_id) = ty {
                    Some(self.lower_type(*ty_id)?)
                } else {
                    None
                };

                // Get the DefId for this let binding from resolution table using StmtId
                let def_id = *self
                    .resolved
                    .resolutions
                    .let_bindings
                    .get(&stmt_id)
                    .expect("SAFETY: Name resolution pass must run before lowering and populate let_bindings table. \
                             All Let statements are guaranteed to have a DefId entry after name resolution completes successfully.");

                hir::StmtKind::Let {
                    name: *name,
                    def_id,
                    ty: hir_ty,
                    value: hir_value,
                }
            }

            ast::StmtKind::Type { name, ty } => {
                let hir_ty = self.lower_type(*ty)?;
                hir::StmtKind::Type { name: *name, ty: hir_ty }
            }

            ast::StmtKind::Expr(expr) => {
                let hir_expr = self.lower_expr(*expr)?;
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
                    .exprs
                    .get(&expr_id)
                    .copied()
                    .expect("SAFETY: Name resolution pass must run before lowering and populate exprs table. \
                             All Identifier expressions are guaranteed to have a DefId entry after name resolution completes successfully.");

                hir::ExprKind::Identifier(def_id)
            }

            ast::ExprKind::Unit => hir::ExprKind::Unit,

            ast::ExprKind::Literal(lit) => hir::ExprKind::Literal(lit),

            ast::ExprKind::List(items) => {
                let hir_items: Result<Vec<_>, CompileError> =
                    items.iter().map(|&id| self.lower_expr(id)).collect();
                hir::ExprKind::List(hir_items?)
            }

            ast::ExprKind::Record(fields) => {
                let hir_fields: Result<Vec<_>, CompileError> = fields
                    .iter()
                    .map(|(name, expr)| Ok((*name, self.lower_expr(*expr)?)))
                    .collect();
                hir::ExprKind::Record(hir_fields?)
            }

            ast::ExprKind::Function { params, body } => {
                // Get the DefIds for the parameters from the resolution table
                let param_def_ids = self
                    .resolved
                    .resolutions
                    .function_params
                    .get(&expr_id)
                    .expect("SAFETY: Name resolution pass must run before lowering and populate function_params table. \
                             All Function expressions are guaranteed to have parameter DefId entries after name resolution completes successfully.");

                // Create HIR params with DefIds
                let hir_params: Vec<_> = params
                    .iter()
                    .zip(param_def_ids)
                    .map(|(param, def_id)| hir::Param {
                        name: param.name,
                        def_id: *def_id,
                    })
                    .collect();

                // Lower the body (parameter references are already resolved)
                let hir_body = self.lower_expr(body)?;

                hir::ExprKind::Function {
                    params: hir_params,
                    body: hir_body,
                }
            }

            ast::ExprKind::Application { callee, args } => {
                let hir_callee = self.lower_expr(callee)?;
                let hir_args: Result<Vec<_>, CompileError> =
                    args.iter().map(|&id| self.lower_expr(id)).collect();
                hir::ExprKind::Application {
                    callee: hir_callee,
                    args: hir_args?,
                }
            }

            // Desugar pipe: a |> b  =>  b(a)
            // If b is already a function call f(x, y), then a |> f(x, y)  =>  f(a, x, y)
            ast::ExprKind::Pipe { lhs, rhs } => {
                let hir_arg = self.lower_expr(lhs)?;

                // Check if RHS is an Application node (clone to avoid borrow issues)
                let rhs_expr = self.resolved.ast.exprs.get(rhs);
                let rhs_kind = rhs_expr.kind.clone();

                match rhs_kind {
                    ast::ExprKind::Application { callee, args } => {
                        // RHS is already a function call: prepend LHS as first argument
                        let hir_callee = self.lower_expr(callee)?;
                        let mut hir_args = vec![hir_arg];
                        for arg in args {
                            hir_args.push(self.lower_expr(arg)?);
                        }
                        hir::ExprKind::Application {
                            callee: hir_callee,
                            args: hir_args,
                        }
                    }
                    _ => {
                        // RHS is not a function call: simple pipe desugaring
                        let hir_func = self.lower_expr(rhs)?;
                        hir::ExprKind::Application {
                            callee: hir_func,
                            args: vec![hir_arg],
                        }
                    }
                }
            }

            // Field access: pass through without desugaring
            ast::ExprKind::FieldAccess { expr, field } => {
                let hir_expr = self.lower_expr(expr)?;
                hir::ExprKind::FieldAccess {
                    expr: hir_expr,
                    field,
                }
            }

            ast::ExprKind::Block { stmts } => {
                // Lower all statements
                let hir_stmts: Result<Vec<_>, _> = stmts
                    .iter()
                    .map(|&stmt_id| self.lower_stmt(stmt_id))
                    .collect();

                hir::ExprKind::Block {
                    stmts: hir_stmts?,
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
                    .types
                    .get(&type_id)
                    .copied()
                    .expect("SAFETY: Name resolution pass must run before lowering and populate types table. \
                             All Named type references are guaranteed to have a DefId entry after name resolution completes successfully.");

                hir::TypeKind::Named(def_id)
            }

            ast::TypeKind::Unit => hir::TypeKind::Primitive(ast::PrimitiveType::Unit),

            ast::TypeKind::Primitive(prim) => hir::TypeKind::Primitive(prim),

            ast::TypeKind::Provider { .. } => {
                // Providers should have been expanded by the provider expansion pass (F# style)
                // If we reach here, the expand pass didn't run correctly
                return Err(CompileError::new(
                    crate::error::CompileErrorKind::ProviderError(
                        self.resolved.gcx.interner.intern("Provider type not expanded - this is a compiler bug")
                    ),
                    loc,
                ))
            }

            ast::TypeKind::Function(params, ret) => {
                let hir_params: Result<Vec<_>, CompileError> =
                    params.iter().map(|&id| self.lower_type(id)).collect();
                let hir_ret = self.lower_type(ret)?;
                hir::TypeKind::Function(hir_params?, hir_ret)
            }

            ast::TypeKind::List(inner) => {
                let hir_inner = self.lower_type(inner)?;

                // Convert List<T> to App { ctor: List, args: [T] }
                let list_ctor = self.resolved.gcx.list_type_ctor
                    .expect("SAFETY: List type constructor is registered in GlobalContext::new() as a builtin type. \
                             It is guaranteed to be present in all GlobalContext instances.");

                hir::TypeKind::App {
                    ctor: list_ctor,
                    args: vec![hir_inner],
                }
            }

            ast::TypeKind::App { ctor, args } => {
                // Lower type constructor path to DefId
                // For now, we only support simple paths (e.g., Entity, not module::Entity)
                let ctor_def_id = match &ctor {
                    ast::Path::Simple(sym) => {
                        // Check registered type constructors
                        self.resolved.gcx.type_constructors.iter()
                            .find(|(_, info)| info.name == *sym)
                            .map(|(def_id, _)| *def_id)
                            .ok_or_else(|| {
                                CompileError::new(
                                    crate::error::CompileErrorKind::ProviderError(
                                        self.resolved.gcx.interner.intern("Type constructor not found")
                                    ),
                                    loc.clone(),
                                )
                            })?
                    }
                    ast::Path::Qualified(_) => {
                        // TODO: Support qualified paths for type constructors
                        return Err(CompileError::new(
                            crate::error::CompileErrorKind::ProviderError(
                                self.resolved.gcx.interner.intern("Qualified type constructors not yet supported")
                            ),
                            loc,
                        ));
                    }
                    ast::Path::Relative { .. } => {
                        // TODO: Support relative paths for type constructors
                        return Err(CompileError::new(
                            crate::error::CompileErrorKind::ProviderError(
                                self.resolved.gcx.interner.intern("Relative path type constructors not yet supported")
                            ),
                            loc,
                        ));
                    }
                };

                // Lower type arguments
                let hir_args: Result<Vec<_>, CompileError> =
                    args.iter().map(|&id| self.lower_type(id)).collect();

                hir::TypeKind::App {
                    ctor: ctor_def_id,
                    args: hir_args?,
                }
            }

            ast::TypeKind::Record(fields) => {
                let hir_fields: Result<Vec<_>, CompileError> = fields
                    .iter()
                    .map(|field| Ok(hir::RecordField {
                        name: field.name,
                        ty: self.lower_type(field.ty)?,
                        attrs: field.attrs.clone(), // Preserve attributes
                    }))
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

// Note: We lower statements by ID (StmtId → HIR StmtId), fetching from the AST arena.
// This follows rustc's approach: AST has NodeIds, HIR has HirIds, and lowering maps between them.
