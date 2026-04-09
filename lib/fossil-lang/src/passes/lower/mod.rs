use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{self, Loc};
use crate::context::{DefId, DefKind, Symbol, TypeMetadata};
use crate::error::{FossilError, FossilErrors};
use crate::ir::{
    Argument, Expr, ExprId, ExprKind, Ir, Path, RecordFields, Resolutions, Stmt, StmtId, StmtKind,
    Type, TypeId, TypeKind,
};
use crate::passes::GlobalContext;

pub fn lower(
    ast: ast::Ast,
    gcx: GlobalContext,
) -> Result<(Ir, GlobalContext, Resolutions), FossilErrors> {
    lower_with_metadata(ast, gcx, HashMap::new())
}

pub fn lower_with_metadata(
    ast: ast::Ast,
    gcx: GlobalContext,
    metadata: HashMap<Symbol, TypeMetadata>,
) -> Result<(Ir, GlobalContext, Resolutions), FossilErrors> {
    Lowering::new(ast, gcx, metadata).run()
}

// ── Scope stack ────────────────────────────────────────────────

#[derive(Default, Clone)]
struct Scope {
    values: HashMap<Symbol, DefId>,
    types: HashMap<Symbol, DefId>,
}

struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("No scope available")
    }

    fn lookup_value(&self, name: Symbol) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&def_id) = scope.values.get(&name) {
                return Some(def_id);
            }
        }
        None
    }

    fn lookup_type(&self, name: Symbol) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&def_id) = scope.types.get(&name) {
                return Some(def_id);
            }
        }
        None
    }
}

/// Snapshot of AST StmtKind (extracts small values, avoids cloning Vec<Attribute> etc.)
enum StmtSnapshot {
    Let { name: Symbol, value: ast::ExprId },
    Type { name: Symbol, ty: ast::TypeId, attrs: Vec<crate::ast::Attribute>, ctor_params: Vec<crate::ast::ConstructorParam> },
    Expr(ast::ExprId),
}

// ── Lowering pass ──────────────────────────────────────────────

struct Lowering {
    ast: ast::Ast,
    gcx: GlobalContext,
    ir: Ir,
    scopes: ScopeStack,
    resolutions: Resolutions,
    pending_metadata: HashMap<Symbol, TypeMetadata>,
}

impl Lowering {
    fn new(
        ast: ast::Ast,
        gcx: GlobalContext,
        metadata: HashMap<Symbol, TypeMetadata>,
    ) -> Self {
        Self {
            ast,
            gcx,
            ir: Ir::default(),
            scopes: ScopeStack::new(),
            resolutions: Resolutions::default(),
            pending_metadata: metadata,
        }
    }

    fn run(mut self) -> Result<(Ir, GlobalContext, Resolutions), FossilErrors> {
        let mut errors = FossilErrors::new();

        // Pre-scan: collect type declarations (types can be referenced before their
        // body is lowered, e.g. in other type fields). Lets are NOT pre-scanned —
        // they must be defined before use.
        // Collect root stmt ids (Vec<StmtId> = Vec<u32 wrapper>, cheap)
        let root: Vec<_> = self.ast.root.iter().copied().collect();
        // Pre-scan: extract (loc, name) for Type declarations
        let type_decls: Vec<_> = root.iter().filter_map(|&sid| {
            let stmt = self.ast.stmts.get(sid);
            if let ast::StmtKind::Type { name, .. } = &stmt.kind {
                Some((stmt.loc, *name))
            } else {
                None
            }
        }).collect();
        for (loc, name) in type_decls {
            if self.scopes.current_mut().types.contains_key(&name) {
                let name_str = self.gcx.interner.resolve(name).to_string();
                errors.push(FossilError::already_defined(name_str, loc, loc));
            } else {
                let def_id = self.gcx.definitions.insert(None, name, DefKind::Type);
                self.scopes.current_mut().types.insert(name, def_id);
                if let Some(metadata) = self.pending_metadata.remove(&name) {
                    self.gcx.type_metadata.insert(def_id, Arc::new(metadata));
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        // Main pass: lower each statement sequentially.
        for &stmt_id in &root {
            let ir_stmt_id = self.fold_stmt(stmt_id, &mut errors);
            self.ir.root.push(ir_stmt_id);
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok((self.ir, self.gcx, self.resolutions))
    }

    // ── Statements ─────────────────────────────────────────────

    fn fold_stmt(&mut self, stmt_id: ast::StmtId, errors: &mut FossilErrors) -> StmtId {
        // Extract what we need from AST, then drop the borrow
        let (loc, snapshot) = {
            let stmt = self.ast.stmts.get(stmt_id);
            let snap = match &stmt.kind {
                ast::StmtKind::Let { name, value } => StmtSnapshot::Let { name: *name, value: *value },
                ast::StmtKind::Type { name, ty, attrs, ctor_params } => StmtSnapshot::Type {
                    name: *name, ty: *ty, attrs: attrs.clone(), ctor_params: ctor_params.clone(),
                },
                ast::StmtKind::ProviderType { .. } => {
                    // Provider types handled by register_builtins in queries.rs
                    return self.ir.stmts.alloc(Stmt { loc: stmt.loc, kind: StmtKind::Expr(self.ir.exprs.alloc(Expr { loc: stmt.loc, kind: ExprKind::Unit })) });
                }
                ast::StmtKind::Expr(e) => StmtSnapshot::Expr(*e),
            };
            (stmt.loc, snap)
        };
        // AST borrow dropped — free to mutate self

        let (ir_kind, ir_stmt_id) = match snapshot {
            StmtSnapshot::Let { name, value } => {
                let ir_value = self.fold_expr(value, errors);
                let kind = StmtKind::Let {
                    name,
                    value: ir_value,
                };
                let ir_id = self.ir.stmts.alloc(Stmt { loc, kind: kind.clone() });

                // Register let binding
                let def_id = self.scopes.lookup_value(name).unwrap_or_else(|| {
                    let def_id = self.gcx.definitions.insert(None, name, DefKind::Let);
                    self.scopes.current_mut().values.insert(name, def_id);
                    def_id
                });
                self.resolutions.stmt_defs.insert(ir_id, def_id);

                return ir_id;
            }

            StmtSnapshot::Type {
                name,
                ty,
                attrs,
                ctor_params,
            } => {
                let ir_ty = self.fold_type(ty, errors);
                let ir_ctor_params = ctor_params
                    .iter()
                    .map(|p| crate::ir::CtorParam {
                        name: p.name,
                        ty: self.fold_type(p.ty, errors),
                    })
                    .collect();
                let kind = StmtKind::Type {
                    name,
                    ty: ir_ty,
                    attrs,
                    ctor_params: ir_ctor_params,
                };
                let ir_id = self.ir.stmts.alloc(Stmt { loc, kind: kind.clone() });

                // The type def was already registered in pre-scan; store stmt→def mapping.
                if let Some(&def_id) = self.scopes.current_mut().types.get(&name) {
                    self.resolutions.stmt_defs.insert(ir_id, def_id);
                    self.generate_record_constructor(name, ir_ty, Some(def_id));
                }

                return ir_id;
            }

            StmtSnapshot::Expr(expr) => {
                let ir_expr = self.fold_expr(expr, errors);
                let kind = StmtKind::Expr(ir_expr);
                (kind, None)
            }
        };

        if let Some(id) = ir_stmt_id {
            id
        } else {
            self.ir.stmts.alloc(Stmt { loc, kind: ir_kind })
        }
    }

    // ── Expressions ────────────────────────────────────────────

    fn fold_expr(&mut self, expr_id: ast::ExprId, errors: &mut FossilErrors) -> ExprId {
        let expr = self.ast.exprs.get(expr_id);
        let loc = expr.loc;
        let kind_clone = expr.kind.clone();

        match kind_clone {
            ast::ExprKind::Identifier(path) => {
                let ir_expr_id =
                    self.ir
                        .exprs
                        .alloc(Expr { loc, kind: ExprKind::Identifier(path.clone()) });

                // Resolve the identifier
                match &path {
                    Path::Simple(_) => {
                        if let Some(def_id) =
                            self.resolve_value_path(&path, loc, errors)
                        {
                            self.resolutions.expr_defs.insert(ir_expr_id, def_id);
                        }
                    }
                    Path::Qualified(parts) => {
                        let ast_path = Path::Qualified(parts.clone());
                        if let Some(def_id) = self.gcx.definitions.resolve(&ast_path) {
                            self.resolutions.expr_defs.insert(ir_expr_id, def_id);
                        } else if parts.len() >= 2
                            && self.scopes.lookup_value(parts[0]).is_some()
                        {
                            let base_def_id = self.scopes.lookup_value(parts[0]).unwrap();
                            let base = self.ir.exprs.alloc(Expr {
                                kind: ExprKind::Identifier(Path::Simple(parts[0])),
                                loc,
                            });
                            self.resolutions.expr_defs.insert(base, base_def_id);
                            let mut current = base;
                            for &field_sym in &parts[1..] {
                                current = self.ir.exprs.alloc(Expr {
                                    kind: ExprKind::FieldAccess {
                                        expr: current,
                                        field: field_sym,
                                    },
                                    loc,
                                });
                            }
                            self.resolutions
                                .expr_rewrites
                                .insert(ir_expr_id, current);
                        } else {
                            let path_str = ast_path.display(&self.gcx.interner);
                            errors.push(FossilError::undefined_path(path_str, loc));
                        }
                    }
                }

                ir_expr_id
            }

            ast::ExprKind::Unit => {
                self.ir.exprs.alloc(Expr { loc, kind: ExprKind::Unit })
            }

            ast::ExprKind::Literal(lit) => self.ir.exprs.alloc(Expr {
                loc,
                kind: ExprKind::Literal(lit),
            }),

            ast::ExprKind::RecordInstance {
                type_path,
                ctor_args,
                spread,
                fields,
            } => {
                let ir_ctor_args = self.fold_args(&ctor_args, errors);
                let ir_spread = spread.map(|e| self.fold_expr(e, errors));
                let ir_fields: Vec<_> = fields
                    .iter()
                    .map(|(name, expr)| (*name, self.fold_expr(*expr, errors)))
                    .collect();

                let ir_expr_id = self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::RecordInstance {
                        type_name: type_path.clone(),
                        ctor_args: ir_ctor_args,
                        spread: ir_spread,
                        fields: ir_fields,
                    },
                });

                // Resolve the type name
                if let Some(def_id) =
                    self.resolve_type_path(&type_path, loc, errors)
                {
                    self.resolutions.expr_defs.insert(ir_expr_id, def_id);
                }

                ir_expr_id
            }

            ast::ExprKind::Application { callee, args, type_args } => {
                let ir_callee = self.fold_expr(callee, errors);
                let ir_args = self.fold_args(&args, errors);
                // Resolve type arguments from AST TypeIds to DefIds
                let resolved_type_args: Vec<crate::context::DefId> = type_args
                    .iter()
                    .filter_map(|ast_type_id| {
                        let ast_type = self.ast.types.get(*ast_type_id);
                        if let crate::ast::TypeKind::Named(path) = &ast_type.kind {
                            self.resolve_type_path(path, ast_type.loc, errors)
                        } else {
                            errors.push(FossilError::data_error(
                                "type argument must be a named type",
                                ast_type.loc,
                            ));
                            None
                        }
                    })
                    .collect();
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Application {
                        callee: ir_callee,
                        args: ir_args,
                        type_args: resolved_type_args,
                    },
                })
            }

            ast::ExprKind::Projection {
                source,
                param,
                outputs,
            } => {
                let ir_source = self.fold_expr(source, errors);

                // Push scope for the binding
                self.scopes.push();
                let def_id = self.gcx.definitions.insert(None, param, DefKind::Let);
                self.scopes.current_mut().values.insert(param, def_id);

                let ir_outputs: Vec<_> = outputs
                    .iter()
                    .map(|&out| self.fold_expr(out, errors))
                    .collect();

                self.scopes.pop();

                let ir_expr_id = self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Projection {
                        source: ir_source,
                        binding: param,
                        outputs: ir_outputs,
                    },
                });

                self.resolutions.expr_defs.insert(ir_expr_id, def_id);
                ir_expr_id
            }

            ast::ExprKind::Join {
                left,
                right,
                left_on,
                right_on,
                suffix,
            } => {
                let ir_left = self.fold_expr(left, errors);
                let ir_right = self.fold_expr(right, errors);
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Join {
                        left: ir_left,
                        right: ir_right,
                        left_on,
                        right_on,
                        suffix,
                    },
                })
            }

            ast::ExprKind::FieldAccess { expr, field } => {
                let ir_expr = self.fold_expr(expr, errors);
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::FieldAccess {
                        expr: ir_expr,
                        field,
                    },
                })
            }

            ast::ExprKind::StringInterpolation { parts, exprs } => {
                let ir_exprs: Vec<_> =
                    exprs.iter().map(|&e| self.fold_expr(e, errors)).collect();
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::StringInterpolation {
                        parts,
                        exprs: ir_exprs,
                    },
                })
            }

            ast::ExprKind::Coalesce { value, default } => {
                let ir_value = self.fold_expr(value, errors);
                let ir_default = self.fold_expr(default, errors);
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Coalesce {
                        value: ir_value,
                        default: ir_default,
                    },
                })
            }

            ast::ExprKind::Ref { type_path, args } => {
                let ir_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.fold_expr(arg.value(), errors))
                    .collect();

                let ir_expr_id = self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Ref {
                        type_name: type_path.clone(),
                        args: ir_args,
                    },
                });

                if let Some(def_id) =
                    self.resolve_type_path(&type_path, loc, errors)
                {
                    self.resolutions.expr_defs.insert(ir_expr_id, def_id);
                }

                ir_expr_id
            }

            ast::ExprKind::ProviderInvocation { .. } => {
                unreachable!("ProviderInvocation should be expanded before lowering")
            }
        }
    }

    fn fold_args(
        &mut self,
        args: &[ast::Argument],
        errors: &mut FossilErrors,
    ) -> Vec<Argument> {
        args.iter()
            .map(|arg| match arg {
                ast::Argument::Positional(e) => {
                    Argument::Positional(self.fold_expr(*e, errors))
                }
                ast::Argument::Named { name, value } => Argument::Named {
                    name: *name,
                    value: self.fold_expr(*value, errors),
                },
            })
            .collect()
    }

    // ── Types ──────────────────────────────────────────────────

    fn fold_type(&mut self, type_id: ast::TypeId, errors: &mut FossilErrors) -> TypeId {
        let ty = self.ast.types.get(type_id);
        let loc = ty.loc;
        let kind_clone = ty.kind.clone();

        match kind_clone {
            ast::TypeKind::Named(path) => {
                // Resolve directly to Named(DefId) — never create Unresolved.
                if let Some(def_id) = self.resolve_type_path(&path, loc, errors) {
                    self.ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::Named(def_id),
                    })
                } else {
                    // Error already reported; produce a Unit placeholder so lowering
                    // can continue collecting more errors.
                    self.ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::Unit,
                    })
                }
            }

            ast::TypeKind::Unit => self.ir.types.alloc(Type {
                loc,
                kind: TypeKind::Unit,
            }),

            ast::TypeKind::Primitive(prim) => self.ir.types.alloc(Type {
                loc,
                kind: TypeKind::Primitive(prim),
            }),

            ast::TypeKind::Optional(inner) => {
                let ir_inner = self.fold_type(inner, errors);
                self.ir.types.alloc(Type {
                    loc,
                    kind: TypeKind::Optional(ir_inner),
                })
            }

            ast::TypeKind::Record(fields) => {
                let ir_fields: Vec<_> = fields
                    .iter()
                    .map(|f| (f.name, self.fold_type(f.ty, errors)))
                    .collect();
                self.ir.types.alloc(Type {
                    loc,
                    kind: TypeKind::Record(RecordFields::from_fields(ir_fields)),
                })
            }
        }
    }

    // ── Name resolution helpers ────────────────────────────────

    fn resolve_path(
        &self,
        path: &Path,
        loc: Loc,
        scope_lookup: impl Fn(&ScopeStack, Symbol) -> Option<DefId>,
        make_error: impl Fn(String, Loc) -> FossilError,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = scope_lookup(&self.scopes, *name) {
                    return Some(def_id);
                }
                if let Some(def_id) = self.gcx.definitions.resolve(&Path::Simple(*name)) {
                    return Some(def_id);
                }
                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(make_error(name_str, loc));
                None
            }
            Path::Qualified(parts) => {
                let ast_path = Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = ast_path.display(&self.gcx.interner);
                    errors.push(make_error(path_str, loc));
                    None
                })
            }
        }
    }

    fn resolve_value_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        self.resolve_path(
            path,
            loc,
            ScopeStack::lookup_value,
            FossilError::undefined_variable,
            errors,
        )
    }

    fn resolve_type_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        self.resolve_path(
            path,
            loc,
            ScopeStack::lookup_type,
            FossilError::undefined_type,
            errors,
        )
    }

    fn generate_record_constructor(
        &mut self,
        type_name: Symbol,
        type_id: TypeId,
        type_def_id: Option<DefId>,
    ) {
        let ty = self.ir.types.get(type_id);
        if let TypeKind::Record(_) = &ty.kind {
            if self.scopes.current_mut().values.contains_key(&type_name) {
                return;
            }
            let ctor_def_id = self.gcx.definitions.insert(
                type_def_id,
                type_name,
                DefKind::RecordConstructor,
            );
            self.scopes
                .current_mut()
                .values
                .insert(type_name, ctor_def_id);
        }
    }
}

