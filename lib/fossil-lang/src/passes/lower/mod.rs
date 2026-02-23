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
        let root: Vec<_> = self.ast.root.clone();
        for &stmt_id in &root {
            let stmt = self.ast.stmts.get(stmt_id);
            let loc = stmt.loc;
            match &stmt.kind.clone() {
                ast::StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
                    } else {
                        let def_id =
                            self.gcx.definitions.insert(None, *name, DefKind::Type);
                        self.scopes.current_mut().types.insert(*name, def_id);

                        if let Some(metadata) = self.pending_metadata.remove(name) {
                            self.gcx.type_metadata.insert(def_id, Arc::new(metadata));
                        }
                    }
                }
                ast::StmtKind::ProviderType { .. } => {
                    unreachable!("ProviderType should be expanded before lowering")
                }
                ast::StmtKind::Let { .. } | ast::StmtKind::Expr(_) => {}
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
        let stmt = self.ast.stmts.get(stmt_id);
        let loc = stmt.loc;
        let kind_clone = stmt.kind.clone();

        let (ir_kind, ir_stmt_id) = match kind_clone {
            ast::StmtKind::Let { name, value } => {
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

            ast::StmtKind::Type {
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

            ast::StmtKind::ProviderType { .. } => {
                unreachable!("ProviderType should be expanded before lowering")
            }

            ast::StmtKind::Expr(expr) => {
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

            ast::ExprKind::Application { callee, args } => {
                let ir_callee = self.fold_expr(callee, errors);
                let ir_args = self.fold_args(&args, errors);
                self.ir.exprs.alloc(Expr {
                    loc,
                    kind: ExprKind::Application {
                        callee: ir_callee,
                        args: ir_args,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::extract_type_metadata;
    use crate::context::DefKind;
    use crate::error::FossilError;
    use crate::ir::{Argument, ExprKind, Literal, Path, StmtKind, TypeKind};
    use crate::passes::expand::ProviderExpander;
    use crate::passes::parse::Parser;

    // ── Helpers ────────────────────────────────────────────────

    fn parse_and_lower(src: &str) -> (Ir, GlobalContext, Resolutions) {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = extract_type_metadata(&expand_result.ast);
        lower_with_metadata(expand_result.ast, expand_result.gcx, ty)
            .expect("lower failed")
    }

    fn lower_err(src: &str) -> FossilErrors {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = extract_type_metadata(&expand_result.ast);
        match lower_with_metadata(expand_result.ast, expand_result.gcx, ty) {
            Err(errors) => errors,
            Ok(_) => panic!("expected lower error, but lowering succeeded"),
        }
    }

    fn get_let_value_expr<'a>(ir: &'a Ir, root_idx: usize) -> &'a Expr {
        let stmt = ir.stmts.get(ir.root[root_idx]);
        match &stmt.kind {
            StmtKind::Let { value, .. } => ir.exprs.get(*value),
            other => panic!("expected Let stmt at root[{}], got {:?}", root_idx, other),
        }
    }

    fn get_expr_stmt<'a>(ir: &'a Ir, root_idx: usize) -> &'a Expr {
        let stmt = ir.stmts.get(ir.root[root_idx]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => ir.exprs.get(*expr_id),
            other => panic!("expected Expr stmt at root[{}], got {:?}", root_idx, other),
        }
    }

    // ── Convert tests (from convert.rs) ────────────────────────

    #[test]
    fn convert_let_stmt() {
        let (ir, _, _) = parse_and_lower("let x = 42");
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
        let (ir, _, _) = parse_and_lower("type T do Name: string end");
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
        let (ir, _, _) = parse_and_lower("42");
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
    fn pipe_application_prepends_source() {
        let (ir, _, _) = parse_and_lower("let f = 1\nlet x = 42\nlet r = x |> f(1)");
        let val_expr = get_let_value_expr(&ir, 2);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                    "callee should be simple path, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 2, "expected 2 args (source prepended + original)");
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_identifier_to_application() {
        let (ir, _, _) = parse_and_lower("let f = 1\nlet x = 42\nlet y = x |> f");
        let val_expr = get_let_value_expr(&ir, 2);
        match &val_expr.kind {
            ExprKind::Application { callee, args } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Path::Simple(_))),
                    "callee should be simple path, got {:?}",
                    callee_expr.kind
                );
                assert_eq!(args.len(), 1, "expected 1 arg (just source)");
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_chained() {
        let (ir, _, _) = parse_and_lower("let f = 1\nlet g = 1\nlet x = 42\nlet y = x |> f |> g");
        let val_expr = get_let_value_expr(&ir, 3);
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
                            matches!(
                                &inner_callee_expr.kind,
                                ExprKind::Identifier(Path::Simple(_))
                            ),
                            "inner callee should be f"
                        );
                        assert_eq!(inner_args.len(), 1, "inner application has 1 arg");
                    }
                    other => panic!("expected inner Application, got {:?}", other),
                }
            }
            other => panic!("expected outer Application, got {:?}", other),
        }
    }

    #[test]
    fn pipe_preserves_named_args() {
        let (ir, _, _) = parse_and_lower("let f = 1\nlet x = 42\nlet y = x |> f(name: 1)");
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
        let (ir, _, _) = parse_and_lower("let f = 1\nlet x = 42\nlet y = x |> f");
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
        let (ir, _, _) = parse_and_lower("let f = 1\nlet y = 42 |> f");
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

    #[test]
    fn pipe_projection() {
        let (ir, _, _) = parse_and_lower("let x = 42\nlet y = x |> each row -> row");
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
        let (ir, _, _) =
            parse_and_lower("let x = 42\nlet y = x |> each row -> row +> each row -> row");
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
        let (ir, _, _) = parse_and_lower(
            "let x = 42\nlet y = x |> each row -> row +> each row -> row +> each row -> row",
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
        let (ir, _, _) =
            parse_and_lower("type T do Name: string end\nT(42) { Name = \"hi\" }");
        let expr = get_expr_stmt(&ir, 1);
        match &expr.kind {
            ExprKind::RecordInstance {
                ctor_args, fields, ..
            } => {
                assert_eq!(ctor_args.len(), 1, "expected 1 ctor arg");
                assert_eq!(fields.len(), 1, "expected 1 field");
            }
            other => panic!("expected RecordInstance, got {:?}", other),
        }
    }

    #[test]
    fn join_basic() {
        let (ir, _, _) = parse_and_lower("let a = 1\nlet b = 2\nlet c = a |> join b on X = Y");
        let val = get_let_value_expr(&ir, 2);
        assert!(matches!(&val.kind, ExprKind::Join { .. }));
    }

    #[test]
    fn join_multi_column() {
        let (ir, _, _) = parse_and_lower(
            "let a = 1\nlet b = 2\nlet c = a |> join b on (X, Y) = (A, B)",
        );
        let val = get_let_value_expr(&ir, 2);
        match &val.kind {
            ExprKind::Join {
                left_on, right_on, ..
            } => {
                assert_eq!(left_on.len(), 2);
                assert_eq!(right_on.len(), 2);
            }
            other => panic!("expected Join, got {:?}", other),
        }
    }

    #[test]
    fn join_with_suffix() {
        let (ir, _, _) = parse_and_lower(
            "let a = 1\nlet b = 2\nlet c = a |> join b on X = Y suffix \"_room\"",
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
        let (ir, _, _) = parse_and_lower("let name = \"world\"\n\"hello ${name}\"");
        let stmt = ir.stmts.get(ir.root[1]);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let expr = ir.exprs.get(*expr_id);
                match &expr.kind {
                    ExprKind::StringInterpolation { parts, exprs } => {
                        assert_eq!(parts.len(), 2, "expected 2 string parts");
                        assert_eq!(exprs.len(), 1, "expected 1 interpolated expr");
                    }
                    other => panic!("expected StringInterpolation, got {:?}", other),
                }
            }
            other => panic!("expected Expr stmt, got {:?}", other),
        }
    }

    #[test]
    fn convert_dot_access_is_qualified_path() {
        let (ir, _, _) = parse_and_lower("let x = 42\nlet y = x.name");
        let val_expr = get_let_value_expr(&ir, 1);
        // In lower, x.name is a Qualified path that gets rewritten to FieldAccess
        // via expr_rewrites. The original Identifier is still there but rewritten.
        assert!(
            matches!(
                &val_expr.kind,
                ExprKind::Identifier(Path::Qualified(_))
            ),
            "x.name should be Qualified path after lower, got {:?}",
            val_expr.kind
        );
    }

    // ── Resolve tests (from resolve.rs) ────────────────────────

    #[test]
    fn let_gets_def_id() {
        let (ir, _, resolutions) = parse_and_lower("let x = 42");
        let stmt_id = ir.root[0];
        assert!(
            resolutions.stmt_defs.contains_key(&stmt_id),
            "let binding should have a DefId in resolutions"
        );
    }

    #[test]
    fn type_gets_def_id() {
        let (_, gcx, _) = parse_and_lower("type T do Name: string end");
        let name_sym = gcx.interner.lookup("T").expect("T should be interned");
        let def = gcx
            .definitions
            .get_by_symbol(name_sym)
            .expect("T should have a definition");
        assert!(matches!(def.kind, DefKind::Type));
    }

    #[test]
    fn variable_reference_resolves() {
        let (ir, _, resolutions) = parse_and_lower("let x = 42\nlet y = x");
        let y_stmt = ir.stmts.get(ir.root[1]);
        let value_id = match &y_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let statement, got {:?}", other),
        };
        assert!(
            resolutions.expr_defs.contains_key(&value_id),
            "x reference should be resolved"
        );
    }

    #[test]
    fn record_constructor_generated() {
        let src = "type T do Name: string end\nT { Name = \"hi\" }";
        let (ir, _, resolutions) = parse_and_lower(src);
        let expr_stmt = ir.stmts.get(ir.root[1]);
        let expr_id = match &expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        assert!(
            resolutions.expr_defs.contains_key(&expr_id),
            "record instance type should be resolved"
        );
    }

    #[test]
    fn projection_param_gets_def_id() {
        let (ir, _, resolutions) = parse_and_lower("let x = 42\nx |> each row -> row");
        let expr_stmt = ir.stmts.get(ir.root[1]);
        let expr_id = match &expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        assert!(
            resolutions.expr_defs.contains_key(&expr_id),
            "projection binding should have a DefId in resolutions"
        );
    }

    #[test]
    fn multiple_let_bindings_resolve() {
        let src = "let a = 1\nlet b = 2\nlet c = a";
        let (ir, _, resolutions) = parse_and_lower(src);
        let c_stmt = ir.stmts.get(ir.root[2]);
        let c_value = match &c_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let, got {:?}", other),
        };
        assert!(
            resolutions.expr_defs.contains_key(&c_value),
            "c should resolve reference to a"
        );
    }

    // ── Error tests ────────────────────────────────────────────

    #[test]
    fn undefined_variable() {
        let errors = lower_err("x");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "variable", name, .. } if name == "x"),
            "expected Undefined variable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type() {
        let errors = lower_err("type T do Name: Foo end");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "type", name, .. } if name == "Foo"),
            "expected Undefined type for Foo, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_path() {
        let src = "let y = A.b";
        let errors = lower_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { .. }),
            "expected Undefined for A.b, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn duplicate_type_def() {
        let src = "type T do end\ntype T do end";
        let errors = lower_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::AlreadyDefined { name, .. } if name == "T"),
            "expected AlreadyDefined for T, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn forward_reference_non_function() {
        let src = "let y = x\nlet x = 42";
        let errors = lower_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "variable", name, .. } if name == "x"),
            "expected Undefined variable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type_in_record_construction() {
        let src = "Foo { Name = \"hi\" }";
        let errors = lower_err(src);
        assert!(!errors.is_empty(), "should have at least one error");
        let has_type_error = errors
            .0
            .iter()
            .any(|e| matches!(e, FossilError::Undefined { .. }));
        assert!(
            has_type_error,
            "expected Undefined for Foo, got {:?}",
            errors.0
        );
    }
}
