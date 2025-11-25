use std::collections::HashMap;

use crate::ast::Loc;
use crate::ast::ast::*;
use crate::context::*;
use crate::error::{CompileError, CompileErrorKind};
use crate::passes::GlobalContext;

pub struct ResolvedAst {
    pub ast: Ast,
    pub gcx: GlobalContext,
    pub resolutions: ResolutionTable,
}

/// Resolution table mapping AST nodes to their definitions
#[derive(Default)]
pub struct ResolutionTable {
    /// Maps expression IDs (identifiers) to their definitions
    pub expr_to_def: HashMap<ExprId, DefId>,
    /// Maps type IDs (named types) to their definitions
    pub type_to_def: HashMap<TypeId, DefId>,
}

/// Name resolver - builds symbol tables and resolves all names
pub struct NameResolver {
    gcx: GlobalContext,
    ast: Ast,
    resolutions: ResolutionTable,
    scopes: ScopeStack,
}

#[derive(Default)]
struct ScopeStack {
    scopes: Vec<Scope>,
}

#[derive(Default)]
struct Scope {
    /// Value bindings (let bindings, function parameters)
    values: HashMap<Symbol, DefId>,
    /// Type bindings (type definitions)
    types: HashMap<Symbol, DefId>,
    /// Import aliases (module imports)
    imports: HashMap<Symbol, Path>,
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
        self.scopes.last_mut().unwrap()
    }

    fn lookup_value(&self, name: Symbol) -> Option<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.values.get(&name).copied())
    }

    fn lookup_type(&self, name: Symbol) -> Option<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|s| s.types.get(&name).copied())
    }

    fn resolve_import(&self, alias: Symbol) -> Option<&Path> {
        self.scopes.iter().rev().find_map(|s| s.imports.get(&alias))
    }
}

impl NameResolver {
    pub fn new(ast: Ast, gcx: GlobalContext) -> Self {
        Self {
            gcx,
            ast,
            resolutions: ResolutionTable::default(),
            scopes: ScopeStack::new(),
        }
    }

    pub fn resolve(mut self) -> Result<ResolvedAst, CompileError> {
        self.collect_declarations()?;

        let stmt_ids: Vec<_> = self.ast.stmts.iter().map(|(id, _)| id).collect();
        for stmt_id in stmt_ids {
            self.resolve_stmt(stmt_id)?;
        }

        Ok(ResolvedAst {
            ast: self.ast,
            gcx: self.gcx,
            resolutions: self.resolutions,
        })
    }

    fn collect_declarations(&mut self) -> Result<(), CompileError> {
        let stmt_ids: Vec<_> = self.ast.stmts.iter().map(|(id, _)| id).collect();

        for stmt_id in stmt_ids {
            let stmt = self.ast.stmts.get(stmt_id);
            let loc = stmt.loc.clone();

            match stmt.kind {
                StmtKind::Import { module, alias } => {
                    self.scopes.current_mut().imports.insert(alias, module);
                }

                StmtKind::Let { name, value } => {
                    let expr = self.ast.exprs.get(value);
                    if matches!(expr.kind, ExprKind::Function { .. }) {
                        if self.scopes.current_mut().values.contains_key(&name) {
                            return Err(CompileError::new(
                                CompileErrorKind::AlreadyDefined(name),
                                loc,
                            ));
                        }

                        let def_id = self.gcx.definitions.insert(DefKind::LocalLet { name });
                        self.scopes.current_mut().values.insert(name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(&name) {
                        return Err(CompileError::new(
                            CompileErrorKind::AlreadyDefined(name),
                            loc,
                        ));
                    }

                    let def_id = self.gcx.definitions.insert(DefKind::LocalType { name });
                    self.scopes.current_mut().types.insert(name, def_id);
                }

                StmtKind::Expr(_) => {}
            }
        }

        Ok(())
    }

    /// Pass 2: Resolve names in statements
    fn resolve_stmt(&mut self, stmt_id: StmtId) -> Result<(), CompileError> {
        let stmt_kind = self.ast.stmts.get(stmt_id).kind.clone();
        let loc = self.ast.stmts.get(stmt_id).loc.clone();

        match stmt_kind {
            StmtKind::Import { .. } => {
                // Already processed in collect_declarations
                Ok(())
            }

            StmtKind::Let { name, value } => {
                // Resolve the value expression
                self.resolve_expr(value)?;

                // If not already registered (not a function), register now
                if !self.scopes.current_mut().values.contains_key(&name) {
                    let def_id = self.gcx.definitions.insert(DefKind::LocalLet { name });
                    self.scopes.current_mut().values.insert(name, def_id);
                }

                Ok(())
            }

            StmtKind::Type { ty, .. } => {
                // Already registered in collect_declarations, just resolve the type
                self.resolve_type(ty)
            }

            StmtKind::Expr(expr) => self.resolve_expr(expr),
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId) -> Result<(), CompileError> {
        let expr_kind = self.ast.exprs.get(expr_id).kind.clone();
        let loc = self.ast.exprs.get(expr_id).loc.clone();

        match expr_kind {
            ExprKind::Identifier(ref path) => {
                let def_id = self.resolve_value_path(path, loc)?;
                self.resolutions.expr_to_def.insert(expr_id, def_id);
            }

            ExprKind::List(items) => {
                for item in items {
                    self.resolve_expr(item)?;
                }
            }

            ExprKind::Record(fields) => {
                for (_, expr) in fields {
                    self.resolve_expr(expr)?;
                }
            }

            ExprKind::Function { params, body } => {
                self.scopes.push();

                for param in params {
                    let def_id = self
                        .gcx
                        .definitions
                        .insert(DefKind::LocalLet { name: param.name });
                    self.scopes.current_mut().values.insert(param.name, def_id);
                }

                self.resolve_expr(body)?;
                self.scopes.pop();
            }

            ExprKind::Application { callee, args } => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }

            ExprKind::Pipe { lhs, rhs } => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)?;
            }

            ExprKind::Unit | ExprKind::Literal(_) => {}
        }

        Ok(())
    }

    fn resolve_type(&mut self, type_id: TypeId) -> Result<(), CompileError> {
        let type_kind = self.ast.types.get(type_id).kind.clone();
        let loc = self.ast.types.get(type_id).loc.clone();

        match type_kind {
            TypeKind::Named(ref path) => {
                let def_id = self.resolve_type_path(path, loc)?;
                self.resolutions.type_to_def.insert(type_id, def_id);
            }

            TypeKind::Function(params, ret) => {
                for param in params {
                    self.resolve_type(param)?;
                }
                self.resolve_type(ret)?;
            }

            TypeKind::List(inner) => self.resolve_type(inner)?,

            TypeKind::Record(fields) => {
                for (_, ty) in fields {
                    self.resolve_type(ty)?;
                }
            }

            TypeKind::Provider { .. } => {}

            TypeKind::Unit | TypeKind::Primitive(_) => {}
        }

        Ok(())
    }

    fn resolve_value_path(&mut self, path: &Path, loc: Loc) -> Result<DefId, CompileError> {
        match path {
            &Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_value(name) {
                    return Ok(def_id);
                }

                if let Some(def_id) = self.gcx.definitions.resolve(name) {
                    return Ok(def_id);
                }

                Err(CompileError::new(
                    CompileErrorKind::UndefinedVariable { name },
                    loc,
                ))
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(parts)
                    .ok_or_else(|| CompileError::new(CompileErrorKind::UndefinedPath { path }, loc))
            }
        }
    }

    fn resolve_type_path(&mut self, path: &Path, loc: Loc) -> Result<DefId, CompileError> {
        match path {
            &Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_type(name) {
                    return Ok(def_id);
                }

                if let Some(def_id) = self.gcx.definitions.resolve(name) {
                    return Ok(def_id);
                }

                Err(CompileError::new(
                    CompileErrorKind::UndefinedType(name.into()),
                    loc,
                ))
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(parts)
                    .ok_or_else(|| CompileError::new(CompileErrorKind::UndefinedType(path), loc))
            }
        }
    }
}
