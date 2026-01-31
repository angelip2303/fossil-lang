//! IR Name Resolution
//!
//! Resolves identifiers in the IR from Ident::Unresolved to Ident::Resolved.
//! Also desugars Pipe expressions.

use std::collections::HashMap;

use std::sync::Arc;

use crate::ast::Loc;
use crate::context::{DefId, DefKind, Symbol, TypeMetadata};
use crate::error::{CompileError, CompileErrors, ResolutionError};
use crate::ir::{Argument, ExprId, ExprKind, Ident, Ir, Path, StmtId, StmtKind, TypeId, TypeKind};
use crate::passes::GlobalContext;

/// Scope for name resolution
#[derive(Default, Clone)]
struct Scope {
    values: HashMap<Symbol, DefId>,
    types: HashMap<Symbol, DefId>,
}

/// Stack of scopes for nested resolution
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

/// IR Name Resolver
pub struct IrResolver {
    gcx: GlobalContext,
    ir: Ir,
    scopes: ScopeStack,
    /// Type metadata extracted from AST, keyed by type name
    pending_metadata: HashMap<Symbol, TypeMetadata>,
}

impl IrResolver {
    pub fn new(ir: Ir, gcx: GlobalContext) -> Self {
        Self {
            gcx,
            ir,
            scopes: ScopeStack::new(),
            pending_metadata: HashMap::new(),
        }
    }

    /// Add type metadata to be transferred during resolution
    pub fn with_type_metadata(mut self, metadata: HashMap<Symbol, TypeMetadata>) -> Self {
        self.pending_metadata = metadata;
        self
    }

    pub fn resolve(mut self) -> Result<(Ir, GlobalContext), CompileErrors> {
        let mut errors = CompileErrors::new();

        // Phase 1: Collect declarations
        self.collect_declarations(&mut errors);

        // Return early if there were declaration errors
        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 2: Resolve all statements
        let root = self.ir.root.clone();
        for stmt_id in root {
            self.resolve_stmt(stmt_id, &mut errors);
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 3: Desugar pipes
        self.desugar_pipes();

        Ok((self.ir, self.gcx))
    }

    fn collect_declarations(&mut self, errors: &mut CompileErrors) {
        let root = self.ir.root.clone();
        for stmt_id in root {
            let stmt = self.ir.stmts.get(stmt_id);
            let loc = stmt.loc;

            match &stmt.kind.clone() {
                StmtKind::Let { name, value, .. } => {
                    // Check if it's a function (forward declare)
                    let expr = self.ir.exprs.get(*value);
                    if matches!(expr.kind, ExprKind::Function { .. }) {
                        if self.scopes.current_mut().values.contains_key(name) {
                            errors.push(ResolutionError::AlreadyDefined {
                                name: *name,
                                first_def: loc,
                                second_def: loc,
                            });
                        } else {
                            let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                            self.scopes.current_mut().values.insert(*name, def_id);
                        }
                    }
                }

                StmtKind::Const { name, .. } => {
                    if self.scopes.current_mut().values.contains_key(name) {
                        errors.push(ResolutionError::AlreadyDefined {
                            name: *name,
                            first_def: loc,
                            second_def: loc,
                        });
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Const);
                        self.scopes.current_mut().values.insert(*name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        errors.push(ResolutionError::AlreadyDefined {
                            name: *name,
                            first_def: loc,
                            second_def: loc,
                        });
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Type);
                        self.scopes.current_mut().types.insert(*name, def_id);

                        // Transfer type metadata from pending to final storage
                        if let Some(mut metadata) = self.pending_metadata.remove(name) {
                            metadata.def_id = def_id;
                            self.gcx.type_metadata.insert(def_id, Arc::new(metadata));
                        }
                    }
                }

                StmtKind::Expr(_) => {}
            }
        }
    }

    fn resolve_stmt(&mut self, stmt_id: StmtId, errors: &mut CompileErrors) {
        let stmt = self.ir.stmts.get(stmt_id).clone();

        match &stmt.kind {
            StmtKind::Let {
                name, ty, value, ..
            } => {
                // Resolve type if present
                if let Some(type_id) = ty {
                    self.resolve_type(*type_id, errors);
                }

                // Resolve value
                self.resolve_expr(*value, errors);

                // Get or create DefId
                let def_id = self.scopes.lookup_value(*name).unwrap_or_else(|| {
                    let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                    self.scopes.current_mut().values.insert(*name, def_id);
                    def_id
                });

                // Update statement with DefId
                let stmt_mut = self.ir.stmts.get_mut(stmt_id);
                if let StmtKind::Let { def_id: d, .. } = &mut stmt_mut.kind {
                    *d = Some(def_id);
                }
            }

            StmtKind::Const { name, value, .. } => {
                self.resolve_expr(*value, errors);

                let def_id = self
                    .scopes
                    .lookup_value(*name)
                    .expect("Const should be declared");

                let stmt_mut = self.ir.stmts.get_mut(stmt_id);
                if let StmtKind::Const { def_id: d, .. } = &mut stmt_mut.kind {
                    *d = Some(def_id);
                }
            }

            StmtKind::Type { name, ty, .. } => {
                self.resolve_type(*ty, errors);

                // Generate constructor for record types
                // Look up the type's DefId to use as the constructor's parent
                let type_def_id = self.scopes.lookup_type(*name);
                self.generate_record_constructor(*name, *ty, type_def_id);
            }

            StmtKind::Expr(expr_id) => {
                self.resolve_expr(*expr_id, errors);
            }
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, errors: &mut CompileErrors) {
        let expr = self.ir.exprs.get(expr_id).clone();

        match &expr.kind {
            ExprKind::Identifier(ident) => {
                if let Ident::Unresolved(path) = ident
                    && let Some(def_id) = self.resolve_value_path(path, expr.loc, errors)
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    expr_mut.kind = ExprKind::Identifier(Ident::Resolved(def_id));
                }
            }

            ExprKind::List(items) => {
                for &item in items {
                    self.resolve_expr(item, errors);
                }
            }

            ExprKind::NamedRecordConstruction {
                type_ident,
                fields,
                meta_fields,
            } => {
                // Resolve the type identifier
                if let Ident::Unresolved(path) = type_ident
                    && let Some(def_id) = self.resolve_type_path(path, expr.loc, errors)
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::NamedRecordConstruction { type_ident: ti, .. } =
                        &mut expr_mut.kind
                    {
                        *ti = Ident::Resolved(def_id);
                    }
                }

                // Resolve field expressions
                for (_, field_expr) in fields {
                    self.resolve_expr(*field_expr, errors);
                }

                // Resolve meta-field expressions
                for (_, meta_expr) in meta_fields {
                    self.resolve_expr(*meta_expr, errors);
                }
            }

            ExprKind::Function { params, body, .. } => {
                self.scopes.push();

                // Add parameters to scope and update their DefIds
                let params_clone = params.clone();
                for (i, param) in params_clone.iter().enumerate() {
                    let def_id = self.gcx.definitions.insert(None, param.name, DefKind::Let);
                    self.scopes.current_mut().values.insert(param.name, def_id);

                    // Update the param's def_id in the IR
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::Function { params: ps, .. } = &mut expr_mut.kind {
                        ps[i].def_id = Some(def_id);
                    }

                    // Resolve type annotation
                    if let Some(ty) = param.ty {
                        self.resolve_type(ty, errors);
                    }

                    // Resolve default
                    if let Some(default) = param.default {
                        self.resolve_expr(default, errors);
                    }
                }

                self.resolve_expr(*body, errors);
                self.scopes.pop();
            }

            ExprKind::Application { callee, args } => {
                self.resolve_expr(*callee, errors);
                for arg in args {
                    self.resolve_expr(arg.value(), errors);
                }
            }

            ExprKind::Pipe { lhs, rhs } => {
                self.resolve_expr(*lhs, errors);
                self.resolve_expr(*rhs, errors);
            }

            ExprKind::FieldAccess { expr, .. } => {
                self.resolve_expr(*expr, errors);
            }

            ExprKind::Block { stmts } => {
                self.scopes.push();
                for &stmt_id in stmts {
                    self.resolve_stmt(stmt_id, errors);
                }
                self.scopes.pop();
            }

            ExprKind::StringInterpolation { exprs, .. } => {
                for &expr in exprs {
                    self.resolve_expr(expr, errors);
                }
            }

            ExprKind::Unit | ExprKind::Literal(_) => {}
        }
    }

    fn resolve_type(&mut self, type_id: TypeId, errors: &mut CompileErrors) {
        let ty = self.ir.types.get(type_id).clone();

        match &ty.kind {
            TypeKind::Named(ident) => {
                if let Ident::Unresolved(path) = ident
                    && let Some(def_id) = self.resolve_type_path(path, ty.loc, errors)
                {
                    let ty_mut = self.ir.types.get_mut(type_id);
                    ty_mut.kind = TypeKind::Named(Ident::Resolved(def_id));
                }
            }

            TypeKind::Function(params, ret) => {
                for &param in params {
                    self.resolve_type(param, errors);
                }
                self.resolve_type(*ret, errors);
            }

            TypeKind::List(inner) => {
                self.resolve_type(*inner, errors);
            }

            TypeKind::Record(fields) => {
                self.resolve_record_fields(fields, errors);
            }

            TypeKind::App { ctor, args } => {
                // Resolve constructor
                if let Ident::Unresolved(path) = ctor
                    && let Some(def_id) = self.resolve_type_path(path, ty.loc, errors)
                {
                    let ty_mut = self.ir.types.get_mut(type_id);
                    if let TypeKind::App { ctor: c, .. } = &mut ty_mut.kind {
                        *c = Ident::Resolved(def_id);
                    }
                }

                for &arg in args {
                    self.resolve_type(arg, errors);
                }
            }

            TypeKind::Provider { args, .. } => {
                // Provider types are handled during expansion
                // Just make sure they don't have unresolved references
                for _arg in args {
                    // Provider args are literals, no resolution needed
                }
            }

            TypeKind::Unit | TypeKind::Primitive(_) | TypeKind::Var(_) => {}
        }
    }

    fn resolve_record_fields(
        &mut self,
        fields: &crate::ir::RecordFields,
        errors: &mut CompileErrors,
    ) {
        for (_, ty) in &fields.fields {
            self.resolve_type(*ty, errors);
        }
    }

    fn resolve_value_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut CompileErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_value(*name) {
                    return Some(def_id);
                }

                if let Some(def_id) = self
                    .gcx
                    .definitions
                    .resolve(&crate::ast::ast::Path::Simple(*name))
                {
                    return Some(def_id);
                }

                errors.push(ResolutionError::undefined_variable(*name, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    errors.push(ResolutionError::undefined_path(ast_path, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(CompileError::internal(
                    "resolve",
                    "Relative paths not supported in IR resolver",
                    loc,
                ));
                None
            }
        }
    }

    fn resolve_type_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut CompileErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_type(*name) {
                    return Some(def_id);
                }

                if let Some(def_id) = self
                    .gcx
                    .definitions
                    .resolve(&crate::ast::ast::Path::Simple(*name))
                {
                    return Some(def_id);
                }

                errors.push(ResolutionError::undefined_type(
                    crate::ast::ast::Path::Simple(*name),
                    loc,
                ));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    errors.push(ResolutionError::undefined_type(ast_path, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(CompileError::internal(
                    "resolve",
                    "Relative paths not supported in IR resolver",
                    loc,
                ));
                None
            }
        }
    }

    fn generate_record_constructor(
        &mut self,
        type_name: Symbol,
        type_id: TypeId,
        type_def_id: Option<DefId>,
    ) {
        let ty = self.ir.types.get(type_id);
        if let TypeKind::Record(_) = &ty.kind {
            // Check if constructor name already exists
            if self.scopes.current_mut().values.contains_key(&type_name) {
                return;
            }

            // Register constructor with type's DefId as parent (for metadata lookup)
            let ctor_def_id =
                self.gcx
                    .definitions
                    .insert(type_def_id, type_name, DefKind::Func(None));
            self.scopes
                .current_mut()
                .values
                .insert(type_name, ctor_def_id);
        }
    }

    fn desugar_pipes(&mut self) {
        let expr_ids: Vec<ExprId> = self.ir.exprs.iter().map(|(id, _)| id).collect();
        for expr_id in expr_ids {
            self.desugar_pipe_expr(expr_id);
        }
    }

    fn desugar_pipe_expr(&mut self, expr_id: ExprId) {
        let expr = self.ir.exprs.get(expr_id).clone();

        if let ExprKind::Pipe { lhs, rhs } = expr.kind {
            let rhs_expr = self.ir.exprs.get(rhs).clone();

            let new_kind = match &rhs_expr.kind {
                ExprKind::Application { callee, args } => {
                    let mut new_args = vec![Argument::Positional(lhs)];
                    new_args.extend(args.iter().cloned());
                    ExprKind::Application {
                        callee: *callee,
                        args: new_args,
                    }
                }
                _ => ExprKind::Application {
                    callee: rhs,
                    args: vec![Argument::Positional(lhs)],
                },
            };

            let expr_mut = self.ir.exprs.get_mut(expr_id);
            expr_mut.kind = new_kind;
        }
    }
}
