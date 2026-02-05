//! IR Name Resolution
//!
//! Resolves identifiers in the IR from Ident::Unresolved to Ident::Resolved.
//! Also desugars Pipe expressions.

use std::collections::HashMap;

use std::sync::Arc;

use crate::ast::Loc;
use crate::context::{DefId, DefKind, Symbol, TypeMetadata};
use crate::error::{FossilError, FossilErrors};
use crate::ir::{ExprId, ExprKind, Ident, Ir, Path, StmtId, StmtKind, TypeId, TypeKind};
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

    pub fn resolve(mut self) -> Result<(Ir, GlobalContext), FossilErrors> {
        let mut errors = FossilErrors::new();

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

        Ok((self.ir, self.gcx))
    }

    fn collect_declarations(&mut self, errors: &mut FossilErrors) {
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
                            let name_str = self.gcx.interner.resolve(*name).to_string();
                            errors.push(FossilError::already_defined(name_str, loc, loc));
                        } else {
                            let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                            self.scopes.current_mut().values.insert(*name, def_id);
                        }
                    }
                }

                StmtKind::Const { name, .. } => {
                    if self.scopes.current_mut().values.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Const);
                        self.scopes.current_mut().values.insert(*name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
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

    fn resolve_stmt(&mut self, stmt_id: StmtId, errors: &mut FossilErrors) {
        let stmt = self.ir.stmts.get(stmt_id).clone();

        match &stmt.kind {
            StmtKind::Let {
                name, value, ..
            } => {
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

    fn resolve_expr(&mut self, expr_id: ExprId, errors: &mut FossilErrors) {
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

            ExprKind::NamedRecordConstruction { type_ident, fields } => {
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

            ExprKind::ForYield {
                source,
                outputs,
                binding,
                ..
            } => {
                // Resolve the source expression first (outside the binding scope)
                self.resolve_expr(*source, errors);

                // Create a new scope for the binding
                self.scopes.push();

                // Add binding to scope and store in IR
                let binding_def_id =
                    self.gcx
                        .definitions
                        .insert(None, *binding, crate::context::DefKind::Let);
                self.scopes
                    .current_mut()
                    .values
                    .insert(*binding, binding_def_id);

                // Store the binding_def_id in the IR expression
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::ForYield {
                        binding_def_id: bd, ..
                    } = &mut expr_mut.kind
                    {
                        *bd = Some(binding_def_id);
                    }
                }

                // Resolve each output
                for (idx, output) in outputs.iter().enumerate() {
                    // Resolve the type identifier
                    if let Ident::Unresolved(path) = &output.type_ident
                        && let Some(def_id) = self.resolve_type_path(path, expr.loc, errors)
                    {
                        let expr_mut = self.ir.exprs.get_mut(expr_id);
                        if let ExprKind::ForYield { outputs: o, .. } = &mut expr_mut.kind {
                            o[idx].type_ident = Ident::Resolved(def_id);
                        }
                    }

                    // Resolve constructor arguments
                    for ctor_arg in &output.ctor_args {
                        self.resolve_expr(*ctor_arg, errors);
                    }

                    // Resolve field expressions
                    for (_, field_expr) in &output.fields {
                        self.resolve_expr(*field_expr, errors);
                    }
                }

                self.scopes.pop();
            }

            ExprKind::Unit | ExprKind::Literal(_) => {}
        }
    }

    fn resolve_type(&mut self, type_id: TypeId, errors: &mut FossilErrors) {
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
        errors: &mut FossilErrors,
    ) {
        for (_, ty) in &fields.fields {
            self.resolve_type(*ty, errors);
        }
    }

    fn resolve_value_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
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

                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(FossilError::undefined_variable(name_str, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = format!("{:?}", ast_path);
                    errors.push(FossilError::undefined_path(path_str, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(FossilError::internal(
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
        errors: &mut FossilErrors,
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

                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(FossilError::undefined_type(name_str, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = format!("{:?}", ast_path);
                    errors.push(FossilError::undefined_type(path_str, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(FossilError::internal(
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
                    .insert(type_def_id, type_name, DefKind::RecordConstructor);
            self.scopes
                .current_mut()
                .values
                .insert(type_name, ctor_def_id);
        }
    }
}
