//! Name resolver implementation

use crate::ast::Loc;
use crate::ast::ast::*;
use crate::context::*;
use crate::error::{CompileError, CompileErrorKind};
use crate::passes::GlobalContext;

use super::scope::ScopeStack;
use super::table::{ResolutionTable, ResolvedAst};

/// Name resolver - builds symbol tables and resolves all names
pub struct NameResolver {
    gcx: GlobalContext,
    ast: Ast,
    resolutions: ResolutionTable,
    scopes: ScopeStack,
}

impl NameResolver {
    pub fn new(ast: Ast, gcx: GlobalContext) -> Self {
        let mut resolver = Self {
            gcx,
            ast,
            resolutions: ResolutionTable::default(),
            scopes: ScopeStack::new(),
        };

        // Apply prelude to root scope
        let prelude = crate::passes::Prelude::standard();
        prelude.apply(resolver.scopes.current_mut(), &mut resolver.gcx);

        resolver
    }

    pub fn resolve(mut self) -> Result<ResolvedAst, CompileError> {
        self.collect_declarations()?;

        // Resolve root statements (block statements are resolved when their block is resolved)
        let stmt_ids = self.ast.root.clone();
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
        // Collect declarations from root statements only
        let stmt_ids = self.ast.root.clone();

        for stmt_id in stmt_ids {
            let stmt = self.ast.stmts.get(stmt_id);
            let loc = stmt.loc.clone();

            match &stmt.kind {
                StmtKind::Import { module, alias } => {
                    if let Some(alias) = alias {
                        self.scopes
                            .current_mut()
                            .imports
                            .insert(*alias, module.clone());
                    }
                }

                StmtKind::Let { name, ty: _, value } => {
                    let expr = self.ast.exprs.get(*value);
                    if matches!(expr.kind, ExprKind::Function { .. }) {
                        if self.scopes.current_mut().values.contains_key(name) {
                            return Err(CompileError::new(
                                CompileErrorKind::AlreadyDefined(*name),
                                loc,
                            ));
                        }

                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                        self.scopes.current_mut().values.insert(*name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        return Err(CompileError::new(
                            CompileErrorKind::AlreadyDefined(*name),
                            loc,
                        ));
                    }

                    let def_id = self.gcx.definitions.insert(None, *name, DefKind::Type);
                    self.scopes.current_mut().types.insert(*name, def_id);
                }

                StmtKind::Expr(_) => {}
            }
        }

        Ok(())
    }

    /// Pass 2: Resolve names in statements
    fn resolve_stmt(&mut self, stmt_id: StmtId) -> Result<(), CompileError> {
        let stmt = self.ast.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let _loc = stmt.loc.clone();

        match &stmt_kind {
            StmtKind::Import { .. } => {
                // Already processed in collect_declarations
                Ok(())
            }

            StmtKind::Let { name, ty, value } => {
                // Resolve type annotation if present
                if let Some(type_id) = ty {
                    self.resolve_type(*type_id)?;
                }

                // Resolve the value expression
                self.resolve_expr(*value)?;

                // Get or create the DefId for this let binding
                let def_id =
                    if let Some(&existing_def_id) = self.scopes.current_mut().values.get(name) {
                        // Already registered (function forward declaration)
                        existing_def_id
                    } else {
                        // Register now
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                        self.scopes.current_mut().values.insert(*name, def_id);
                        def_id
                    };

                // Track stmt_id -> def_id mapping for lowering
                self.resolutions.let_bindings.insert(stmt_id, def_id);

                Ok(())
            }

            StmtKind::Type { name, ty } => {
                // Already registered in collect_declarations, just resolve the type
                self.resolve_type(*ty)?;

                // Extract type metadata (attributes) and store in GlobalContext
                let def_id = self.scopes.lookup_type(*name)
                    .expect("Type definition should exist in scope");

                if let Some(metadata) = self.extract_type_metadata(def_id, *ty) {
                    self.gcx.type_metadata.insert(def_id, std::sync::Arc::new(metadata));
                }

                Ok(())
            }

            StmtKind::Expr(expr) => self.resolve_expr(*expr),
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId) -> Result<(), CompileError> {
        let expr_kind = self.ast.exprs.get(expr_id).kind.clone();
        let loc = self.ast.exprs.get(expr_id).loc.clone();

        match expr_kind {
            ExprKind::Identifier(ref path) => {
                let def_id = self.resolve_value_path(path, loc)?;
                self.resolutions.exprs.insert(expr_id, def_id);
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

                let mut param_def_ids = Vec::new();
                for param in params {
                    let def_id = self.gcx.definitions.insert(None, param.name, DefKind::Let);
                    self.scopes.current_mut().values.insert(param.name, def_id);
                    param_def_ids.push(def_id);
                }

                // Store the parameter DefIds for this function
                self.resolutions
                    .function_params
                    .insert(expr_id, param_def_ids);

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

            ExprKind::FieldAccess { expr, .. } => {
                // Resolve the expression being accessed
                // Field name doesn't need resolution (it's a Symbol, not a Path)
                self.resolve_expr(expr)?;
            }

            ExprKind::Block { stmts } => {
                // Create new scope for block
                self.scopes.push();

                // Resolve all statements in the block
                for stmt_id in stmts {
                    self.resolve_stmt(stmt_id)?;
                }

                // Pop block scope
                self.scopes.pop();
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
                self.resolutions.types.insert(type_id, def_id);
            }

            TypeKind::App { ctor: _, args } => {
                // We don't resolve the type constructor path here
                // because it will be looked up in type_constructors during lowering
                // Just resolve the type arguments
                for arg in args {
                    self.resolve_type(arg)?;
                }
            }

            TypeKind::Function(params, ret) => {
                for param in params {
                    self.resolve_type(param)?;
                }
                self.resolve_type(ret)?;
            }

            TypeKind::List(inner) => self.resolve_type(inner)?,

            TypeKind::Record(fields) => {
                for field in fields {
                    self.resolve_type(field.ty)?;
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

                let simple_path = Path::Simple(name);
                if let Some(def_id) = self.gcx.definitions.resolve(&simple_path) {
                    return Ok(def_id);
                }

                Err(
                    CompileError::new(CompileErrorKind::UndefinedVariable { name }, loc)
                        .with_context(format!(
                            "Variable '{}' is not defined in the current scope",
                            self.gcx.interner.resolve(name)
                        )),
                )
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path: Path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(&path)
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

                let simple_path = Path::Simple(name);
                if let Some(def_id) = self.gcx.definitions.resolve(&simple_path) {
                    return Ok(def_id);
                }

                Err(
                    CompileError::new(CompileErrorKind::UndefinedType(Path::Simple(name)), loc)
                        .with_context(format!(
                            "Type '{}' is not defined in the current scope",
                            self.gcx.interner.resolve(name)
                        )),
                )
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path: Path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(&path)
                    .ok_or_else(|| CompileError::new(CompileErrorKind::UndefinedType(path), loc))
            }
        }
    }

    /// Extract type metadata from AST attributes
    ///
    /// Traverses the type definition and extracts metadata from attributes
    /// on record fields. This metadata is captured at compile-time and made
    /// available to runtime functions via RuntimeContext.
    ///
    /// # Arguments
    ///
    /// * `def_id` - The DefId of the type being defined
    /// * `type_id` - The AST TypeId to extract metadata from
    ///
    /// # Returns
    ///
    /// `Some(TypeMetadata)` if the type has any field metadata, `None` otherwise
    fn extract_type_metadata(&self, def_id: DefId, type_id: TypeId) -> Option<TypeMetadata> {
        let ty = self.ast.types.get(type_id);

        match &ty.kind {
            TypeKind::Record(fields) => {
                let mut metadata = TypeMetadata::new(def_id);

                for field in fields {
                    if !field.attrs.is_empty() {
                        let mut field_meta = FieldMetadata::new();

                        for attr in &field.attrs {
                            field_meta.attributes.push(AttributeData {
                                name: attr.name,
                                args: attr.args.clone(),
                            });
                        }

                        metadata.field_metadata.insert(field.name, field_meta);
                    }
                }

                if metadata.is_empty() {
                    None
                } else {
                    Some(metadata)
                }
            }
            _ => None,
        }
    }
}
