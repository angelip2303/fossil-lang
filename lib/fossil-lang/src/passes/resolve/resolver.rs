//! Name resolver implementation

use std::collections::HashMap;

use crate::ast::Loc;
use crate::ast::ast::*;
use crate::context::*;
use crate::error::{CompileError, CompileErrorKind, CompileErrors};
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
        Self {
            gcx,
            ast,
            resolutions: ResolutionTable::default(),
            scopes: ScopeStack::new(),
        }
    }

    pub fn resolve(mut self) -> Result<ResolvedAst, CompileErrors> {
        let mut errors = CompileErrors::new();

        // Phase 1: Collect declarations
        if let Err(mut decl_errors) = self.collect_declarations() {
            // Merge declaration errors
            errors.0.append(&mut decl_errors.0);
            // If collection fails, we can't proceed with resolution
            return Err(errors);
        }

        // Phase 2: Resolve root statements (block statements are resolved when their block is resolved)
        let stmt_ids = self.ast.root.clone();
        for stmt_id in stmt_ids {
            self.resolve_stmt(stmt_id, &mut errors);
        }

        // Return errors if any occurred
        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 3: Desugar pipe expressions in-place
        self.desugar_pipes();

        Ok(ResolvedAst {
            ast: self.ast,
            gcx: self.gcx,
            resolutions: self.resolutions,
        })
    }

    /// Desugar pipe expressions in-place
    ///
    /// Transforms:
    /// - `a |> b` → `b(a)`
    /// - `a |> f(x, y)` → `f(a, x, y)`
    fn desugar_pipes(&mut self) {
        // Collect all expression IDs that need to be processed
        let expr_ids: Vec<ExprId> = self.ast.exprs.iter().map(|(id, _)| id).collect();

        for expr_id in expr_ids {
            self.desugar_pipe_expr(expr_id);
        }
    }

    /// Desugar a single pipe expression if it is one
    fn desugar_pipe_expr(&mut self, expr_id: ExprId) {
        // Get the expression kind
        let expr = self.ast.exprs.get(expr_id);
        let kind = expr.kind.clone();

        if let ExprKind::Pipe { lhs, rhs } = kind {
            // Check if RHS is an Application
            let rhs_expr = self.ast.exprs.get(rhs);
            let rhs_kind = rhs_expr.kind.clone();

            let new_kind = match rhs_kind {
                ExprKind::Application { callee, args } => {
                    // RHS is already a function call: prepend LHS as first argument
                    let mut new_args = vec![Argument::Positional(lhs)];
                    new_args.extend(args);
                    ExprKind::Application {
                        callee,
                        args: new_args,
                    }
                }
                _ => {
                    // RHS is not a function call: simple pipe desugaring
                    ExprKind::Application {
                        callee: rhs,
                        args: vec![Argument::Positional(lhs)],
                    }
                }
            };

            // Modify the expression in place
            let expr_mut = self.ast.exprs.get_mut(expr_id);
            expr_mut.kind = new_kind;
        }
    }

    fn collect_declarations(&mut self) -> Result<(), CompileErrors> {
        // Collect declarations from root statements only
        let stmt_ids = self.ast.root.clone();
        let mut errors = CompileErrors::new();

        for stmt_id in stmt_ids {
            let stmt = self.ast.stmts.get(stmt_id);
            let loc = stmt.loc.clone();

            match &stmt.kind {
                StmtKind::Let { name, ty: _, value } => {
                    let expr = self.ast.exprs.get(*value);
                    if matches!(expr.kind, ExprKind::Function { .. }) {
                        if self.scopes.current_mut().values.contains_key(name) {
                            errors.push(CompileError::new(
                                CompileErrorKind::AlreadyDefined(*name),
                                loc,
                            ));
                        } else {
                            let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                            self.scopes.current_mut().values.insert(*name, def_id);
                        }
                    }
                }

                StmtKind::Const { name, .. } => {
                    if self.scopes.current_mut().values.contains_key(name) {
                        errors.push(CompileError::new(
                            CompileErrorKind::AlreadyDefined(*name),
                            loc,
                        ));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Const);
                        self.scopes.current_mut().values.insert(*name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        errors.push(CompileError::new(
                            CompileErrorKind::AlreadyDefined(*name),
                            loc,
                        ));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Type);
                        self.scopes.current_mut().types.insert(*name, def_id);
                    }
                }

                StmtKind::Trait { name, methods } => {
                    let method_names: Vec<_> = methods.iter().map(|m| m.name).collect();
                    let def_id = self.gcx.definitions.insert(
                        None,
                        *name,
                        DefKind::Trait {
                            methods: method_names,
                        },
                    );
                    self.scopes.current_mut().types.insert(*name, def_id);

                    // Register each method as a child definition
                    for method in methods {
                        let method_def_id = self.gcx.definitions.insert(
                            Some(def_id),
                            method.name,
                            DefKind::TraitMethod,
                        );
                        self.resolutions
                            .trait_methods
                            .insert((*name, method.name), method_def_id);
                    }

                    // Track trait def for lowering
                    self.resolutions.trait_defs.insert(stmt_id, def_id);
                }

                StmtKind::Impl { .. } => {
                    // Impl blocks don't introduce new names into scope
                    // They are processed in resolve_stmt
                }

                StmtKind::Expr(_) => {}
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Pass 2: Resolve names in statements
    fn resolve_stmt(&mut self, stmt_id: StmtId, errors: &mut CompileErrors) {
        let stmt = self.ast.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let _loc = stmt.loc.clone();

        match &stmt_kind {
            StmtKind::Let { name, ty, value } => {
                // Resolve type annotation if present
                if let Some(type_id) = ty
                    && let Err(e) = self.resolve_type(*type_id)
                {
                    errors.push(e);
                }

                // Resolve the value expression
                if let Err(e) = self.resolve_expr(*value) {
                    errors.push(e);
                }

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
            }

            StmtKind::Const { name, value } => {
                // Resolve the value expression
                if let Err(e) = self.resolve_expr(*value) {
                    errors.push(e);
                }

                // Get DefId from collect phase
                let def_id = self.scopes.lookup_value(*name)
                    .expect("SAFETY: Const was registered in collect_declarations(). \
                             The lookup cannot fail immediately after successful declaration collection.");

                // Track stmt_id -> def_id mapping for lowering
                self.resolutions.const_bindings.insert(stmt_id, def_id);
            }

            StmtKind::Type { name, ty } => {
                // Already registered in collect_declarations, just resolve the type
                if let Err(e) = self.resolve_type(*ty) {
                    errors.push(e);
                }

                // Extract type metadata (attributes) and store in GlobalContext
                let type_def_id = self.scopes.lookup_type(*name)
                    .expect("SAFETY: Type definition was just added to scope in collect_declarations(). \
                             The lookup cannot fail immediately after successful declaration collection.");

                if let Some(metadata) = self.extract_type_metadata(type_def_id, *ty, errors) {
                    self.gcx
                        .type_metadata
                        .insert(type_def_id, std::sync::Arc::new(metadata));
                }

                // Generate constructor function for record types
                if let Err(e) = self.generate_record_constructor(*name, type_def_id, *ty) {
                    errors.push(e);
                }
            }

            StmtKind::Trait { name, methods } => {
                // Add "self" as a type in scope for trait method signatures
                // It resolves to the trait's own DefId as a placeholder
                let self_sym = self.gcx.interner.intern("self");
                let trait_def_id = self
                    .scopes
                    .lookup_type(*name)
                    .expect("Trait should be declared before resolution");
                self.scopes
                    .current_mut()
                    .types
                    .insert(self_sym, trait_def_id);

                // Resolve method type signatures
                for method in methods {
                    if let Err(e) = self.resolve_type(method.ty) {
                        errors.push(e);
                    }
                }

                // Remove "self" from scope after resolving trait methods
                self.scopes.current_mut().types.remove(&self_sym);
            }

            StmtKind::Impl {
                trait_name,
                type_name,
                methods,
            } => {
                // Resolve trait name
                let trait_def_id = match self.resolve_type_path(trait_name, _loc.clone()) {
                    Ok(id) => id,
                    Err(e) => {
                        errors.push(e);
                        return;
                    }
                };

                // Resolve type name
                let type_def_id = match self.resolve_type_path(type_name, _loc.clone()) {
                    Ok(id) => id,
                    Err(e) => {
                        errors.push(e);
                        return;
                    }
                };

                // Validate that trait_def_id is actually a trait
                let trait_def = self.gcx.definitions.get(trait_def_id);
                if let DefKind::Trait {
                    methods: trait_methods,
                } = &trait_def.kind
                {
                    // Check all required methods are implemented
                    for required in trait_methods {
                        if !methods.iter().any(|(name, _)| name == required) {
                            errors.push(CompileError::new(
                                CompileErrorKind::UndefinedVariable { name: *required },
                                _loc.clone(),
                            ));
                        }
                    }
                }

                // Resolve method expressions
                for (_, expr) in methods {
                    if let Err(e) = self.resolve_expr(*expr) {
                        errors.push(e);
                    }
                }

                // Register impl
                let mut impl_methods = HashMap::new();
                for (name, _) in methods {
                    let method_def_id =
                        self.gcx
                            .definitions
                            .insert(Some(type_def_id), *name, DefKind::Func(None));
                    impl_methods.insert(*name, method_def_id);
                }

                self.gcx.trait_impls.insert(
                    (trait_def_id, type_def_id),
                    crate::passes::TraitImplInfo {
                        methods: impl_methods,
                    },
                );

                // Track for lowering
                self.resolutions
                    .impl_trait_defs
                    .insert(stmt_id, trait_def_id);
                self.resolutions.impl_type_defs.insert(stmt_id, type_def_id);
            }

            StmtKind::Expr(expr) => {
                if let Err(e) = self.resolve_expr(*expr) {
                    errors.push(e);
                }
            }
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

                    // Resolve type annotation if present
                    if let Some(ty_id) = param.ty {
                        self.resolve_type(ty_id)?;
                    }

                    // Resolve default value if present
                    if let Some(default_expr) = param.default {
                        self.resolve_expr(default_expr)?;
                    }
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
                // Resolve each argument's value (whether positional or named)
                for arg in args {
                    self.resolve_expr(arg.value())?;
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
                // Collect errors to return the first one encountered
                let mut block_errors = CompileErrors::new();
                for stmt_id in stmts {
                    self.resolve_stmt(stmt_id, &mut block_errors);
                }

                // Pop block scope
                self.scopes.pop();

                // Return first error if any
                if !block_errors.is_empty() {
                    return Err(block_errors.0.into_iter().next().unwrap());
                }
            }

            ExprKind::Unit | ExprKind::Literal(_) | ExprKind::Placeholder => {}

            ExprKind::StringInterpolation { parts: _, exprs } => {
                // Resolve all interpolated expressions
                for expr in exprs {
                    self.resolve_expr(expr)?;
                }
            }
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

                // Variable not found
                let name_str = self.gcx.interner.resolve(name);
                let error = CompileError::new(CompileErrorKind::UndefinedVariable { name }, loc)
                    .with_context(format!(
                        "Variable '{}' is not defined in the current scope",
                        name_str
                    ));

                Err(error)
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path: Path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(&path)
                    .ok_or_else(|| CompileError::new(CompileErrorKind::UndefinedPath { path }, loc))
            }

            Path::Relative { .. } => {
                // Relative paths are not supported
                Err(CompileError::new(
                    CompileErrorKind::Runtime("Relative paths are not supported".into()),
                    loc,
                ))
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

                // Type not found
                let name_str = self.gcx.interner.resolve(name);
                let error =
                    CompileError::new(CompileErrorKind::UndefinedType(Path::Simple(name)), loc)
                        .with_context(format!(
                            "Type '{}' is not defined in the current scope",
                            name_str
                        ));

                Err(error)
            }

            Path::Qualified(parts) => {
                let parts = parts.clone();
                let path: Path = parts.clone().into();
                self.gcx
                    .definitions
                    .resolve(&path)
                    .ok_or_else(|| CompileError::new(CompileErrorKind::UndefinedType(path), loc))
            }

            Path::Relative { .. } => {
                // Relative paths are not supported
                Err(CompileError::new(
                    CompileErrorKind::Runtime("Relative paths are not supported".into()),
                    loc,
                ))
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
    /// * `errors` - Mutable reference to collect validation errors
    ///
    /// # Returns
    ///
    /// `Some(TypeMetadata)` if the type has any field metadata, `None` otherwise
    fn extract_type_metadata(
        &self,
        def_id: DefId,
        type_id: TypeId,
        errors: &mut CompileErrors,
    ) -> Option<TypeMetadata> {
        let ty = self.ast.types.get(type_id);

        match &ty.kind {
            TypeKind::Record(fields) => {
                let mut metadata = TypeMetadata::new(def_id);

                for field in fields {
                    if !field.attrs.is_empty() {
                        let mut field_meta = FieldMetadata::new();

                        for attr in &field.attrs {
                            // Validate the attribute against its schema
                            self.validate_attribute(
                                attr,
                                AttributeTarget::Field,
                                &attr.loc,
                                errors,
                            );

                            // Convert Vec<AttributeArg> to HashMap<Symbol, Literal>
                            let args = attr
                                .args
                                .iter()
                                .map(|arg| (arg.key, arg.value.clone()))
                                .collect();

                            field_meta.attributes.push(AttributeData {
                                name: attr.name,
                                args,
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

    /// Validate a single attribute against its schema
    ///
    /// Checks:
    /// 1. Attribute name is registered
    /// 2. Attribute target is valid
    /// 3. All required arguments are present
    /// 4. All argument types are correct
    /// 5. No unknown arguments
    fn validate_attribute(
        &self,
        attr: &Attribute,
        target: AttributeTarget,
        loc: &Loc,
        errors: &mut CompileErrors,
    ) {
        // Look up the attribute schema
        let schema = match self.gcx.attribute_registry.get(attr.name) {
            Some(s) => s,
            None => {
                // Unknown attribute
                let attr_name = self.gcx.interner.resolve(attr.name);
                let error =
                    CompileError::new(CompileErrorKind::UnknownAttribute(attr.name), loc.clone())
                        .with_context(format!("Attribute '#[{}]' is not recognized", attr_name));

                errors.push(error);
                return;
            }
        };

        // Check if attribute can be applied to this target
        if !schema.target.allows(target) {
            errors.push(
                CompileError::new(
                    CompileErrorKind::InvalidAttributeTarget {
                        attr: attr.name,
                        actual_target: target.name(),
                        expected_target: schema.target.name(),
                    },
                    loc.clone(),
                )
                .with_context(format!(
                    "Attribute '#[{}]' can only be applied to {} definitions",
                    schema.name,
                    schema.target.name()
                )),
            );
        }

        // Collect provided argument names
        let provided_args: std::collections::HashSet<Symbol> =
            attr.args.iter().map(|arg| arg.key).collect();

        // Check for missing required arguments
        for required_arg in schema.required_args() {
            let required_sym = self.gcx.interner.lookup(required_arg);
            let is_provided = required_sym.is_some_and(|sym| provided_args.contains(&sym));

            if !is_provided {
                errors.push(
                    CompileError::new(
                        CompileErrorKind::MissingAttributeArg {
                            attr: attr.name,
                            arg: required_arg,
                        },
                        loc.clone(),
                    )
                    .with_context(format!(
                        "Attribute '#[{}]' requires argument '{}'",
                        schema.name, required_arg
                    )),
                );
            }
        }

        // Validate each provided argument
        for arg in &attr.args {
            let arg_name = self.gcx.interner.resolve(arg.key);

            match schema.get_arg(arg_name) {
                Some(spec) => {
                    // Check type
                    if !spec.ty.matches(&arg.value) {
                        let actual_type = match &arg.value {
                            Literal::String(_) => "string",
                            Literal::Integer(_) => "int",
                            Literal::Boolean(_) => "bool",
                        };

                        errors.push(
                            CompileError::new(
                                CompileErrorKind::AttributeArgTypeMismatch {
                                    attr: attr.name,
                                    arg: arg.key,
                                    expected: spec.ty.name(),
                                    actual: actual_type,
                                },
                                loc.clone(),
                            )
                            .with_context(format!(
                                "Argument '{}' expects {} value, got {}",
                                arg_name,
                                spec.ty.name(),
                                actual_type
                            )),
                        );
                    }
                }
                None => {
                    // Unknown argument - create error with suggestion
                    let error = CompileError::new(
                        CompileErrorKind::UnknownAttributeArg {
                            attr: attr.name,
                            arg: arg.key,
                        },
                        loc.clone(),
                    )
                    .with_context(format!(
                        "Unknown argument '{}' for attribute '#[{}]'",
                        arg_name, schema.name
                    ));

                    errors.push(error);
                }
            }
        }
    }

    /// Generate a constructor function for record types
    ///
    /// For record types like `type Person = { name: string, age: int }`,
    /// this generates a constructor function `Person(name, age)` that creates
    /// record instances.
    ///
    /// # Arguments
    /// * `type_name` - The name of the type (e.g., "Person")
    /// * `type_def_id` - The DefId of the type definition
    /// * `type_id` - The TypeId of the type definition
    ///
    /// # Returns
    /// Ok if constructor was generated (or type is not a record), Err if there's a conflict
    fn generate_record_constructor(
        &mut self,
        type_name: Symbol,
        type_def_id: DefId,
        type_id: TypeId,
    ) -> Result<(), CompileError> {
        let ty = self.ast.types.get(type_id);

        // Only generate constructors for record types
        if let TypeKind::Record(fields) = &ty.kind {
            // Check if constructor name already exists in value scope
            if self.scopes.current().values.contains_key(&type_name) {
                // Constructor name conflicts with existing value binding
                // This is OK - we'll skip constructor generation
                // The type can still be constructed with record literals
                return Ok(());
            }

            // Register constructor function with placeholder implementation
            // Phase 3.2 will provide the actual runtime implementation
            let constructor_def_id = self.gcx.definitions.insert(
                None,
                type_name,
                DefKind::Func(None), // Will be filled in Phase 3.2
            );

            // Add constructor to value scope (not type scope)
            self.scopes
                .current_mut()
                .values
                .insert(type_name, constructor_def_id);

            // Store metadata linking constructor to its type
            // This will be used during type checking and code generation
            self.resolutions
                .record_constructors
                .insert(constructor_def_id, type_def_id);

            // Store field information for runtime constructor
            let field_names: Vec<Symbol> = fields.iter().map(|f| f.name).collect();
            self.resolutions
                .constructor_fields
                .insert(constructor_def_id, field_names);
        }

        Ok(())
    }

}
