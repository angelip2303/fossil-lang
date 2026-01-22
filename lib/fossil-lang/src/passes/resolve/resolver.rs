//! Name resolver implementation

use crate::ast::Loc;
use crate::ast::ast::*;
use crate::context::*;
use crate::error::{CompileError, CompileErrorKind, CompileErrors, ErrorSuggestion};
use crate::passes::GlobalContext;
use crate::suggestions;

use super::scope::ScopeStack;
use super::table::{ResolutionTable, ResolvedAst};

/// Name resolver - builds symbol tables and resolves all names
///
/// # Module Context for Relative Paths
/// For relative path resolution (e.g., `./utils`, `../common`), the current module
/// must be set using `set_module_context()`. This is typically done when resolving
/// multi-module projects where each module has its own context.
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

    /// Set the current module context for relative path resolution
    ///
    /// This should be called before resolving a module that may contain
    /// relative imports (e.g., `./utils`, `../common`).
    ///
    /// # Example
    /// ```ignore
    /// let mut resolver = NameResolver::new(ast, gcx);
    /// resolver.set_module_context(module_def_id);
    /// resolver.resolve()?;
    /// ```
    pub fn set_module_context(&mut self, module_id: DefId) {
        self.scopes.set_current_module(module_id);
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

        Ok(ResolvedAst {
            ast: self.ast,
            gcx: self.gcx,
            resolutions: self.resolutions,
        })
    }

    fn collect_declarations(&mut self) -> Result<(), CompileErrors> {
        // Collect declarations from root statements only
        let stmt_ids = self.ast.root.clone();
        let mut errors = CompileErrors::new();

        for stmt_id in stmt_ids {
            let stmt = self.ast.stmts.get(stmt_id);
            let loc = stmt.loc.clone();

            match &stmt.kind {
                StmtKind::Import { module, items, alias } => {
                    // Register the import in the scope
                    // If items are specified, we'll handle selective imports
                    // For now, just register the alias if present
                    if let Some(alias) = alias {
                        self.scopes
                            .current_mut()
                            .imports
                            .insert(*alias, module.clone());
                    }
                    // TODO: Handle selective imports { item1, item2 }
                    let _ = items; // Suppress unused warning for now
                }

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
            StmtKind::Import { .. } => {
                // Already processed in collect_declarations
            }

            StmtKind::Let { name, ty, value } => {
                // Resolve type annotation if present
                if let Some(type_id) = ty
                    && let Err(e) = self.resolve_type(*type_id) {
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
                    self.gcx.type_metadata.insert(type_def_id, std::sync::Arc::new(metadata));
                }

                // Generate constructor function for record types
                if let Err(e) = self.generate_record_constructor(*name, type_def_id, *ty) {
                    errors.push(e);
                }
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

                // Variable not found - create error with suggestions
                let name_str = self.gcx.interner.resolve(name);
                let mut error = CompileError::new(CompileErrorKind::UndefinedVariable { name }, loc)
                    .with_context(format!(
                        "Variable '{}' is not defined in the current scope",
                        name_str
                    ));

                // Collect available variable names in scope
                let available_names: Vec<String> = self
                    .scopes
                    .current()
                    .values
                    .keys()
                    .map(|sym| self.gcx.interner.resolve(*sym).to_string())
                    .collect();

                // Find similar names using edit distance
                if let Some((suggestion, confidence)) =
                    crate::suggestions::find_similar(name_str, &available_names)
                {
                    error = error.with_suggestion(ErrorSuggestion::DidYouMean {
                        wrong: name_str.to_string(),
                        suggestion,
                        confidence,
                    });
                }

                // TODO: Check if available in an unimported module (future enhancement)

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

            Path::Relative { dots, components } => {
                // Resolve relative to current module
                let base_module = self.navigate_up(*dots, loc.clone())?;
                self.resolve_from_module(base_module, components, loc, /* is_type */ false)
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

                // Type not found - create error with suggestions
                let name_str = self.gcx.interner.resolve(name);
                let mut error = CompileError::new(
                    CompileErrorKind::UndefinedType(Path::Simple(name)),
                    loc,
                )
                .with_context(format!(
                    "Type '{}' is not defined in the current scope",
                    name_str
                ));

                // Collect available type names in scope
                let available_types: Vec<String> = self
                    .scopes
                    .current()
                    .types
                    .keys()
                    .map(|sym| self.gcx.interner.resolve(*sym).to_string())
                    .collect();

                // Find similar type names using edit distance
                if let Some((suggestion, confidence)) =
                    crate::suggestions::find_similar(name_str, &available_types)
                {
                    error = error.with_suggestion(ErrorSuggestion::DidYouMean {
                        wrong: name_str.to_string(),
                        suggestion,
                        confidence,
                    });
                }

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

            Path::Relative { dots, components } => {
                // Resolve relative to current module
                let base_module = self.navigate_up(*dots, loc.clone())?;
                self.resolve_from_module(base_module, components, loc, /* is_type */ true)
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
                            self.validate_attribute(attr, AttributeTarget::Field, &ty.loc, errors);

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
                // Unknown attribute - create error with suggestion
                let attr_name = self.gcx.interner.resolve(attr.name);
                let mut error = CompileError::new(
                    CompileErrorKind::UnknownAttribute(attr.name),
                    loc.clone(),
                )
                .with_context(format!("Attribute '#[{}]' is not recognized", attr_name));

                // Find similar attribute names for suggestion
                let known_attrs = self.gcx.attribute_registry.all_names();
                if let Some((suggestion, confidence)) =
                    suggestions::find_similar(attr_name, &known_attrs)
                {
                    error = error.with_suggestion(ErrorSuggestion::DidYouMean {
                        wrong: attr_name.to_string(),
                        suggestion,
                        confidence,
                    });
                }

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
                    let mut error = CompileError::new(
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

                    // Find similar argument names for suggestion
                    let known_args = schema.arg_names();
                    if let Some((suggestion, confidence)) =
                        suggestions::find_similar(arg_name, &known_args)
                    {
                        error = error.with_suggestion(ErrorSuggestion::DidYouMean {
                            wrong: arg_name.to_string(),
                            suggestion,
                            confidence,
                        });
                    }

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
            self.scopes.current_mut().values.insert(type_name, constructor_def_id);

            // Store metadata linking constructor to its type
            // This will be used during type checking and code generation
            self.resolutions.record_constructors.insert(constructor_def_id, type_def_id);

            // Store field information for runtime constructor
            let field_names: Vec<Symbol> = fields.iter().map(|f| f.name).collect();
            self.resolutions.constructor_fields.insert(constructor_def_id, field_names);
        }

        Ok(())
    }

    /// Navigate up 'dots' levels in the module hierarchy
    ///
    /// # Arguments
    /// - `dots`: Number of levels to go up (0 = current module, 1 = parent, 2 = grandparent, etc.)
    /// - `loc`: Location for error reporting
    ///
    /// # Returns
    /// DefId of the target module after navigating up
    fn navigate_up(&self, dots: u8, loc: Loc) -> Result<DefId, CompileError> {
        // Get current module from scope
        let mut current = self.scopes.current_module().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("No current module context for relative import".into()),
                loc.clone(),
            )
        })?;

        // Navigate up 'dots' times
        for _ in 0..dots {
            let def = self.gcx.definitions.get(current);

            // Get parent module
            if let crate::context::DefKind::Mod { .. } = &def.kind {
                // Need to check the parent field
                // Since parent is private, we need to iterate through definitions
                // to find the module that contains this one
                let parent_id = self.gcx.definitions.iter()
                    .find(|parent_def| {
                        // Check if any of parent's children is current
                        self.gcx.definitions.get_children(parent_def.id())
                            .iter()
                            .any(|child| child.id() == current)
                    })
                    .map(|p| p.id());

                current = parent_id.ok_or_else(|| {
                    CompileError::new(
                        CompileErrorKind::Runtime(
                            "Relative import goes above project root".into()
                        ),
                        loc.clone(),
                    )
                })?;
            } else {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Not a module".into()),
                    loc,
                ));
            }
        }

        Ok(current)
    }

    /// Resolve a path from a specific module
    ///
    /// # Arguments
    /// - `module_id`: The module to resolve from
    /// - `components`: The path components to resolve
    /// - `loc`: Location for error reporting
    /// - `is_type`: Whether this is a type path (vs value path)
    fn resolve_from_module(
        &self,
        module_id: DefId,
        components: &[Symbol],
        loc: Loc,
        is_type: bool,
    ) -> Result<DefId, CompileError> {
        if components.is_empty() {
            return Ok(module_id);
        }

        // Start from the base module
        let mut current = module_id;

        // Navigate through each component
        for (i, &component) in components.iter().enumerate() {
            // Look for this component as a child of current
            let found = self.gcx.definitions.iter()
                .find(|def| {
                    def.name == component &&
                    self.gcx.definitions.get_children(current)
                        .iter()
                        .any(|child| child.id() == def.id())
                });

            if let Some(def) = found {
                current = def.id();

                // If this is not the last component, it must be a module
                if i < components.len() - 1 {
                    match &def.kind {
                        crate::context::DefKind::Mod { .. } => {
                            // Continue to next component
                        }
                        _ => {
                            return Err(CompileError::new(
                                CompileErrorKind::NotAModule(
                                    Path::qualified(components[..=i].to_vec())
                                ),
                                loc,
                            ));
                        }
                    }
                }
            } else {
                // Component not found
                let path = Path::qualified(components.to_vec());
                return Err(if is_type {
                    CompileError::new(CompileErrorKind::UndefinedType(path), loc)
                } else {
                    CompileError::new(CompileErrorKind::UndefinedPath { path }, loc)
                });
            }
        }

        Ok(current)
    }
}
