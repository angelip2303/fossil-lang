use std::collections::HashMap;

use crate::ast::ast::*;
use crate::context::{DefKind, Symbol};
use crate::error::{CompileError, CompileErrorKind, CompileErrors};
use crate::passes::GlobalContext;
use crate::traits::provider::ModuleSpec;

pub struct ProviderExpander {
    ast: Ast,
    gcx: GlobalContext,
    /// Const bindings collected from the AST for interpolation in provider args
    const_values: HashMap<Symbol, String>,
}

impl ProviderExpander {
    pub fn new((ast, gcx): (Ast, GlobalContext)) -> Self {
        Self { ast, gcx, const_values: HashMap::new() }
    }

    /// Collect const bindings with string literal values from the AST
    fn collect_const_bindings(&mut self) {
        for stmt_id in &self.ast.root {
            let stmt = self.ast.stmts.get(*stmt_id);
            if let StmtKind::Const { name, value } = &stmt.kind {
                let value_expr = self.ast.exprs.get(*value);
                if let ExprKind::Literal(Literal::String(s)) = &value_expr.kind {
                    let value_str = self.gcx.interner.resolve(*s).to_string();
                    self.const_values.insert(*name, value_str);
                }
            }
        }
    }

    /// Resolve ${CONST_NAME} interpolation patterns in a string using const bindings
    fn resolve_const_interpolation(&self, s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '$' && chars.peek() == Some(&'{') {
                chars.next(); // consume '{'
                let mut name = String::new();
                while let Some(&ch) = chars.peek() {
                    if ch == '}' {
                        chars.next(); // consume '}'
                        break;
                    }
                    name.push(ch);
                    chars.next();
                }
                // Look up the const value
                if let Some(sym) = self.gcx.interner.lookup(&name) {
                    if let Some(value) = self.const_values.get(&sym) {
                        result.push_str(value);
                        continue;
                    }
                }
                // Not found - keep original
                result.push_str(&format!("${{{}}}", name));
            } else {
                result.push(c);
            }
        }

        result
    }

    /// Expand type providers (following F# model)
    ///
    /// Type providers execute during compilation and generate AST types
    /// and optional modules (F# style).
    pub fn expand(mut self) -> Result<(Ast, GlobalContext), CompileErrors> {
        // Collect const bindings first so they can be used in provider args
        self.collect_const_bindings();
        // Collect all type alias statements with provider invocations
        let type_stmts: Vec<StmtId> = self
            .ast
            .root
            .iter()
            .copied()
            .filter(|&stmt_id| {
                let stmt = self.ast.stmts.get(stmt_id);
                match &stmt.kind {
                    StmtKind::Type { ty, .. } => {
                        let type_node = self.ast.types.get(*ty);
                        matches!(type_node.kind, TypeKind::Provider { .. })
                    }
                    _ => false,
                }
            })
            .collect();

        // Expand each provider (F# style: execute provider, get generated type + module)
        let mut errors = CompileErrors::new();
        for stmt_id in type_stmts {
            if let Err(e) = self.expand_provider_stmt(stmt_id) {
                errors.push(e);
            }
        }

        // Return errors if any occurred
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok((self.ast, self.gcx))
    }

    /// Expand a type alias statement with a provider
    ///
    /// Extracts the type name, executes the provider, generates the type,
    /// and creates a module if the provider specifies one.
    fn expand_provider_stmt(&mut self, stmt_id: StmtId) -> Result<(), CompileError> {
        // Extract type name and type_id from statement
        let (type_name, type_id) = {
            let stmt = self.ast.stmts.get(stmt_id);
            match &stmt.kind {
                StmtKind::Type { name, ty, .. } => (*name, *ty),
                _ => return Ok(()),
            }
        };

        // Get provider information from the type
        let (provider_path, args, loc) = {
            let ty = self.ast.types.get(type_id);
            match &ty.kind {
                TypeKind::Provider { provider, args } => {
                    (provider.clone(), args.clone(), ty.loc.clone())
                }
                _ => return Ok(()),
            }
        };

        // Resolve provider path to DefId
        let provider_def_id = self.gcx.definitions
            .resolve(&provider_path)
            .ok_or_else(|| CompileError::new(
                CompileErrorKind::UndefinedType(provider_path.clone()),
                loc.clone(),
            ))?;

        // Get the provider implementation
        let provider_impl = match &self.gcx.definitions.get(provider_def_id).kind {
            DefKind::Provider(provider) => provider.clone(),
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::UndefinedType(provider_path),
                    loc,
                ))
            }
        };

        // Resolve const references and interpolation in string arguments before passing to provider
        let mut resolved_args: Vec<ProviderArgument> = Vec::with_capacity(args.len());
        for arg in &args {
            let resolved = match arg {
                ProviderArgument::ConstRef(sym) => {
                    if let Some(value) = self.const_values.get(sym) {
                        let new_sym = self.gcx.interner.intern(value);
                        ProviderArgument::Positional(Literal::String(new_sym))
                    } else {
                        return Err(CompileError::new(
                            CompileErrorKind::UndefinedVariable { name: *sym },
                            loc.clone(),
                        ).with_context(
                            "Only `const` bindings with string literal values can be used as provider arguments"
                        ));
                    }
                }
                ProviderArgument::Positional(Literal::String(s)) => {
                    let original = self.gcx.interner.resolve(*s);
                    let resolved = self.resolve_const_interpolation(original);
                    if resolved != original {
                        let new_sym = self.gcx.interner.intern(&resolved);
                        ProviderArgument::Positional(Literal::String(new_sym))
                    } else {
                        arg.clone()
                    }
                }
                ProviderArgument::Named { name, value: Literal::String(s) } => {
                    let original = self.gcx.interner.resolve(*s);
                    let resolved = self.resolve_const_interpolation(original);
                    if resolved != original {
                        let new_sym = self.gcx.interner.intern(&resolved);
                        ProviderArgument::Named { name: *name, value: Literal::String(new_sym) }
                    } else {
                        arg.clone()
                    }
                }
                _ => arg.clone(),
            };
            resolved_args.push(resolved);
        }

        // Execute the provider to generate type + optional module (F# style)
        let type_name_str = self.gcx.interner.resolve(type_name).to_string();
        let provider_output = provider_impl.provide(
            &resolved_args,
            &mut self.ast,
            &mut self.gcx.interner,
            &type_name_str,
        )?;

        // Replace the Provider type node with the generated AST type
        // This is like syntax sugar: csv<"file.csv"> → { name: string, age: int, ... }
        let generated_type = self.ast.types.get(provider_output.generated_type);
        let generated_kind = generated_type.kind.clone();

        let ty_mut = self.ast.types.get_mut(type_id);
        ty_mut.kind = generated_kind;

        // If provider specified a module, create it
        if let Some(module_spec) = provider_output.module_spec {
            self.register_generated_module(type_name, module_spec)?;
        }

        Ok(())
    }

    /// Register a generated module in the GlobalContext
    ///
    /// Creates a module DefId and registers all functions as children.
    /// The module is automatically available globally (no import needed).
    fn register_generated_module(
        &mut self,
        module_name: Symbol,
        spec: ModuleSpec,
    ) -> Result<(), CompileError> {
        // Create module DefId
        let module_def_id = self.gcx.definitions.insert(
            None,
            module_name,
            DefKind::Mod {
                file_path: None,
                is_inline: true,
            },
        );

        // Register functions as children of this module
        for func_def in spec.functions {
            let func_sym = self.gcx.interner.intern(&func_def.name);
            self.gcx.definitions.insert(
                Some(module_def_id), // parent module
                func_sym,
                DefKind::Func(Some(func_def.implementation)),
            );
        }

        // Register submodules recursively (for future extensibility)
        for (submod_name, submod_spec) in spec.submodules {
            let submod_sym = self.gcx.interner.intern(&submod_name);

            // Create submodule with parent=module_def_id
            let submod_def_id = self.gcx.definitions.insert(
                Some(module_def_id),
                submod_sym,
                DefKind::Mod {
                    file_path: None,
                    is_inline: true,
                },
            );

            // Register its functions with parent=submod_def_id
            for func_def in submod_spec.functions {
                let func_sym = self.gcx.interner.intern(&func_def.name);
                self.gcx.definitions.insert(
                    Some(submod_def_id),
                    func_sym,
                    DefKind::Func(Some(func_def.implementation)),
                );
            }
        }

        Ok(())
    }
}
