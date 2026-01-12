use crate::ast::ast::*;
use crate::context::{DefKind, Symbol};
use crate::error::{CompileError, CompileErrorKind};
use crate::passes::GlobalContext;
use crate::traits::provider::ModuleSpec;

pub struct ProviderExpander {
    ast: Ast,
    gcx: GlobalContext,
}

impl ProviderExpander {
    pub fn new((ast, gcx): (Ast, GlobalContext)) -> Self {
        Self { ast, gcx }
    }

    /// Expand type providers (following F# model)
    ///
    /// Type providers execute during compilation and generate AST types
    /// and optional modules (F# style).
    pub fn expand(mut self) -> Result<(Ast, GlobalContext), CompileError> {
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
        for stmt_id in type_stmts {
            self.expand_provider_stmt(stmt_id)?;
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
                StmtKind::Type { name, ty } => (*name, *ty),
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

        // Execute the provider to generate type + optional module (F# style)
        let provider_output = provider_impl.provide(
            &args,
            &mut self.ast,
            &mut self.gcx.interner,
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
            DefKind::Mod,
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
                DefKind::Mod,
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
