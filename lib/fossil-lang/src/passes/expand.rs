use std::collections::HashMap;

use crate::ast::ast::*;
use crate::context::{DefKind, Symbol};
use crate::error::{FossilError, FossilErrors, FossilWarnings};
use crate::passes::GlobalContext;
use crate::traits::provider::ModuleSpec;

/// Result of provider expansion including warnings
pub struct ExpandResult {
    pub ast: Ast,
    pub gcx: GlobalContext,
    pub warnings: FossilWarnings,
}

pub struct ProviderExpander {
    ast: Ast,
    gcx: GlobalContext,
    /// Const bindings collected from the AST for interpolation in provider args
    const_values: HashMap<Symbol, String>,
    /// Warnings collected during expansion
    warnings: FossilWarnings,
}

impl ProviderExpander {
    pub fn new((ast, gcx): (Ast, GlobalContext)) -> Self {
        Self {
            ast,
            gcx,
            const_values: HashMap::new(),
            warnings: FossilWarnings::new(),
        }
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
                if let Some(value) = self
                    .gcx
                    .interner
                    .lookup(&name)
                    .and_then(|sym| self.const_values.get(&sym))
                {
                    result.push_str(value);
                    continue;
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
    pub fn expand(mut self) -> Result<ExpandResult, FossilErrors> {
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
        let mut errors = FossilErrors::new();
        for stmt_id in type_stmts {
            if let Err(e) = self.expand_provider_stmt(stmt_id) {
                errors.push(e);
            }
        }

        // Return errors if any occurred
        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(ExpandResult {
            ast: self.ast,
            gcx: self.gcx,
            warnings: self.warnings,
        })
    }

    /// Expand a type alias statement with a provider
    ///
    /// Extracts the type name, executes the provider, generates the type,
    /// and creates a module if the provider specifies one.
    fn expand_provider_stmt(&mut self, stmt_id: StmtId) -> Result<(), FossilError> {
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
                TypeKind::Provider { provider, args } => (provider.clone(), args.clone(), ty.loc),
                _ => return Ok(()),
            }
        };

        // Resolve provider path to DefId
        let provider_def_id = self
            .gcx
            .definitions
            .resolve(&provider_path)
            .ok_or_else(|| {
                let path_str = format!("{:?}", provider_path);
                FossilError::undefined_type(path_str, loc)
            })?;

        // Get the provider implementation
        let provider_impl = match &self.gcx.definitions.get(provider_def_id).kind {
            DefKind::Provider(provider) => provider.clone(),
            _ => {
                let path_str = format!("{:?}", provider_path);
                return Err(FossilError::not_a_provider(path_str, loc));
            }
        };

        // Resolve const references and interpolation in string arguments before passing to provider
        let mut resolved_args: Vec<ProviderArgument> = Vec::with_capacity(args.len());
        for arg in &args {
            let resolved = match arg {
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
                ProviderArgument::Named {
                    name,
                    value: Literal::String(s),
                } => {
                    let original = self.gcx.interner.resolve(*s);
                    let resolved = self.resolve_const_interpolation(original);
                    if resolved != original {
                        let new_sym = self.gcx.interner.intern(&resolved);
                        ProviderArgument::Named {
                            name: *name,
                            value: Literal::String(new_sym),
                        }
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
            loc,
        )?;

        // Collect warnings from provider
        self.warnings.extend(provider_output.warnings);

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
    ) -> Result<(), FossilError> {
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
                DefKind::Func(func_def.implementation),
            );
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passes::parse::Parser;

    fn expand_ok(src: &str) -> ExpandResult {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed")
    }

    fn expand_err(src: &str) -> crate::error::FossilErrors {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        match ProviderExpander::new((parsed.ast, parsed.gcx)).expand() {
            Err(errors) => errors,
            Ok(_) => panic!("expected expand error"),
        }
    }

    // ── 1. collect_const_bindings_string ────────────────────────

    #[test]
    fn collect_const_bindings_string() {
        let src = r#"const ROOT = "http://example.org""#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let mut expander = ProviderExpander::new((parsed.ast, parsed.gcx));
        expander.collect_const_bindings();

        let root_sym = expander
            .gcx
            .interner
            .lookup("ROOT")
            .expect("ROOT not interned");
        assert!(
            expander.const_values.contains_key(&root_sym),
            "const_values should contain ROOT"
        );
        assert_eq!(expander.const_values[&root_sym], "http://example.org");
    }

    // ── 2. resolve_const_interpolation_basic ────────────────────

    #[test]
    fn resolve_const_interpolation_basic() {
        let src = r#"const ROOT = "http://ex.org""#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let mut expander = ProviderExpander::new((parsed.ast, parsed.gcx));
        expander.collect_const_bindings();

        let result = expander.resolve_const_interpolation("${ROOT}/file");
        assert_eq!(result, "http://ex.org/file");
    }

    // ── 3. attribute_preserved_through_expand ───────────────────

    #[test]
    fn attribute_preserved_through_expand() {
        // Attributes on types should be preserved unchanged after expansion
        // (no provider involved, just passthrough)
        let src = concat!(
            "#[rdf(type = \"http://example.org/Person\")]\n",
            "type Person(subject: string) = {\n",
            "    #[rdf(uri = \"http://xmlns.com/foaf/0.1/name\")]\n",
            "    Name: string\n",
            "}\n",
        );
        let result = expand_ok(src);

        // Find the Type statement
        let type_stmt_id = result
            .ast
            .root
            .iter()
            .find(|&&id| matches!(result.ast.stmts.get(id).kind, StmtKind::Type { .. }))
            .expect("should have a Type statement");

        let stmt = result.ast.stmts.get(*type_stmt_id);
        if let StmtKind::Type { attrs, ty, .. } = &stmt.kind {
            // Check type-level attribute is preserved
            assert_eq!(attrs.len(), 1, "type should have 1 attribute");
            assert_eq!(result.gcx.interner.resolve(attrs[0].name), "rdf");
            assert_eq!(attrs[0].args.len(), 1);
            assert_eq!(result.gcx.interner.resolve(attrs[0].args[0].key), "type");
            match &attrs[0].args[0].value {
                Literal::String(s) => {
                    assert_eq!(
                        result.gcx.interner.resolve(*s),
                        "http://example.org/Person"
                    );
                }
                other => panic!("expected String literal, got {:?}", other),
            }

            // Check field-level attribute is preserved
            let ty_node = result.ast.types.get(*ty);
            if let TypeKind::Record(fields) = &ty_node.kind {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].attrs.len(), 1);
                assert_eq!(
                    result.gcx.interner.resolve(fields[0].attrs[0].name),
                    "rdf"
                );
            } else {
                panic!("expected Record type");
            }
        } else {
            panic!("expected Type statement");
        }
    }

    // ── 4. noop_without_providers ───────────────────────────────

    #[test]
    fn noop_without_providers() {
        let src = concat!(
            "const x = \"hello\"\n",
            "type T = { Name: string }\n",
        );
        let result = expand_ok(src);

        // Should have 2 statements: const + type
        assert_eq!(result.ast.root.len(), 2);

        // The type should still be a Record
        let type_stmt_id = result
            .ast
            .root
            .iter()
            .find(|&&id| matches!(result.ast.stmts.get(id).kind, StmtKind::Type { .. }))
            .expect("should have a Type statement");

        let stmt = result.ast.stmts.get(*type_stmt_id);
        if let StmtKind::Type { ty, .. } = &stmt.kind {
            let ty_node = result.ast.types.get(*ty);
            assert!(
                matches!(ty_node.kind, TypeKind::Record(_)),
                "type should remain Record after expand, got {:?}",
                ty_node.kind
            );
        } else {
            panic!("expected Type statement");
        }
    }

    // ── 5. unknown_provider_error ───────────────────────────────

    #[test]
    fn unknown_provider_error() {
        let src = r#"type X = unknown!("arg")"#;
        let errors = expand_err(src);
        assert!(!errors.is_empty(), "should produce at least one error");

        // The error should be about an undefined type (provider not found)
        let has_undefined = errors
            .0
            .iter()
            .any(|e| matches!(e, crate::error::FossilError::UndefinedType { .. }));
        assert!(
            has_undefined,
            "expected UndefinedType error for unknown provider, got: {:?}",
            errors
        );
    }

    // ── 6. no_interpolation_without_dollar ──────────────────────

    #[test]
    fn no_interpolation_without_dollar() {
        let src = r#"const ROOT = "http://example.org""#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let mut expander = ProviderExpander::new((parsed.ast, parsed.gcx));
        expander.collect_const_bindings();

        let input = "plain string without interpolation";
        let result = expander.resolve_const_interpolation(input);
        assert_eq!(result, input, "string without ${{}} should be unchanged");
    }

    // ── 7. multiple_const_bindings ──────────────────────────────

    #[test]
    fn multiple_const_bindings() {
        let src = concat!(
            "const BASE = \"http://example.org\"\n",
            "const PREFIX = \"ex\"\n",
            "let count = 42\n", // let binding, not const — should NOT be collected
        );
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let mut expander = ProviderExpander::new((parsed.ast, parsed.gcx));
        expander.collect_const_bindings();

        // String const bindings should be collected
        let base_sym = expander
            .gcx
            .interner
            .lookup("BASE")
            .expect("BASE not interned");
        let prefix_sym = expander
            .gcx
            .interner
            .lookup("PREFIX")
            .expect("PREFIX not interned");

        assert_eq!(expander.const_values[&base_sym], "http://example.org");
        assert_eq!(expander.const_values[&prefix_sym], "ex");

        // `let` binding should NOT be collected (only `const` bindings are)
        let count_sym = expander
            .gcx
            .interner
            .lookup("count")
            .expect("count not interned");
        assert!(
            !expander.const_values.contains_key(&count_sym),
            "let binding should not be collected as a const value"
        );
    }

    // ── 8. expand_preserves_non_provider_types ──────────────────

    #[test]
    fn expand_preserves_non_provider_types() {
        let src = "type Person = { Name: string, Age: int }";
        let result = expand_ok(src);
        assert_eq!(result.ast.root.len(), 1);

        let stmt = result.ast.stmts.get(result.ast.root[0]);
        if let StmtKind::Type { name, ty, .. } = &stmt.kind {
            assert_eq!(result.gcx.interner.resolve(*name), "Person");
            let ty_node = result.ast.types.get(*ty);
            match &ty_node.kind {
                TypeKind::Record(fields) => {
                    assert_eq!(fields.len(), 2, "Person should have 2 fields");
                    assert_eq!(result.gcx.interner.resolve(fields[0].name), "Name");
                    assert_eq!(result.gcx.interner.resolve(fields[1].name), "Age");
                }
                other => panic!("expected Record type, got {:?}", other),
            }
        } else {
            panic!("expected Type statement");
        }
    }
}
