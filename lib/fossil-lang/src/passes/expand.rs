use std::collections::HashSet;

use crate::ast::*;
use crate::context::{DefKind, Interner, Symbol};
use crate::context::global::TypeInfo;
use crate::error::{FossilError, FossilErrors, FossilWarnings};
use crate::passes::GlobalContext;
use crate::traits::provider::{
    CachingFileReader, FieldType, ModuleSpec, ProviderArgs, ProviderContext, ProviderKind,
    ProviderSchema, resolve_to_provider_args,
};

pub struct ExpandResult {
    pub ast: Ast,
    pub gcx: GlobalContext,
    pub warnings: FossilWarnings,
}

struct ResolvedProvider {
    imp: std::sync::Arc<dyn crate::traits::provider::TypeProviderImpl>,
    args: ProviderArgs,
    name: String,
    kind: ProviderKind,
}

pub struct ProviderExpander {
    ast: Ast,
    gcx: GlobalContext,
    warnings: FossilWarnings,
}

fn field_type_to_ast(ft: &FieldType, ast: &mut Ast, interner: &mut Interner, loc: Loc) -> TypeId {
    match ft {
        FieldType::Primitive(p) => ast.types.alloc(Type {
            loc,
            kind: TypeKind::Primitive(*p),
        }),
        FieldType::Optional(inner) => {
            let inner_id = field_type_to_ast(inner, ast, interner, loc);
            ast.types.alloc(Type {
                loc,
                kind: TypeKind::Optional(inner_id),
            })
        }
        FieldType::Named(name) => {
            let sym = interner.intern(name);
            ast.types.alloc(Type {
                loc,
                kind: TypeKind::Named(Path::simple(sym)),
            })
        }
    }
}

fn schema_to_ast(schema: &ProviderSchema, ast: &mut Ast, interner: &mut Interner, loc: Loc) -> TypeId {
    let fields = schema
        .fields
        .iter()
        .map(|f| RecordField {
            name: f.name,
            ty: field_type_to_ast(&f.ty, ast, interner, loc),
            attrs: f.attrs.clone(),
        })
        .collect();
    ast.types.alloc(Type {
        loc,
        kind: TypeKind::Record(fields),
    })
}

fn build_provider_attribute(
    interner: &mut Interner,
    provider_name: &str,
    kind: ProviderKind,
    loc: Loc,
) -> Attribute {
    let attr_name = interner.intern("provider");
    let name_key = interner.intern("name");
    let name_value = interner.intern(provider_name);
    let kind_key = interner.intern("kind");
    let kind_value = interner.intern(match kind {
        ProviderKind::Schema => "schema",
        ProviderKind::Data => "data",
        ProviderKind::Both => "both",
    });
    Attribute {
        name: attr_name,
        args: vec![
            AttributeArg::Named { key: name_key, value: Literal::String(name_value) },
            AttributeArg::Named { key: kind_key, value: Literal::String(kind_value) },
        ],
        loc,
    }
}

impl ProviderExpander {
    pub fn new((ast, gcx): (Ast, GlobalContext)) -> Self {
        Self {
            ast,
            gcx,
            warnings: FossilWarnings::new(),
        }
    }

    pub fn expand(mut self) -> Result<ExpandResult, FossilErrors> {
        let provider_stmts: Vec<StmtId> = self
            .ast
            .root
            .iter()
            .copied()
            .filter(|&stmt_id| {
                matches!(self.ast.stmts.get(stmt_id).kind, StmtKind::ProviderType { .. })
            })
            .collect();

        let let_stmts: Vec<StmtId> = self
            .ast
            .root
            .iter()
            .copied()
            .filter(|&stmt_id| {
                let stmt = self.ast.stmts.get(stmt_id);
                matches!(stmt.kind, StmtKind::Let { .. })
                    && matches!(
                        self.ast.exprs.get(match &stmt.kind {
                            StmtKind::Let { value, .. } => *value,
                            _ => unreachable!(),
                        }).kind,
                        ExprKind::ProviderInvocation { .. }
                    )
            })
            .collect();

        let mut errors = FossilErrors::new();

        for stmt_id in provider_stmts {
            if let Err(e) = self.expand_provider_type_stmt(stmt_id) {
                errors.push(e);
            }
        }

        for stmt_id in let_stmts {
            if let Err(e) = self.expand_let_provider_stmt(stmt_id) {
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        self.expand_generated_modules();

        Ok(ExpandResult {
            ast: self.ast,
            gcx: self.gcx,
            warnings: self.warnings,
        })
    }

    fn resolve_provider(
        &self,
        provider_path: &Path,
        loc: Loc,
    ) -> Result<std::sync::Arc<dyn crate::traits::provider::TypeProviderImpl>, FossilError> {
        let provider_def_id = self
            .gcx
            .definitions
            .resolve(provider_path)
            .ok_or_else(|| {
                FossilError::undefined_type(provider_path.display(&self.gcx.interner), loc)
            })?;

        match &self.gcx.definitions.get(provider_def_id).kind {
            DefKind::Provider(provider) => Ok(provider.clone()),
            _ => Err(FossilError::not_a_provider(
                provider_path.display(&self.gcx.interner),
                loc,
            )),
        }
    }

    fn resolve_provider_with_args(
        &self,
        provider_path: &Path,
        raw_args: &[ProviderArgument],
        loc: Loc,
    ) -> Result<ResolvedProvider, FossilError> {
        let imp = self.resolve_provider(provider_path, loc)?;
        let args = resolve_to_provider_args(
            raw_args,
            &imp.param_info(),
            &self.gcx.interner,
            leak_provider_name(provider_path, &self.gcx.interner),
            loc,
        )?;
        let name = provider_path.display(&self.gcx.interner);
        let kind = imp.info().kind;
        Ok(ResolvedProvider { imp, args, name, kind })
    }

    /// Expand `type Name = provider!(args)` or `type { A, B } = provider!(args)`
    fn expand_provider_type_stmt(&mut self, stmt_id: StmtId) -> Result<(), FossilError> {
        let (entries, provider_path, raw_args, loc) = {
            let stmt = self.ast.stmts.get(stmt_id);
            match &stmt.kind {
                StmtKind::ProviderType { entries, provider, args, loc } => {
                    (entries.clone(), provider.clone(), args.clone(), *loc)
                }
                _ => return Ok(()),
            }
        };

        let resolved = self.resolve_provider_with_args(&provider_path, &raw_args, loc)?;

        if resolved.kind == ProviderKind::Data {
            return Err(FossilError::provider_kind_mismatch(
                resolved.name,
                "loads data, use `let` instead of `type`",
                loc,
            ));
        }

        // Collect all provider outputs first (using caching reader for shared file reads)
        let outputs: Vec<_> = {
            let caching_reader = CachingFileReader::new(self.gcx.file_reader.as_ref());
            entries
                .iter()
                .enumerate()
                .map(|(i, entry)| {
                    let type_name_str = self.gcx.interner.resolve(entry.name).to_string();
                    let ctor_params: Vec<Symbol> = entry.ctor_params.iter().map(|p| p.name).collect();
                    let mut ctx = ProviderContext {
                        interner: &mut self.gcx.interner,
                        path_resolver: self.gcx.path_resolver.as_ref(),
                        file_reader: &caching_reader,
                        ctor_params,
                        expected_type_count: if entries.len() > 1 { Some(entries.len()) } else { None },
                        entry_index: Some(i),
                    };
                    resolved.imp.provide(&resolved.args, &mut ctx, &type_name_str, loc)
                })
                .collect::<Result<Vec<_>, _>>()?
        };

        for (entry, provider_output) in entries.iter().zip(outputs) {
            self.warnings.extend(provider_output.warnings);

            let generated_type_id = schema_to_ast(
                &provider_output.schema, &mut self.ast, &mut self.gcx.interner, loc,
            );

            let mut attrs = entry.attrs.clone();
            let mut provider_attrs = provider_output.type_attributes;
            provider_attrs.push(build_provider_attribute(
                &mut self.gcx.interner, &resolved.name, resolved.kind, loc,
            ));
            merge_provider_attributes(&mut attrs, provider_attrs);

            let type_stmt = self.ast.stmts.alloc(Stmt {
                loc,
                kind: StmtKind::Type {
                    name: entry.name,
                    ty: generated_type_id,
                    attrs,
                    ctor_params: entry.ctor_params.clone(),
                },
            });
            self.ast.root.push(type_stmt);

            if let Some(module_spec) = provider_output.module_spec {
                self.register_generated_module(entry.name, module_spec)?;
            }
        }

        self.ast.root.retain(|&id| id != stmt_id);
        Ok(())
    }

    /// Expand `let name = provider!(args)` — only for Data providers
    fn expand_let_provider_stmt(&mut self, stmt_id: StmtId) -> Result<(), FossilError> {
        let (binding_name, value_id) = {
            let stmt = self.ast.stmts.get(stmt_id);
            match &stmt.kind {
                StmtKind::Let { name, value } => (*name, *value),
                _ => return Ok(()),
            }
        };

        let (provider_path, args, loc) = {
            let expr = self.ast.exprs.get(value_id);
            match &expr.kind {
                ExprKind::ProviderInvocation { provider, args } => {
                    (provider.clone(), args.clone(), expr.loc)
                }
                _ => return Ok(()),
            }
        };

        let resolved = self.resolve_provider_with_args(&provider_path, &args, loc)?;

        if resolved.kind == ProviderKind::Schema {
            return Err(FossilError::provider_kind_mismatch(
                resolved.name,
                "is schema-only, use `type` instead of `let`",
                loc,
            ));
        }

        let binding_name_str = self.gcx.interner.resolve(binding_name).to_string();

        let provider_output = {
            let mut ctx = ProviderContext {
                interner: &mut self.gcx.interner,
                path_resolver: self.gcx.path_resolver.as_ref(),
                file_reader: self.gcx.file_reader.as_ref(),
                ctor_params: vec![],
                expected_type_count: None,
                entry_index: None,
            };
            resolved.imp.provide(&resolved.args, &mut ctx, &binding_name_str, loc)?
        };

        let provider_attr = build_provider_attribute(
            &mut self.gcx.interner, &resolved.name, resolved.kind, loc,
        );

        self.warnings.extend(provider_output.warnings);

        let generated_type_id = schema_to_ast(&provider_output.schema, &mut self.ast, &mut self.gcx.interner, loc);

        let mut attrs = provider_output.type_attributes;
        attrs.push(provider_attr);

        let synth_type_stmt = self.ast.stmts.alloc(Stmt {
            loc,
            kind: StmtKind::Type {
                name: binding_name,
                ty: generated_type_id,
                attrs,
                ctor_params: vec![],
            },
        });

        if let Some(pos) = self.ast.root.iter().position(|&id| id == stmt_id) {
            self.ast.root.insert(pos, synth_type_stmt);
        }

        if let Some(module_spec) = provider_output.module_spec {
            self.register_generated_module(binding_name, module_spec)?;
        }

        let load_sym = self.gcx.interner.intern("load");
        let callee_path = Path::qualified(vec![binding_name, load_sym]);
        let callee_expr = self.ast.exprs.alloc(Expr {
            loc,
            kind: ExprKind::Identifier(callee_path),
        });
        let application_expr = self.ast.exprs.alloc(Expr {
            loc,
            kind: ExprKind::Application {
                callee: callee_expr,
                args: vec![],
            },
        });

        let stmt_mut = self.ast.stmts.get_mut(stmt_id);
        if let StmtKind::Let { value, .. } = &mut stmt_mut.kind {
            *value = application_expr;
        }

        Ok(())
    }

    fn register_generated_module(
        &mut self,
        module_name: Symbol,
        spec: ModuleSpec,
    ) -> Result<(), FossilError> {
        self.gcx.register_module_by_symbol(module_name, spec);
        Ok(())
    }

    fn expand_generated_modules(&mut self) {
        use std::collections::HashMap as StdHashMap;
        use crate::error::FossilWarning;

        let generators = self.gcx.module_generators.clone();
        if generators.is_empty() {
            return;
        }

        let candidates: Vec<(Symbol, Vec<RecordField>)> = self
            .ast
            .root
            .iter()
            .copied()
            .filter_map(|stmt_id| {
                let stmt = self.ast.stmts.get(stmt_id);
                if let StmtKind::Type { name, ty, .. } = &stmt.kind {
                    let ty_node = self.ast.types.get(*ty);
                    if let TypeKind::Record(fields) = &ty_node.kind {
                        return Some((*name, fields.clone()));
                    }
                }
                None
            })
            .collect();

        let mut to_register: Vec<(Symbol, ModuleSpec)> = Vec::new();
        let mut registered_fns: StdHashMap<(Symbol, String), bool> = StdHashMap::new();

        for (type_name, fields) in candidates {
            let type_def_id = self
                .gcx
                .definitions
                .resolve(&crate::common::Path::Simple(type_name))
                .unwrap_or_else(|| {
                    self.gcx.definitions.insert(None, type_name, DefKind::Mod)
                });

            let info = TypeInfo {
                name: type_name,
                def_id: type_def_id,
                fields: &fields,
                interner: &self.gcx.interner,
            };

            for generator in &generators {
                if let Some(spec) = generator(&info) {
                    for func in &spec.functions {
                        let key = (type_name, func.name.clone());
                        if registered_fns.contains_key(&key) {
                            let type_str = self.gcx.interner.resolve(type_name);
                            self.warnings.push(FossilWarning::generic(
                                format!(
                                    "Module generator conflict: {}.{} registered by multiple generators",
                                    type_str, func.name
                                ),
                                crate::ast::Loc::generated(),
                            ));
                        } else {
                            registered_fns.insert(key, true);
                        }
                    }
                    to_register.push((type_name, spec));
                }
            }
        }

        for (type_name, spec) in to_register {
            let _ = self.register_generated_module(type_name, spec);
        }
    }
}

/// Merge provider-generated attributes into the user's attribute list.
fn merge_provider_attributes(attrs: &mut Vec<Attribute>, provider_attrs: Vec<Attribute>) {
    for prov in provider_attrs {
        match attrs.iter_mut().find(|a| a.name == prov.name) {
            Some(existing) => {
                let user_keys: HashSet<Symbol> = existing
                    .args
                    .iter()
                    .filter_map(|a| match a {
                        AttributeArg::Named { key, .. } => Some(*key),
                        _ => None,
                    })
                    .collect();
                existing.args.extend(prov.args.into_iter().filter(|a| {
                    !matches!(a, AttributeArg::Named { key, .. } if user_keys.contains(key))
                }));
            }
            None => attrs.push(prov),
        }
    }
}

/// Leak a provider name string for use in `&'static str` error contexts.
fn leak_provider_name(path: &Path, interner: &crate::context::Interner) -> &'static str {
    let name = path.display(interner);
    Box::leak(name.into_boxed_str())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::passes::parse::Parser;

    fn expand_ok(src: &str) -> ExpandResult {
        Parser::parse(src, 0)
            .map(|parsed| ProviderExpander::new((parsed.ast, parsed.gcx)))
            .expect("parse failed")
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

    #[test]
    fn attribute_preserved_through_expand() {
        let src = concat!(
            "#[rdf(type = \"http://example.org/Person\")]\n",
            "type Person(subject: string) do\n",
            "    #[rdf(uri = \"http://xmlns.com/foaf/0.1/name\")]\n",
            "    Name: string\n",
            "end\n",
        );
        let result = expand_ok(src);

        let type_stmt_id = result
            .ast
            .root
            .iter()
            .find(|&&id| matches!(result.ast.stmts.get(id).kind, StmtKind::Type { .. }))
            .expect("should have a Type statement");

        let stmt = result.ast.stmts.get(*type_stmt_id);
        if let StmtKind::Type { attrs, ty, .. } = &stmt.kind {
            assert_eq!(attrs.len(), 1);
            assert_eq!(result.gcx.interner.resolve(attrs[0].name), "rdf");

            let ty_node = result.ast.types.get(*ty);
            if let TypeKind::Record(fields) = &ty_node.kind {
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].attrs.len(), 1);
                assert_eq!(result.gcx.interner.resolve(fields[0].attrs[0].name), "rdf");
            } else {
                panic!("expected Record type");
            }
        } else {
            panic!("expected Type statement");
        }
    }

    #[test]
    fn unknown_provider_error() {
        let src = r#"type X = unknown!("arg")"#;
        let errors = expand_err(src);
        assert!(!errors.is_empty());

        let has_undefined = errors
            .0
            .iter()
            .any(|e| matches!(e, crate::error::FossilError::Undefined { kind: "type", .. }));
        assert!(has_undefined, "expected Undefined type error, got: {:?}", errors);
    }

    #[test]
    fn unknown_provider_in_let_error() {
        let src = r#"let x = unknown!("arg")"#;
        let errors = expand_err(src);
        assert!(!errors.is_empty());

        let has_undefined = errors
            .0
            .iter()
            .any(|e| matches!(e, crate::error::FossilError::Undefined { kind: "type", .. }));
        assert!(has_undefined, "expected Undefined type error for let provider, got: {:?}", errors);
    }

    #[test]
    fn let_parses_provider_invocation() {
        let src = r#"let x = unknown!("arg")"#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let stmt = parsed.ast.stmts.get(parsed.ast.root[0]);
        match &stmt.kind {
            StmtKind::Let { value, .. } => {
                let expr = parsed.ast.exprs.get(*value);
                assert!(
                    matches!(&expr.kind, ExprKind::ProviderInvocation { .. }),
                    "expected ProviderInvocation, got {:?}",
                    expr.kind
                );
            }
            other => panic!("expected Let statement, got {:?}", other),
        }
    }

    #[test]
    fn let_provider_invocation_with_named_args() {
        let src = r#"let x = test!(path: "file.csv", delimiter: ",")"#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let stmt = parsed.ast.stmts.get(parsed.ast.root[0]);
        match &stmt.kind {
            StmtKind::Let { value, .. } => {
                let expr = parsed.ast.exprs.get(*value);
                match &expr.kind {
                    ExprKind::ProviderInvocation { args, .. } => {
                        assert_eq!(args.len(), 2, "expected 2 provider args");
                        assert!(matches!(&args[0], crate::common::ProviderArgument::Named { .. }));
                        assert!(matches!(&args[1], crate::common::ProviderArgument::Named { .. }));
                    }
                    other => panic!("expected ProviderInvocation, got {:?}", other),
                }
            }
            other => panic!("expected Let statement, got {:?}", other),
        }
    }

    #[test]
    fn single_provider_type_parses() {
        let src = r#"type X(id: string) = unknown!("arg")"#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let stmt = parsed.ast.stmts.get(parsed.ast.root[0]);
        assert!(
            matches!(&stmt.kind, StmtKind::ProviderType { entries, .. } if entries.len() == 1),
            "expected ProviderType with 1 entry, got {:?}",
            stmt.kind
        );
    }

    #[test]
    fn multi_provider_type_parses() {
        let src = r#"type { A(id: string), B(id: int) } = unknown!("arg")"#;
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let stmt = parsed.ast.stmts.get(parsed.ast.root[0]);
        assert!(
            matches!(&stmt.kind, StmtKind::ProviderType { entries, .. } if entries.len() == 2),
            "expected ProviderType with 2 entries, got {:?}",
            stmt.kind
        );
    }
}
