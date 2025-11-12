use crate::ast::{self, Ast};
use crate::context::{Arena, Interner};
use crate::error::LowerError;
use crate::module::{Binding, ModuleRegistry};
use crate::resolved::{self, ResolvedAst, Scope};

/// Name resolution for the `fossil` language.
pub struct Resolver {
    output: ResolvedAst,
    registry: ModuleRegistry,
    scope: Scope,
}

impl Resolver {
    pub fn new(registry: ModuleRegistry) -> Self {
        Self {
            output: Default::default(),
            registry,
            scope: Default::default(),
        }
    }

    pub fn resolve(mut self, ast: Ast) -> Result<ResolvedAst, LowerError> {
        let Ast {
            decls,
            exprs,
            types,
            symbols,
        } = ast;

        for (_, decl) in decls.into_iter() {
            match decl {
                ast::Decl::Let(name, expr) => {
                    let expr = self.resolve_expr(expr, &exprs, &symbols)?;
                    let decl = self.output.decls.alloc(resolved::Decl::Let(name, expr));
                    self.scope.define_var(name, decl);
                }

                ast::Decl::Type(name, ty) => {
                    let ty = self.resolve_type(ty, &types, &symbols)?;
                    let _ = self.output.decls.alloc(resolved::Decl::Type(name, ty));
                    self.scope.define_type(name, ty);
                }

                ast::Decl::Expr(expr) => {
                    let expr = self.resolve_expr(expr, &exprs, &symbols)?;
                    self.output.decls.alloc(resolved::Decl::Expr(expr));
                }
            }
        }

        self.output.symbols = symbols;
        Ok(self.output)
    }

    fn resolve_expr(
        &mut self,
        expr: ast::ExprId,
        exprs: &Arena<ast::Expr>,
        symbols: &Interner,
    ) -> Result<resolved::ExprId, LowerError> {
        let expr = exprs.get(expr).clone();

        let resolved = match expr {
            ast::Expr::Identifier(name) => match self.scope.lookup_var(name) {
                Some(id) => resolved::Expr::LocalItem(id),
                None => {
                    let name = symbols.resolve(name).to_string();
                    return Err(LowerError::UndefinedVariable(name));
                }
            },

            ast::Expr::Qualified(path) => {
                let module = self.registry.resolve(&path, symbols)?;
                resolved::Expr::ModuleItem(module)
            }

            ast::Expr::Literal(lit) => resolved::Expr::Literal(lit.clone()),

            ast::Expr::List(list) => {
                let items = list
                    .into_iter()
                    .map(|item| self.resolve_expr(item, exprs, symbols))
                    .collect::<Result<Vec<_>, _>>()?;
                resolved::Expr::List(items)
            }

            ast::Expr::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        let value = self.resolve_expr(value, exprs, symbols)?;
                        Ok((name, value))
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                resolved::Expr::Record(fields)
            }

            ast::Expr::Function { params, body } => {
                let body = self.resolve_expr(body, exprs, symbols)?;
                resolved::Expr::Function { params, body }
            }

            ast::Expr::Application { callee, args } => {
                let callee = self.resolve_expr(callee, exprs, symbols)?;
                let args = args
                    .into_iter()
                    .map(|arg| self.resolve_expr(arg, exprs, symbols))
                    .collect::<Result<Vec<_>, _>>()?;
                resolved::Expr::Application { callee, args }
            }
        };

        Ok(self.output.exprs.alloc(resolved))
    }

    fn resolve_type(
        &mut self,
        ty: ast::TypeId,
        types: &Arena<ast::Type>,
        symbols: &Interner,
    ) -> Result<resolved::TypeId, LowerError> {
        let ty = types.get(ty).clone();

        let resolved = match ty {
            ast::Type::Named(name) => match symbols.resolve(name) {
                "int" | "Int" => resolved::Type::Int,
                "bool" | "Bool" => resolved::Type::Bool,
                "string" | "String" => resolved::Type::String,
                _ => match self.scope.lookup_type(name) {
                    Some(ty) => return Ok(ty),
                    None => {
                        let name = symbols.resolve(name).to_string();
                        return Err(LowerError::UndefinedType(name));
                    }
                },
            },

            ast::Type::Function(params, ty) => {
                let params = params
                    .into_iter()
                    .map(|param| self.resolve_type(param, types, symbols))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty = self.resolve_type(ty, types, symbols)?;
                resolved::Type::Function(params, ty)
            }

            ast::Type::List(inner) => {
                let inner = self.resolve_type(inner, types, symbols)?;
                resolved::Type::List(inner)
            }

            ast::Type::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let ty = self.resolve_type(ty, types, symbols)?;
                        Ok((name, ty))
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                resolved::Type::Record(fields)
            }

            ast::Type::Provider(provider, args) => {
                let path = vec![provider];
                let id = self.registry.resolve(&path, symbols)?;

                match self.registry.get(id) {
                    Binding::Provider(_) => resolved::Type::Provider { provider: id, args },
                    _ => {
                        let provider = symbols.resolve(provider).to_string();
                        return Err(LowerError::NotAProvider(provider));
                    }
                }
            }
        };

        Ok(self.output.types.alloc(resolved))
    }
}
