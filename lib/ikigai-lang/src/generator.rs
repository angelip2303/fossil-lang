use crate::context::Arena;
use crate::error::TypeGenError;
use crate::generated::{self, ConcreteAst};
use crate::module::{Binding, ModuleRegistry};
use crate::resolved::{self, ResolvedAst};

pub struct TypeGenerator {
    output: ConcreteAst,
    registry: ModuleRegistry,
}

impl TypeGenerator {
    pub fn new(registry: ModuleRegistry) -> Self {
        Self {
            output: Default::default(),
            registry,
        }
    }

    pub fn generate(mut self, ast: ResolvedAst) -> Result<ConcreteAst, TypeGenError> {
        let ResolvedAst {
            decls,
            exprs,
            types,
            symbols,
        } = ast;

        for (_, decl) in decls.into_iter() {
            match decl {
                resolved::Decl::Let(name, expr) => {
                    let expr = self.resolve_expr(expr, &exprs);
                    self.output.decls.alloc(generated::Decl::Let(name, expr));
                }

                resolved::Decl::Type(name, ty) => {
                    let ty = self.resolve_type(ty, &types)?;
                    self.output.decls.alloc(generated::Decl::Type(name, ty));
                }

                resolved::Decl::Expr(expr) => {
                    let expr = self.resolve_expr(expr, &exprs);
                    self.output.decls.alloc(generated::Decl::Expr(expr));
                }
            }
        }

        self.output.symbols = symbols;
        Ok(self.output)
    }

    fn resolve_expr(
        &mut self,
        expr: resolved::ExprId,
        exprs: &Arena<resolved::Expr>,
    ) -> generated::ExprId {
        let expr = exprs.get(expr).clone();

        let resolved = match expr {
            resolved::Expr::LocalItem(name) => generated::Expr::LocalItem(name.into()),

            resolved::Expr::ModuleItem(path) => generated::Expr::ModuleItem(path),

            resolved::Expr::Literal(lit) => generated::Expr::Literal(lit.clone()),

            resolved::Expr::List(list) => {
                let items = list
                    .into_iter()
                    .map(|item| self.resolve_expr(item, exprs))
                    .collect();
                generated::Expr::List(items)
            }

            resolved::Expr::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        let value = self.resolve_expr(value, exprs);
                        (name, value)
                    })
                    .collect();
                generated::Expr::Record(fields)
            }

            resolved::Expr::Function { params, body } => {
                let body = self.resolve_expr(body, exprs);
                generated::Expr::Function { params, body }
            }

            resolved::Expr::Application { callee, args } => {
                let callee = self.resolve_expr(callee, exprs);
                let args = args
                    .into_iter()
                    .map(|arg| self.resolve_expr(arg, exprs))
                    .collect();
                generated::Expr::Application { callee, args }
            }
        };

        self.output.exprs.alloc(resolved)
    }

    fn resolve_type(
        &mut self,
        ty: resolved::TypeId,
        types: &Arena<resolved::Type>,
    ) -> Result<generated::TypeId, TypeGenError> {
        let ty = types.get(ty).clone();

        let resolved = match ty {
            resolved::Type::Bool => generated::Type::Bool,
            resolved::Type::Int => generated::Type::Int,
            resolved::Type::String => generated::Type::String,

            resolved::Type::Function(params, ty) => {
                let params = params
                    .into_iter()
                    .map(|param| self.resolve_type(param, types).unwrap()) // this cannot fail
                    .collect();
                let ty = self.resolve_type(ty, types)?;
                generated::Type::Function(params, ty)
            }

            resolved::Type::List(inner) => {
                let inner = self.resolve_type(inner, types).unwrap(); // this cannot fail
                generated::Type::List(inner)
            }

            resolved::Type::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let ty = self.resolve_type(ty, types).unwrap(); // this cannot fail
                        (name, ty)
                    })
                    .collect();
                generated::Type::Record(fields)
            }

            resolved::Type::Provider { provider, args } => {
                let provider = self.registry.get(provider);
                match provider {
                    Binding::Provider(provider) => provider.generate(&args)?,
                    _ => unreachable!(),
                }
            }
        };

        Ok(self.output.types.alloc(resolved))
    }
}
