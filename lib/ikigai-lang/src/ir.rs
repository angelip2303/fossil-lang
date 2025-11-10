use thiserror::Error;

use crate::ast::Ast;
use crate::ast::Decl as AstDecl;
use crate::ast::Literal as AstLiteral;
use crate::ast::{Expr as AstExpr, ExprId as AstExprId};
use crate::ast::{Type as AstType, TypeId as AstTypeId};
use crate::context::*;
use crate::module::Binding;
use crate::module::ModuleRegistry;

#[derive(Error, Debug)]
pub enum LowererError {}

type DeclId = NodeId<Decl>;
type ExprId = NodeId<Expr>;
type TypeId = NodeId<Type>;

pub struct IrCtx {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub symbols: Interner,
}

pub enum Decl {
    Let(Symbol, ExprId),
    Type(Symbol, TypeId),
    Expr(ExprId),
}

pub enum Expr {
    LocalItem(DeclId),
    ModuleItem(BindingId), // TODO: think about it
    Int(i64),
    String(Symbol),
    Bool(bool),
    List(Vec<ExprId>),
    Record(Vec<(Symbol, ExprId)>),
    Function { params: Vec<Symbol>, body: ExprId },
    Application { callee: ExprId, args: Vec<ExprId> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

pub enum Type {
    Int,
    String,
    Bool,
    Named(Symbol),
    Var(TypeVar), // internal to the typechecker
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
}

pub struct Resolver<'a> {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub registry: ModuleRegistry<'a>,
}

impl<'a> Resolver<'a> {
    pub fn resolve(mut self, ast: Ast) -> Result<IrCtx, LowererError> {
        for (_, decl) in ast.decls.iter() {
            match decl {
                AstDecl::Let(name, expr) => {
                    let expr = self.resolve_expr(*expr, &ast)?;
                    self.decls.alloc(Decl::Let(*name, expr));
                }

                AstDecl::Type(name, ty) => {
                    let ty = self.resolve_type(*ty, &ast)?;
                    self.decls.alloc(Decl::Type(*name, ty));
                }

                AstDecl::Expr(expr) => {
                    let expr = self.resolve_expr(*expr, &ast)?;
                    self.decls.alloc(Decl::Expr(expr));
                }
            }
        }

        Ok(IrCtx {
            decls: self.decls,
            exprs: self.exprs,
            types: self.types,
            symbols: ast.symbols,
        })
    }

    fn resolve_expr(&mut self, expr: AstExprId, ast: &Ast) -> Result<ExprId, LowererError> {
        let resolved = match ast.exprs.get(expr) {
            AstExpr::Identifier(name) => {
                // TODO: lookup symbol in context
                Expr::LocalItem(name)
            }

            AstExpr::Qualified(path) => {
                // TODO: lookup symbol in context
                Expr::ModuleItem(path)
            }

            AstExpr::Literal(lit) => match lit {
                AstLiteral::Boolean(b) => Expr::Bool(*b),
                AstLiteral::Integer(n) => Expr::Int(*n),
                AstLiteral::String(s) => Expr::String(*s),
            },

            AstExpr::List(list) => {
                let items = list
                    .into_iter()
                    .map(|item| self.resolve_expr(*item, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::List(items)
            }

            AstExpr::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        let value = self.resolve_expr(*value, ast)?;
                        Ok((*name, value))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Record(fields)
            }

            AstExpr::Function { params, body } => {
                // TODO: scope?
                let body = self.resolve_expr(*body, ast)?;
                Expr::Function {
                    params: *params,
                    body,
                }
            }

            AstExpr::Application { callee, args } => {
                let callee = self.resolve_expr(*callee, ast)?;
                let args = args
                    .into_iter()
                    .map(|arg| self.resolve_expr(*arg, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Application { callee, args }
            }
        };

        Ok(self.exprs.alloc(resolved))
    }

    fn resolve_type(&mut self, ty: AstTypeId, ast: &Ast) -> Result<TypeId, LowererError> {
        let ty = match ast.types.get(ty) {
            AstType::Named(name) => match ast.symbols.resolve(*name) {
                "int" | "Int" => Type::Int,
                "bool" | "Bool" => Type::Bool,
                "string" | "String" => Type::String,
                _ => {
                    // TODO: lookup if it was already defined?
                    Type::Named(*name)
                }
            },

            AstType::Function(params, ty) => {
                let params = params
                    .into_iter()
                    .map(|param| self.resolve_type(*param, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty = self.resolve_type(*ty, ast)?;
                Type::Function(params, ty)
            }

            AstType::List(inner) => {
                let inner = self.resolve_type(*inner, ast)?;
                Type::List(inner)
            }

            AstType::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let ty = self.resolve_type(*ty, ast)?;
                        Ok((*name, ty))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Type::Record(fields)
            }

            AstType::Provider(provider, args) => {
                let provider = vec![*provider];
                let provider = match self.registry.resolve(&provider) {
                    Some(Binding::Provider(provider)) => provider,
                    _ => todo!(),
                };
                provider.generate(&args)?
            }
        };

        Ok(self.types.alloc(ty))
    }
}
