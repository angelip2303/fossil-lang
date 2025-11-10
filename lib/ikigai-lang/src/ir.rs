use std::collections::HashMap;

use thiserror::Error;

use crate::ast::Ast;
use crate::ast::Decl as AstDecl;
use crate::ast::Literal as AstLiteral;
use crate::ast::{Expr as AstExpr, ExprId as AstExprId};
use crate::ast::{Type as AstType, TypeId as AstTypeId};
use crate::context::*;
use crate::module::Binding;
use crate::module::ModuleRegistry;
use crate::module::RegistryError;
use crate::traits::provider::ProviderError;

#[derive(Error, Debug)]
pub enum LowererError {
    #[error("Provider not found: {0}")]
    ProviderNotFound(String),

    #[error("Cannot find symbol: {0}")]
    UndefinedSymbol(String),

    #[error(transparent)]
    InvalidModulePath(#[from] RegistryError),

    #[error(transparent)]
    TypeGeneration(#[from] ProviderError),
}

pub type DeclId = NodeId<Decl>;
pub type ExprId = NodeId<Expr>;
pub type TypeId = NodeId<Type>;

pub struct IrCtx {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub symbols: Interner,
}

#[derive(Debug, Clone)]

pub enum Decl {
    Let(Symbol, ExprId),
    Type(Symbol, TypeId),
    Expr(ExprId),
}

#[derive(Debug, Clone)]

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

#[derive(Debug, Clone)]

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

pub struct Scope {
    pub vars: HashMap<Symbol, DeclId>, // TODO: would it make sense to match it against ExprId?
    pub types: HashMap<Symbol, TypeId>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            vars: HashMap::new(),
            types: HashMap::new(),
        }
    }

    /// This implements variable shadowing
    pub fn define_var(&mut self, name: Symbol, decl_id: DeclId) {
        self.vars.insert(name, decl_id);
    }

    /// This implements type shadowing
    pub fn define_type(&mut self, name: Symbol, type_id: TypeId) {
        self.types.insert(name, type_id);
    }

    pub fn lookup_var(&self, name: Symbol) -> Option<DeclId> {
        self.vars.get(&name).copied()
    }

    pub fn lookup_type(&self, name: Symbol) -> Option<TypeId> {
        self.types.get(&name).copied()
    }
}

pub struct Resolver {
    pub decls: Arena<Decl>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub registry: ModuleRegistry,
    scope: Scope,
}

impl Resolver {
    pub fn resolve(mut self, ast: Ast) -> Result<IrCtx, LowererError> {
        for (_, decl) in ast.decls.iter() {
            match decl {
                AstDecl::Let(name, expr) => {
                    let expr = self.resolve_expr(expr, &ast)?;
                    let decl = self.decls.alloc(Decl::Let(*name, expr));
                    self.scope.define_var(*name, decl);
                }

                AstDecl::Type(name, ty) => {
                    let ty = self.resolve_type(ty, &ast)?;
                    self.decls.alloc(Decl::Type(*name, ty.clone()));
                    self.scope.define_type(*name, ty);
                }

                AstDecl::Expr(expr) => {
                    let expr = self.resolve_expr(expr, &ast)?;
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

    fn resolve_expr(&mut self, expr: &AstExprId, ast: &Ast) -> Result<ExprId, LowererError> {
        let expr = ast.exprs.get(expr.clone()); // TODO: remove unnecessary clone

        let resolved = match expr {
            // TODO: this could also be a function or a type?
            AstExpr::Identifier(name) => match self.scope.lookup_var(*name) {
                Some(id) => Expr::LocalItem(id),
                None => {
                    let name = ast.symbols.resolve(*name).to_string();
                    return Err(LowererError::UndefinedSymbol(name));
                }
            },

            AstExpr::Qualified(path) => match self.registry.resolve(path, &ast.symbols)? {
                Binding::Function(func) => Expr::ModuleItem(func),
                Binding::Provider(_) => unreachable!(),
            },

            AstExpr::Literal(lit) => match lit {
                AstLiteral::Boolean(b) => Expr::Bool(*b),
                AstLiteral::Integer(n) => Expr::Int(*n),
                AstLiteral::String(s) => Expr::String(*s),
            },

            AstExpr::List(list) => {
                let items = list
                    .into_iter()
                    .map(|item| self.resolve_expr(item, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::List(items)
            }

            AstExpr::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        let value = self.resolve_expr(value, ast)?;
                        Ok((*name, value))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Record(fields)
            }

            AstExpr::Function { params, body } => {
                let body = self.resolve_expr(body, ast)?;
                Expr::Function {
                    params: *params,
                    body,
                }
            }

            AstExpr::Application { callee, args } => {
                let callee = self.resolve_expr(callee, ast)?;
                let args = args
                    .into_iter()
                    .map(|arg| self.resolve_expr(arg, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Application { callee, args }
            }
        };

        Ok(self.exprs.alloc(resolved))
    }

    fn resolve_type(&mut self, ty: &AstTypeId, ast: &Ast) -> Result<TypeId, LowererError> {
        let ty = ast.types.get(ty.clone());

        let resolved = match ty {
            AstType::Named(name) => match ast.symbols.resolve(*name) {
                "int" | "Int" => Type::Int,
                "bool" | "Bool" => Type::Bool,
                "string" | "String" => Type::String,
                _ => match self.scope.lookup_type(*name) {
                    Some(ty) => return Ok(ty), // it was already resolved
                    None => Type::Named(*name),
                },
            },

            AstType::Function(params, ty) => {
                let params = params
                    .into_iter()
                    .map(|param| self.resolve_type(param, ast))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty = self.resolve_type(ty, ast)?;
                Type::Function(params, ty)
            }

            AstType::List(inner) => {
                let inner = self.resolve_type(inner, ast)?;
                Type::List(inner)
            }

            AstType::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let ty = self.resolve_type(ty, ast)?;
                        Ok((*name, ty))
                    })
                    .collect::<Result<Vec<_>, LowererError>>()?;
                Type::Record(fields)
            }

            AstType::Provider(provider, args) => {
                let path = vec![*provider];
                let binding = self.registry.resolve(&path, &ast.symbols)?;
                let provider = match binding {
                    Binding::Provider(provider) => provider,
                    _ => {
                        let provider = ast.symbols.resolve(*provider).to_string();
                        return Err(LowererError::ProviderNotFound(provider));
                    }
                };
                provider.generate(args)?
            }
        };

        Ok(self.types.alloc(resolved))
    }
}
