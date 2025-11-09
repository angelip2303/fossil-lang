use paste::paste;
use thiserror::Error;

use crate::ast::AstCtx;
use crate::ast::Decl as AstDecl;
use crate::ast::Expr as AstExpr;
use crate::ast::Literal as AstLiteral;
use crate::ast::Type as AstType;
use crate::interner::{Interner, Symbol};
use crate::module::Binding;
use crate::module::ModuleRegistry;

#[derive(Error, Debug)]
pub enum LowererError {}

trait ArenaAllocated<'a> {
    fn alloc(self, context: &'a IrCtx<'a>) -> &'a Self
    where
        Self: 'a;
}

macro_rules! declare_arena {
    ($t: ident <$a: lifetime>) => {
        paste! {
            pub type [<$t Arena>]<$a> = ::typed_arena::Arena<$t<$a>>;
            pub type [<$t Ref>]<$a> = &$a $t<$a>;

            impl<'a> ArenaAllocated<'a> for $t<'a> {
                fn alloc(self, context: &'a IrCtx<'a>) -> &'a $t<'a> where Self: 'a {
                    context. [<$t:lower s>].alloc(self)
                }
            }
        }
    };

    ($($t: ident $(< $a: lifetime >)?),+) => {
        $(
            declare_arena!($t $(<$a>)?);
        )+
    };
}

declare_arena!(Decl<'a>, Expr<'a>, Type<'a>);

pub struct IrCtx<'a> {
    pub decls: DeclArena<'a>,
    pub exprs: ExprArena<'a>,
    pub types: TypeArena<'a>,
    pub interner: Interner,
}

impl<'a> IrCtx<'a> {
    pub fn alloc<T>(&'a self, t: T) -> &'a T
    where
        T: ArenaAllocated<'a>,
    {
        &*t.alloc(self)
    }
}

pub enum Decl<'a> {
    Let(Symbol, ExprRef<'a>),
    Type(Symbol, TypeRef<'a>),
    Expr(ExprRef<'a>),
}

pub enum Expr<'a> {
    Var(Symbol),
    Qualified(Vec<Symbol>),
    Int(i64),
    String(Symbol),
    Bool(bool),
    List(Vec<ExprRef<'a>>),
    Record(Vec<(Symbol, ExprRef<'a>)>),
    Function {
        params: Vec<Symbol>,
        body: ExprRef<'a>,
    },
    Application {
        callee: ExprRef<'a>,
        args: Vec<ExprRef<'a>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

pub enum Type<'a> {
    Int,
    String,
    Bool,
    Named(Symbol),
    Var(TypeVar), // internal to the typechecker
    Function(Vec<TypeRef<'a>>, TypeRef<'a>),
    List(TypeRef<'a>),
    Record(Vec<(Symbol, TypeRef<'a>)>),
}

pub struct Lowerer<'a> {
    pub ctx: IrCtx<'a>,
    pub registry: ModuleRegistry<'a>,
}

impl<'a> Lowerer<'a> {
    pub fn new(ctx: IrCtx<'a>) -> Self {
        Self {
            ctx,
            registry: Default::default(),
        }
    }

    pub fn lower(&mut self, ast: AstCtx<'a>) -> Result<(), LowererError> {
        for decl in ast.decls.into_vec() {
            match decl {
                AstDecl::Let(name, expr) => {
                    let sym = self.ctx.interner.intern(name);
                    let expr = self.lower_expr(*expr)?;
                    self.ctx.alloc(Decl::Let(sym, expr));
                }
                AstDecl::Type(name, ty) => {
                    let sym = self.ctx.interner.intern(name);
                    let ty = self.lower_type(*ty)?;
                    self.ctx.alloc(Decl::Type(sym, ty));
                }
                AstDecl::Expr(expr) => {
                    let expr = self.lower_expr(*expr)?;
                    self.ctx.alloc(Decl::Expr(expr));
                }
            }
        }

        Ok(())
    }

    fn lower_expr(&mut self, expr: AstExpr<'a>) -> Result<ExprRef<'a>, LowererError> {
        let expr = match expr {
            AstExpr::Identifier(name) => {
                let sym = self.ctx.interner.intern(name);
                Expr::Var(sym)
            }

            AstExpr::Qualified(path) => {
                let path = path
                    .into_iter()
                    .map(|segment| self.ctx.interner.intern(segment))
                    .collect();
                Expr::Qualified(path)
            }

            AstExpr::Literal(lit) => match lit {
                AstLiteral::Boolean(b) => Expr::Bool(b),
                AstLiteral::Integer(n) => Expr::Int(n),
                AstLiteral::String(s) => {
                    let sym = self.ctx.interner.intern(s);
                    Expr::String(sym)
                }
            },

            AstExpr::List(list) => {
                let items = list
                    .into_iter()
                    .map(|item| self.lower_expr(*item))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::List(items)
            }

            AstExpr::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| {
                        let sym = self.ctx.interner.intern(name);
                        let value = self.lower_expr(*value)?;
                        Ok((sym, value))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Record(fields)
            }

            AstExpr::Function { params, body } => {
                let params = params
                    .into_iter()
                    .map(|param| self.ctx.interner.intern(param))
                    .collect();
                let body = self.lower_expr(*body)?;
                Expr::Function { params, body }
            }

            AstExpr::Application { callee, args } => {
                let callee = self.lower_expr(*callee)?;
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_expr(*arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Expr::Application { callee, args }
            }
        };

        Ok(self.ctx.alloc(expr))
    }

    fn lower_type(&mut self, ty: AstType<'a>) -> Result<TypeRef<'a>, LowererError> {
        let ty = match ty {
            AstType::Named(name) => match name {
                "Int" => Type::Int,
                "Bool" => Type::Bool,
                "String" => Type::String,
                other => {
                    let sym = self.ctx.interner.intern(other);
                    Type::Named(sym)
                }
            },

            AstType::Function(params, ty) => {
                let params = params
                    .into_iter()
                    .map(|param| self.lower_type(*param))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty = self.lower_type(*ty)?;
                Type::Function(params, ty)
            }

            AstType::List(inner) => {
                let inner = self.lower_type(*inner)?;
                Type::List(inner)
            }

            AstType::Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(name, ty)| {
                        let name = self.ctx.interner.intern(name);
                        let ty = self.lower_type(*ty)?;
                        Ok((name, ty))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Type::Record(fields)
            }

            AstType::Provider(provider, args) => {
                let provider = provider.split(".").collect::<Vec<_>>();
                let provider = match self.registry.resolve(&provider) {
                    Some(Binding::Provider(provider)) => provider,
                    _ => todo!(),
                };
                provider.generate(&args)?
            }
        };

        Ok(self.ctx.alloc(ty))
    }
}
