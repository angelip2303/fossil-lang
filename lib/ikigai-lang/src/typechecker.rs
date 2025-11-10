use std::collections::HashMap;

use thiserror::Error;

use crate::context::Symbol;
use crate::ir::{Decl, Expr, IrCtx, Type, TypeId, TypeVar};

#[derive(Error, Debug)]
pub enum TypeError {}

/// A type scheme represents polymorphic types, e.g. `forall a. a -> a`.
#[derive(Clone)]
pub struct TypeScheme {
    pub quantified: Vec<TypeVar>,
    pub ty: TypeId,
}

impl TypeScheme {
    /// Create a monomorphic type scheme, e.g. no quantified variables
    pub fn mono(ty: TypeId) -> Self {
        Self {
            quantified: Vec::new(),
            ty,
        }
    }
}

#[derive(Clone, Default)]
pub struct TypeEnv {
    bindings: HashMap<Symbol, TypeScheme>,
}

impl TypeEnv {
    pub fn extend(&self, sym: Symbol, scheme: TypeScheme) -> Self {
        let mut new_env = self.clone();
        new_env.bindings.insert(sym, scheme);
        new_env
    }

    pub fn lookup(&self, sym: Symbol) -> Option<&TypeScheme> {
        self.bindings.get(&sym)
    }

    pub fn free_vars(&self) -> Vec<TypeVar> {
        // TODO: revisit this
        self.bindings
            .values()
            .flat_map(|scheme| scheme.quantified.clone())
            .collect()
    }
}

#[derive(Default)]
pub struct Substitution {
    map: HashMap<TypeVar, TypeId>,
}

impl Substitution {
    pub fn insert(&mut self, var: TypeVar, ty: TypeId) {
        self.map.insert(var, ty);
    }

    pub fn lookup(&self, var: TypeVar) -> Option<&TypeId> {
        self.map.get(&var)
    }

    // s1 = s1 ∘ s2
    pub fn compose(&mut self, other: &Substitution) {
        // TODO: revisit this
        for (var, ty) in other.map.iter() {
            if let Some(ty2) = self.lookup(*var) {
                self.insert(*var, ty2.clone());
            } else {
                self.insert(*var, ty.clone());
            }
        }
    }
}

// TODO: maybe this should be a method on TypeEnv
pub struct TypeChecker {
    next_var: u32,
}

impl TypeChecker {
    pub fn check(&self, ctx: &IrCtx) -> Result<TypeEnv, TypeError> {
        let env = TypeEnv::default();

        for (_, decl) in ctx.decls.iter() {
            match decl {
                Decl::Let(name, expr) => {
                    let expr = ctx.exprs.get(*expr);
                    let (ty, _) = self.infer(ctx, &env, expr)?;
                    let scheme = self.generalize(&env, ty);
                    env = env.extend(*sym, scheme);
                }

                // TODO: this should already be in TypeEnv due to resolvers?
                Decl::Type(name, ty) => {
                    let scheme = TypeScheme::mono(ty);
                    env = env.extend(*sym, scheme);
                }

                Decl::Expr(expr) => {
                    self.infer(ctx, &env, expr)?;
                }
            }
        }

        Ok(env)
    }

    /// Algorithm W
    fn infer(
        &mut self,
        ctx: &IrCtx,
        env: &TypeEnv,
        expr: &Expr,
    ) -> Result<(Substitution, Type), TypeError> {
        let ans = match expr {
            Expr::Int(_) => (Default::default(), Type::Int),
            Expr::Bool(_) => (Default::default(), Type::Bool),
            Expr::String(_) => (Default::default(), Type::String),

            Expr::List(elems) => {
                if elems.is_empty() {
                    let ty = Type::Var(self.fresh_var());
                    let inner = ctx.types.alloc(ty);
                    return Ok((Default::default(), Type::List(inner)));
                }

                let (mut subst, first_ty) = self.infer(env, elems[0])?;

                for &item in &elems[1..] {
                    let (item_ty, s) = self.infer(env, item)?;
                    let s = self.unify(first_ty, item_ty, &s)?;
                    subst = subst.compose(&s, self);
                }

                let elem_ty = self.apply_subst(&subst, first_ty);

                (subst, Type::List(elem_ty))
            }
        };

        Ok(ans)
    }

    fn fresh_var(&mut self) -> TypeVar {
        let var = TypeVar(self.next_var);
        self.next_var += 1;
        var
    }
}
