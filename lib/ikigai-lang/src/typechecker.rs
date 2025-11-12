use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;
use std::result;

use thiserror::Error;

use crate::ast::Literal;
use crate::context::Symbol;
use crate::ir::{Decl, Expr, ExprId, IrCtx, TypeId};

type Result<T> = result::Result<T, TypeError>;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Illegal recursive type, {0:?} is recursive with {1:?}")]
    RecursiveType(TypeVar, TypeId),
}

/// Operations on types
trait TypeOps {
    /// Find the set of free type variables in the type
    fn ftv(&self) -> HashSet<TypeVar>;
    /// Apply a substitution to the type
    fn apply(&self, subst: &Subst) -> Self;
}

impl<T: TypeOps> TypeOps for Vec<T> {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter().flat_map(|t| t.ftv()).collect()
    }

    fn apply(&self, subst: &Subst) -> Self {
        self.iter().map(|x| x.apply(s)).collect()
    }
}

// Type variable for polymorphism
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(pub usize);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

impl TypeVar {
    /// Attempt to bind a type variable to a type, returning an appropriate substitution
    fn bind(&self, ty: Type) -> Result<Subst> {
        if let Type::Var(var) = ty
            && var == *self
        {
            return Ok(Default::default());
        }

        if ty.ftv().contains(self) {
            Err(TypeError::RecursiveType(self.clone(), ty.clone()))
        } else {
            Ok(Subst::new([self.clone(), ty.clone()]))
        }
    }
}

pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn new() -> Self {
        Self { supply: 0 }
    }

    pub fn next(&mut self) -> TypeVar {
        let v = TypeVar(self.supply);
        self.supply += 1;
        v
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    String,
    Bool,
    Named(Symbol), // TODO: this should refer to the type's impl (i.e. declaration)
    Var(TypeVar),
    Function(Vec<TypeId>, TypeId),
    List(TypeId),
    Record(Vec<(Symbol, TypeId)>),
}

impl TypeOps for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            // primitive types have no free variables
            Type::Int | Type::String | Type::Bool => Default::default(),

            // for a type variable, there is one free variable: the type variable itself
            Type::Var(var) => HashSet::from([*var]),

            // for functions, we take the all the free type variables in the parameters and return type
            Type::Function(params, ret) => {
                let mut ftv = Default::default();
                for param in params {
                    ftv.extend(param.ftv());
                }
                ftv.extend(ret.ftv());
                ftv
            }
        }
    }

    fn apply(&self, subst: &Subst) -> Self {
        match self {
            // primitive types are not affected by substitution
            Type::Int | Type::String | Type::Bool => self.clone(),

            // if the variable is bound by the substitution, we replace it with the substituted type
            Type::Var(var) => subst.get(var).cloned().unwrap_or(self.clone()),

            // for functions, we apply the substitution to the parameters and return type
            Type::Function(params, ty) => {
                let mut new_params = Vec::with_capacity(params.len());
                for param in params {
                    new_params.push(param.apply(subst));
                }
                Type::Function(new_params, ty.apply(subst))
            }
        }
    }
}

impl Type {
    fn mgu(&self, other: &Self) -> Result<Subst> {
        let subst = match (self, other) {
            // if they both are primitive types, no substitution needs to be performed
            (Type::Int, Type::Int) | (Type::String, Type::String) | (Type::Bool, Type::Bool) => {
                Default::default()
            }

            // if one of them is a type variable, we can bind the variable to the other type
            (Type::Var(var), ty) | (ty, Type::Var(var)) => var.bind(ty)?,

            // for functions, we find the most general unifier of the parameters, apply the resulting
            // substitution to the return type, find the most general unifier of the return types, and
            // compose the substitutions
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                let mut subst = Default::default();
                for (param1, param2) in params1.iter().zip(params2) {
                    subst = subst.compose(param1.mgu(param2)?)?;
                }
                subst.compose(ret1.mgu(ret2)?)?
            }
        };

        Ok(subst)
    }
}

/// Polytypes (or type schemes) are types containing variables bound by zero or more for-all
/// quantifiers, e.g. `forall a. a -> a`. As an example, a function `forall a. a -> a` with
/// polytype can map any value of the same type to itself and the identity function is a value
/// for this type. As another example, `forall a. Set(a) -> int` is the type of a function mapping
/// all finite sets to integers. A function which returns the size of a set is a value for this type.
#[derive(Clone, Debug)]
pub struct Polytype {
    pub vars: Vec<TypeVar>,
    pub ty: TypeId,
}

impl TypeOps for Polytype {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.ty
            .ftv()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    fn apply(&self, s: &Subst) -> Polytype {
        Polytype {
            vars: self.vars.clone(),
            ty: {
                let mut sub = s.clone();
                for var in &self.vars {
                    sub.remove(var);
                }
                self.ty.apply(&sub)
            },
        }
    }
}

impl Polytype {
    /// Instantiate polytype to a monotype with fresh type variables. Replaces all bound type variables
    /// with fresh type variables and return the instantiated type.
    pub fn instantiate(&self, tvg: &mut TypeVarGen) -> TypeId {
        // in case of no bound variables, it already is a monotype, so return it
        if self.vars.is_empty() {
            return self.ty;
        }

        let newvars = self.vars.iter().map(|_| Type::Var(tvg.next()));
        let subst = Subst::new(self.vars.iter().cloned().zip(newvars));
        self.ty.apply(&subst)
    }
}

/// A substitution is a mapping `σ: V -> T` from variables to terms; the notation
/// `{x1 -> t1, x2 -> t2, ..., xn -> tn}` refers to the substitution that maps each variable
/// `xi` to the term `ti` for all `i < n`. The variable `xi` must be pairwise distinct.
/// Applying a substitution `σ` to a type `T` yields a new type `T'` by replacing each
/// occurrence of a variable `xi` in `T` with the term `ti` in `σ`.
#[derive(Clone, Debug, Default)]
pub struct Subst(HashMap<TypeVar, TypeId>);

impl Deref for Subst {
    type Target = HashMap<TypeVar, TypeId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Subst {
    /// Create a new substitution from a list of variable-type pairs
    pub fn new(vars: impl IntoIterator<Item = (TypeVar, TypeId)>) -> Self {
        Self(vars.into_iter().collect())
    }

    /// Insert a mapping from a variable to a type
    pub fn insert(&mut self, var: TypeVar, ty: TypeId) {
        self.0.insert(var, ty);
    }

    /// Get the type associated with a variable
    pub fn get(&self, var: &TypeVar) -> Option<&TypeId> {
        self.0.get(var)
    }

    /// Compose two substitutions by applying self to each type in the other and merging the results
    pub fn compose(&self, other: &Subst) -> Subst {
        let composition = other
            .iter()
            .map(|(var, ty)| (var, ty.apply(self)))
            .collect();
        self.union(composition)
    }
}

/// A type environment is a mapping from variables (in expressions) to polymorphic types.
#[derive(Clone, Debug, Default)]
pub struct TypeEnv(HashMap<Symbol, Polytype>);

impl Deref for TypeEnv {
    type Target = HashMap<Symbol, Polytype>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl TypeOps for TypeEnv {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.values().cloned().collect::<Vec<Polytype>>().ftv()
    }

    fn apply(&self, subst: &Subst) -> Self {
        TypeEnv(self.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
    }
}

impl TypeEnv {
    /// Create a new polymorphic type from a type
    pub fn generalize(&self, ty: &Type) -> Polytype {
        Polytype {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    fn infer(&self, expr: &Expr, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        let ans = match expr {
            // a local variable is typed as an instantiation of the corresponding type in the environment
            Expr::LocalItem(id) => {
                let ty = self.get(id).unwrap();
                (Default::default(), ty.instantiate(tvg))
            }

            // a module variable is typed as an instantiation of the corresponding type in the registry
            Expr::ModuleItem(id) => {
                let ty = self.get(id).unwrap();
                (Default::default(), ty.instantiate(tvg))
            }

            // a literal is typed as its primitive type
            Expr::Literal(lit) => match lit {
                Literal::Boolean(_) => (Default::default(), Type::Bool),
                Literal::Integer(_) => (Default::default(), Type::Int),
                Literal::String(_) => (Default::default(), Type::String),
            },

            Expr::Application { callee, args } => {
                let (s1, t1) = self.infer(callee, tvg)?;
                todo!()
            }
        };

        Ok(ans)
    }
}

pub struct TypeChecker {
    env: TypeEnv,
    tvg: TypeVarGen,
    ctx: IrCtx,
}

impl TypeChecker {
    pub fn check(mut self) -> Result<TypedIr> {
        // Fase 1: Construir entorno inicial con tipos declarados
        // Primero recolectar los IDs para evitar problemas de borrowing
        let type_decls: Vec<_> = self
            .ctx
            .decls
            .iter()
            .filter_map(|(_, decl)| {
                if let Decl::Type(name, ty_id) = decl {
                    Some((*name, *ty_id))
                } else {
                    None
                }
            })
            .collect();

        for (name, ty_id) in type_decls {
            let poly = Polytype {
                vars: vec![],
                ty: ty_id,
            };
            self.env.insert(name, poly);
        }

        // Fase 2: Recolectar las declaraciones que necesitamos inferir
        let let_decls: Vec<_> = self
            .ctx
            .decls
            .iter()
            .filter_map(|(_, decl)| {
                if let Decl::Let(name, expr_id) = decl {
                    Some((*name, *expr_id))
                } else {
                    None
                }
            })
            .collect();

        let expr_decls: Vec<_> = self
            .ctx
            .decls
            .iter()
            .filter_map(|(_, decl)| {
                if let Decl::Expr(expr_id) = decl {
                    Some(*expr_id)
                } else {
                    None
                }
            })
            .collect();

        // Fase 3: Inferir tipos sin borrowing conflicts
        let mut expr_types = HashMap::new();

        for (name, expr_id) in let_decls {
            let (subst, ty_id) = self.infer_expr(expr_id)?;
            let ty_applied = self.ctx.apply_subst(ty_id, &subst);

            // Generalizar y añadir al entorno
            let poly = self.env.generalize(ty_applied, &self.ctx);
            self.env.insert(name, poly);

            expr_types.insert(expr_id, ty_applied);
        }

        for expr_id in expr_decls {
            let (_, ty_id) = self.infer_expr(expr_id)?;
            expr_types.insert(expr_id, ty_id);
        }

        Ok(TypedIr {
            ctx: self.ctx,
            expr_types,
        })
    }
}
