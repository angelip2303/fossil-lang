use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::Deref;
use std::ops::DerefMut;
use std::result;

use thiserror::Error;

use crate::ast::Literal;
use crate::context::Symbol;
use crate::context::{Arena, NodeId};
use crate::ir::{Decl, Expr, ExprId, IrCtx};
use crate::module::{Binding, ModuleRegistry};

type Result<T> = result::Result<T, TypeError>;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Illegal recursive type: {0} appears in itself")]
    RecursiveType(TypeVar),

    #[error("Type mismatch: cannot unify types at {:?} and {:?}", .expected, .found)]
    TypeMismatch { expected: TypeId, found: TypeId },

    #[error("Arity mismatch: expected {expected} arguments, got {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("Should have been already resolved")]
    AlreadyResolved,
}

/// Operations on types
// trait TypeOps {
//     /// Find the set of free type variables in the type
//     fn ftv(&self, arena: &TypeArena) -> HashSet<TypeVar>;
//     /// Apply a substitution to the type
//     fn apply(&self, subst: &Subst, arena: &mut TypeArena) -> Self;
// }

// impl<T: TypeOps> TypeOps for Vec<T> {
//     fn ftv(&self) -> HashSet<TypeVar> {
//         self.iter().flat_map(|t| t.ftv()).collect()
//     }

//     fn apply(&self, subst: &Subst) -> Self {
//         self.iter().map(|x| x.apply(s)).collect()
//     }
// }

// Type variable for polymorphism
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

// impl TypeVar {
//     /// Attempt to bind a type variable to a type, returning an appropriate substitution
//     fn bind(&self, ty: TypeId, arena: &TypeArena) -> Result<Subst> {
//         if let Type::Var(var) = arena.get(ty)
//             && var == self
//         {
//             return Ok(Default::default());
//         }

//         match ty.ftv(arena).contains(self) {
//             true => Err(TypeError::RecursiveType(self.clone(), ty)),
//             false => Ok(Subst::singleton(*self, ty)),
//         }
//     }
// }

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

pub type TypeId = NodeId<Type>;
pub type TypeArena = Arena<Type>;

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

impl TypeArena {
    fn ftv(&self, ty: TypeId) -> HashSet<TypeVar> {
        match self.get(ty) {
            // primitive types have no free variables
            Type::Int | Type::String | Type::Bool => Default::default(),

            // for lists, we take the free type variables in the inner type
            Type::List(inner) => self.ftv(*inner),

            // for records, we take the free type variables in the field types
            Type::Record(fields) => fields.iter().flat_map(|(_, ty)| self.ftv(*ty)).collect(),

            // for a type variable, there is one free variable: the type variable itself
            Type::Var(var) => HashSet::from([*var]),

            // for functions, we take the all the free type variables in the parameters and return type
            Type::Function(params, ret) => {
                let mut ftv = params
                    .iter()
                    .flat_map(|param| self.ftv(*param))
                    .collect::<HashSet<_>>();
                ftv.extend(self.ftv(*ret));
                ftv
            }

            Type::Named(_) => todo!(),
        }
    }
}

// impl TypeOps for TypeId {
//     // fn ftv(&self, arena: &TypeArena) -> HashSet<TypeVar> {
//     //     match arena.get(*self) {
//     //         // primitive types have no free variables
//     //         Type::Int | Type::String | Type::Bool => Default::default(),

//     //         // for lists, we take the free type variables in the inner type
//     //         Type::List(inner) => inner.ftv(arena),

//     //         // for records, we take the free type variables in the field types
//     //         Type::Record(fields) => fields.iter().flat_map(|(_, ty)| ty.ftv(arena)).collect(),

//     //         // for a type variable, there is one free variable: the type variable itself
//     //         Type::Var(var) => HashSet::from([*var]),

//     //         // for functions, we take the all the free type variables in the parameters and return type
//     //         Type::Function(params, ret) => {
//     //             let mut ftv = params
//     //                 .iter()
//     //                 .flat_map(|param| param.ftv(arena))
//     //                 .collect::<HashSet<_>>();
//     //             ftv.extend(ret.ftv(arena));
//     //             ftv
//     //         }

//     //         Type::Named(_) => todo!(),
//     //     }
//     // }

//     fn apply(&self, subst: &Subst, arena: &mut TypeArena) -> Self {}
// }

// impl Type {
//     fn mgu(&self, other: &Self) -> Result<Subst> {
//         let subst = match (self, other) {
//             // if they both are primitive types, no substitution needs to be performed
//             (Type::Int, Type::Int) | (Type::String, Type::String) | (Type::Bool, Type::Bool) => {
//                 Default::default()
//             }

//             // if they both are list types, return the mgu of the inner types
//             (Type::List(inner1), Type::List(inner2)) => inner1.mgu(inner2)?,

//             // if they both are record types,
//             (Type::Record(fields1), Type::Record(fields2)) => todo!(),

//             // if one of them is a type variable, we can bind the variable to the other type
//             (Type::Var(var), ty) | (ty, Type::Var(var)) => var.bind(ty)?,

//             // for functions, we find the most general unifier of the parameters, apply the resulting
//             // substitution to the return type, find the most general unifier of the return types, and
//             // compose the substitutions
//             (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
//                 let mut subst = Default::default();
//                 for (param1, param2) in params1.iter().zip(params2) {
//                     subst = subst.compose(param1.mgu(param2)?)?;
//                 }
//                 subst.compose(ret1.mgu(ret2)?)?
//             }
//         };

//         Ok(subst)
//     }
// }

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

impl Polytype {
    pub fn new(vars: Vec<TypeVar>, ty: TypeId) -> Self {
        Polytype { vars, ty }
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
    pub fn singleton(var: TypeVar, ty: TypeId) -> Self {
        Self([(var, ty)].into_iter().collect())
    }

    /// Create a new substitution from a list of variable-type pairs
    pub fn new(vars: impl IntoIterator<Item = (TypeVar, TypeId)>) -> Self {
        Self(vars.into_iter().collect())
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

impl TypeEnv {
    fn ftv(&self, arena: &TypeArena) -> HashSet<TypeVar> {
        let mut ans = HashSet::default();
        for poly in self.values() {
            let ftv = arena.ftv(poly.ty);
            let bound = poly.vars.iter().copied().collect();
            ans.extend(ftv.difference(&bound));
        }
        ans
    }

    /// Create a new polymorphic type from a type
    pub fn generalize(&self, ty: TypeId, arena: &TypeArena) -> Polytype {
        let env_ftv = self.ftv(arena);
        let ty_ftv = arena.ftv(ty);
        let mut vars: Vec<TypeVar> = ty_ftv.difference(&env_ftv).copied().collect();
        vars.sort(); // deterministic
        Polytype { vars, ty }
    }
}

pub struct TypeChecker {
    env: TypeEnv,
    tvg: TypeVarGen,
    ctx: IrCtx,
    registry: ModuleRegistry,
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
            let poly = self.env.generalize(ty_applied, &self.ctx.types);
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

    fn infer(&mut self, expr: ExprId) -> Result<(Subst, TypeId)> {
        let expr = self.ctx.exprs.get(expr).clone();

        let ans = match expr {
            // a local variable is typed as an instantiation of the corresponding type in the environment
            Expr::LocalItem(id) => match self.ctx.decls.get(id) {
                Decl::Let(name, expr) => {
                    todo!("lookup?")
                }
                Decl::Type(_, ty) => (Default::default(), *ty),
                Decl::Expr(expr) => self.infer(*expr)?,
            },

            // a module variable is typed as an instantiation of the corresponding type in the registry
            Expr::ModuleItem(id) => match self.registry.get(id) {
                Binding::Function(func) => {
                    let signature = func.signature();
                    // TODO: apply polytype as a type
                    todo!()
                }
                Binding::Provider(_) => return Err(TypeError::AlreadyResolved),
            },

            // a literal is typed as its primitive type
            Expr::Literal(lit) => {
                let ty = match lit {
                    Literal::Boolean(_) => Type::Bool,
                    Literal::Integer(_) => Type::Int,
                    Literal::String(_) => Type::String,
                };
                (Default::default(), self.ctx.types.alloc(ty))
            }

            Expr::List(items) => todo!(),

            Expr::Record(fields) => todo!(),

            Expr::Function { params, body } => todo!(),

            Expr::Application { callee, args } => {
                let (s1, t1) = self.infer(callee)?;
                let actual = self.apply_subst_to_type(t1, &s1);

                let mut ans: Vec<TypeId> = Vec::new();
                for arg in args {
                    let (s2, t2) = self.infer(arg)?;
                    let subst = self.compose(&s1, &s2);
                    let ty = self.apply_subst_to_type(t2, &subst);
                    ans.push(ty);
                }

                let ret = self.ctx.types.alloc(Type::Var(self.tvg.next()));
                let expected = self.ctx.types.alloc(Type::Function(ans, ret));

                let s3 = self.mgu(actual, expected)?;

                (self.compose(&s1, &s3), self.apply_subst_to_type(ret, &s3))
            }
        };

        Ok(ans)
    }

    fn mgu(&mut self, ty_id: TypeId, other_id: TypeId) -> Result<Subst> {
        let ty = self.ctx.types.get(ty_id).clone();
        let other = self.ctx.types.get(other_id).clone();

        let subst = match (ty, other) {
            // if they both are primitive types, no substitution needs to be performed
            (Type::Int, Type::Int) | (Type::String, Type::String) | (Type::Bool, Type::Bool) => {
                Default::default()
            }

            (Type::Named(a), Type::Named(b)) if a == b => Default::default(),

            // if they both are list types, return the mgu of the inner types
            (Type::List(inner1), Type::List(inner2)) => self.mgu(inner1, inner2)?,

            // if they both are record types,
            (Type::Record(fields1), Type::Record(fields2)) => todo!(),

            // if one of them is a type variable, we can bind the variable to the other type
            (Type::Var(var), _) => self.bind(var, other_id)?,
            (_, Type::Var(var)) => self.bind(var, ty_id)?,

            // for functions, we find the most general unifier of the parameters, apply the resulting
            // substitution to the return type, find the most general unifier of the return types, and
            // compose the substitutions
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: params1.len(),
                        found: params2.len(),
                    });
                }

                let mut subst1 = Subst::default();
                for (param1, param2) in params1.iter().zip(params2) {
                    let subst2 = self.mgu(*param1, param2)?;
                    subst1 = self.compose(&subst1, &subst2);
                }
                let subst3 = self.mgu(ret1, ret2)?;
                self.compose(&subst1, &subst3)
            }

            _ => {
                return Err(TypeError::TypeMismatch {
                    expected: ty_id,
                    found: other_id,
                });
            }
        };

        Ok(subst)
    }

    fn bind(&self, var: TypeVar, id: TypeId) -> Result<Subst> {
        let ty = self.ctx.types.get(id).clone();

        if let Type::Var(v) = ty
            && v == var
        {
            return Ok(Default::default());
        }

        match self.ctx.types.ftv(id).contains(&var) {
            true => Err(TypeError::RecursiveType(var)),
            false => Ok(Subst::singleton(var, id)),
        }
    }

    fn apply_subst_to_type(&mut self, ty: TypeId, subst: &Subst) -> TypeId {
        match self.ctx.types.get(ty).clone() {
            // primitive types are not affected by substitution
            Type::Int | Type::String | Type::Bool => ty,

            // for lists, we apply the substitution to the inner type
            Type::List(inner) => {
                let inner = self.apply_subst_to_type(inner, subst);
                self.ctx.types.alloc(Type::List(inner))
            }

            // for records, we apply the substitution to the fields
            Type::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| (*name, self.apply_subst_to_type(*ty, subst)))
                    .collect();
                self.ctx.types.alloc(Type::Record(fields))
            }

            // if the variable is bound by the substitution, we replace it with the substituted type
            Type::Var(var) => subst.get(&var).copied().unwrap_or(ty),

            // for functions, we apply the substitution to the parameters and return type
            Type::Function(params, ty) => {
                let mut new = Vec::with_capacity(params.len());
                for param in params {
                    new.push(self.apply_subst_to_type(param, subst));
                }
                let ty = self.apply_subst_to_type(ty, subst);
                self.ctx.types.alloc(Type::Function(new, ty))
            }

            Type::Named(_) => todo!(),
        }
    }

    fn apply_subst_to_env(&mut self, env: &TypeEnv, subst: &Subst) -> TypeEnv {
        let mut ans = TypeEnv::default();

        for (name, poly) in env.iter() {
            let ty = self.apply_subst_to_type(poly.ty, subst);
            ans.insert(*name, Polytype::new(poly.vars.clone(), ty));
        }

        ans
    }

    /// Instantiate polytype to a monotype with fresh type variables. Replaces all bound type variables
    /// with fresh type variables and return the instantiated type.
    pub fn instantiate(&mut self, poly: &Polytype) -> TypeId {
        // in case of no bound variables, it already is a monotype, so return it
        if poly.vars.is_empty() {
            return poly.ty;
        }

        let newvars = poly
            .vars
            .iter()
            .map(|_| self.ctx.types.alloc(Type::Var(self.tvg.next())));

        let subst = Subst::new(poly.vars.iter().cloned().zip(newvars));

        self.apply_subst_to_type(poly.ty, &subst)
    }

    /// Compose two substitutions by applying self to each type in the other and merging the results
    fn compose(&mut self, subst: &Subst, other: &Subst) -> Subst {
        let mut composed = other
            .iter()
            .map(|(var, ty)| (*var, self.apply_subst_to_type(*ty, subst)))
            .collect::<HashMap<_, _>>();

        for (var, ty) in &subst.0 {
            composed.entry(*var).or_insert(*ty);
        }

        Subst(composed)
    }
}
