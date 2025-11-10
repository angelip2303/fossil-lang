use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use std::result;

use thiserror::Error;

use crate::ast::Literal;
use crate::context::Symbol;
use crate::ir::{Expr, Type};

pub type Result<T> = result::Result<T, TypeError>;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Illegal recursive type, {0:?} is recursive with {1:?}")]
    IllegalRecursiveType(TypeVar, Type),

    #[error("Cannot unify {0:?} with {1:?}")]
    CannotUnify(Type, Type),
}

trait Union {
    fn union(&self, other: &Self) -> Self;
}

/// Implement union for HashMap such that the value in `self` is used over the value in `other` in
/// the event of a collision.
impl<K, V> Union for HashMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    fn union(&self, other: &Self) -> Self {
        let mut res = self.clone();
        for (key, value) in other {
            res.entry(key.clone()).or_insert(value.clone());
        }
        res
    }
}

/// A type variable represents a type that is not constrained.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(usize);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

/// A source of unique type variables.
pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn new() -> TypeVarGen {
        TypeVarGen { supply: 0 }
    }
    pub fn next(&mut self) -> TypeVar {
        let v = TypeVar(self.supply);
        self.supply += 1;
        v
    }
}

impl TypeVar {
    /// Attempt to bind a type variable to a type, returning an appropriate substitution.
    fn bind(&self, ty: &Type) -> Result<Subst> {
        // Check for binding a variable to itself
        if let &Type::Var(ref u) = ty {
            if u == self {
                return Ok(Subst::new());
            }
        }

        // The occurs check prevents illegal recursive types.
        if ty.ftv().contains(self) {
            return Err(TypeError::IllegalRecursiveType(self.clone(), ty.clone()));
        }

        let mut s = Subst::new();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

/// A trait common to all things considered types.
trait Types {
    /// Find the set of free variables in a type.
    fn ftv(&self) -> HashSet<TypeVar>;
    /// Apply a substitution to a type.
    fn apply(&self, s: &Subst) -> Self;
}

impl<'a, T> Types for Vec<T>
where
    T: Types,
{
    // The free type variables of a vector of types is the union of the free type variables of each
    // of the types in the vector.
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(|x| x.ftv())
            .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
    }

    // To apply a substitution to a vector of types, just apply to each type in the vector.
    fn apply(&self, s: &Subst) -> Vec<T> {
        self.iter().map(|x| x.apply(s)).collect()
    }
}

impl Type {
    /// Most general unifier, a substitution S such that S(self) is congruent to S(other).
    fn mgu(&self, other: &Type) -> Result<Subst> {
        match (self, other) {
            // For functions, we find the most general unifier for the inputs, apply the resulting
            // substitution to the outputs, find the outputs' most general unifier, and finally
            // compose the two resulting substitutions.
            (&Type::Function(in1, out1), &Type::Function(in2, out2)) => {
                let sub1 = in1.mgu(in2)?;
                let sub2 = out1.apply(&sub1).mgu(&out2.apply(&sub1))?;
                Ok(sub1.compose(&sub2))
            }

            // If one of the types is variable, we can bind the variable to the type.
            // This also handles the case where they are both variables.
            (&Type::Var(ref v), t) => v.bind(t),
            (t, &Type::Var(ref v)) => v.bind(t),

            // If they are both primitives, no substitution needs to be done.
            (&Type::Int, &Type::Int)
            | (&Type::Bool, &Type::Bool)
            | (&Type::String, &Type::String) => Ok(Subst::new()),

            (t1, t2) => Err(TypeError::CannotUnify(t1.clone(), t2.clone())),
        }
    }
}

impl Types for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            // For a type variable, there is one free variable: the variable itself.
            &Type::Var(ref s) => [s.clone()].iter().cloned().collect(),

            // Primitive types have no free variables
            &Type::Int | &Type::Bool | &Type::String => HashSet::new(),

            // For functions, we take the union of the free type variables of the input and output.
            &Type::Function(i, o) => i.ftv().union(&o.ftv()).cloned().collect(),
        }
    }

    fn apply(&self, s: &Subst) -> Type {
        match self {
            // If this type references a variable that is in the substitution, return it's
            // replacement type. Otherwise, return the existing type.
            Type::Var(n) => s.get(n).cloned().unwrap_or(self.clone()),

            // To apply to a function, we simply apply to each of the input and output.
            Type::Function(t1, t2) => Type::Function(t1.apply(s), t2.apply(s)),

            // A primitive type is changed by a substitution.
            Type::Int | Type::Bool | Type::String => self.clone(),
        }
    }
}

/// A polytype is a type in which there are a number of for-all quantifiers, i.e. some parts of the
/// type may not be concrete but instead correct for all possible types.
#[derive(Clone, Debug)]
pub struct Polytype {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl Types for Polytype {
    /// The free type variables in a polytype are those that are free in the internal type and not
    /// bound by the variable mapping.
    fn ftv(&self) -> HashSet<TypeVar> {
        self.ty
            .ftv()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    /// Substitutions are applied to free type variables only.
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
    /// Instantiates a polytype into a type. Replaces all bound type variables with fresh type
    /// variables and return the resulting type.
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Type {
        let newvars = self.vars.iter().map(|_| Type::Var(tvg.next()));
        self.ty
            .apply(&Subst(self.vars.iter().cloned().zip(newvars).collect()))
    }
}

/// A substitution is a mapping from type variables to types.
#[derive(Clone, Debug)]
pub struct Subst(HashMap<TypeVar, Type>);

impl Deref for Subst {
    type Target = HashMap<TypeVar, Type>;
    fn deref(&self) -> &HashMap<TypeVar, Type> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TypeVar, Type> {
        &mut self.0
    }
}

impl Subst {
    /// Construct an empty substitution.
    fn new() -> Subst {
        Subst(HashMap::new())
    }

    /// To compose two substitutions, we apply self to each type in other and union the resulting
    /// substitution with self.
    fn compose(&self, other: &Subst) -> Subst {
        Subst(
            self.union(
                &other
                    .iter()
                    .map(|(k, v)| (k.clone(), v.apply(self)))
                    .collect(),
            ),
        )
    }
}

/// A type environment is a mapping from symbols to polytypes.
#[derive(Clone, Debug)]
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

impl Types for TypeEnv {
    /// The free type variables of a type environment is the union of the free type variables of
    /// each polytype in the environment.
    fn ftv(&self) -> HashSet<TypeVar> {
        self.values()
            .map(|x| x.clone())
            .collect::<Vec<Polytype>>()
            .ftv()
    }

    /// To apply a substitution, we just apply it to each polytype in the type environment.
    fn apply(&self, s: &Subst) -> TypeEnv {
        TypeEnv(self.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
    }
}

impl TypeEnv {
    /// Construct an empty type environment.
    pub fn new() -> TypeEnv {
        TypeEnv(HashMap::new())
    }

    /// Perform type inference on an expression and return the resulting type, if any.
    pub fn type_inference(&self, exp: &Expr, tvg: &mut TypeVarGen) -> Result<Type> {
        let (s, t) = self.ti(exp, tvg)?;
        Ok(t.apply(&s))
    }

    /// Generalize creates a polytype
    fn generalize(&self, ty: &Type) -> Polytype {
        Polytype {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }

    /// The meat of the type inference algorithm.
    fn ti(&self, exp: &Expr, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        let (s, t) = match exp {
            // A variable is typed as an instantiation of the corresponding type in the
            // environment.
            Expr::Var(v) => match self.get(v) {
                Some(s) => (Subst::new(), s.instantiate(tvg)),
                None => unreachable!(), // TODO: in principle this should have been already checked
            },

            // A literal is typed as it's primitive type.
            Expr::Literal(lit) => (
                Subst::new(),
                match lit {
                    Literal::Integer(_) => Type::Int,
                    Literal::Boolean(_) => Type::Bool,
                    Literal::String(_) => Type::String,
                },
            ),

            // An abstraction is typed by:
            // * Removing any existing type with the same name as the argument to prevent name
            // clashes.
            // * Inserting a new type variable for the argument.
            // * Inferring the type of the expression in the new environment to define the type of
            // the expression.
            // * Applying the resulting substitution to the argument to define the type of the
            // argument.
            Expr::Abs(n, e) => {
                let tv = Type::Var(tvg.next());
                let mut env = self.clone();
                env.remove(n);
                env.insert(
                    n.clone(),
                    Polytype {
                        vars: Vec::new(),
                        ty: tv.clone(),
                    },
                );
                let (s1, t1) = env.ti(e, tvg)?;
                (s1.clone(), Type::Function(tv.apply(&s1), t1))
            }

            // An application is typed by:
            // * Inferring the type of the callee.
            // * Applying the resulting substitution to the argument and inferring it's type.
            // * Finding the most general unifier for the callee type and a function from the
            // argument type to a new type variable. This combines the previously known type of the
            // function and the type as it is now being used.
            // * Applying the unifier to the new type variable.
            Expr::Application { callee, args } => {
                let (s1, t1) = self.ti(callee, tvg)?;
                let (s2, t2) = self.apply(&s1).ti(args, tvg)?;
                let tv = Type::Var(tvg.next());
                let s3 = t1.apply(&s2).mgu(&Type::Function(t2, tv.clone()))?;
                (s3.compose(&s2.compose(&s1)), tv.apply(&s3))
            }
        };

        Ok((s, t))
    }
}
