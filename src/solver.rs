use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;

use crate::ast::*;
use crate::error::Result;
use crate::module::ModuleRegistry;

trait Union {
    fn union(&mut self, other: &Self) -> Self;
}

impl<K, V> Union for HashMap<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone,
{
    fn union(&mut self, other: &Self) -> Self {
        let mut result = self.clone();
        for (key, value) in other {
            result.entry(key.clone()).or_insert(value.clone());
        }
        result
    }
}

/// A type variable represents a type that is not constrained yet: t0, t1, t2, ....
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(pub usize);

impl TypeVar {
    /// Attempt to bind a type variable to a type, returning an appropiate substitution
    fn bind(&self, ty: &Type) -> Result<Subst> {
        // check for binding a variable to itself
        if let &Type::Var(ref var) = ty {
            if var == self {
                return Ok(Subst::new());
            }
        }

        // the occurs check prevents illegal recursive types
        if ty.ftv().contains(self) {
            todo!("Occurs check failed")
        }

        let mut s = Subst::new();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t({})", self.0)
    }
}

/// A source of unique type variables, used to generate fresh type variables
pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn new() -> Self {
        TypeVarGen { supply: 0 }
    }

    pub fn next(&mut self) -> TypeVar {
        let var = TypeVar(self.supply);
        self.supply += 1;
        var
    }
}

/// A trait common to all things considered types
trait Typed {
    /// Find the set of free variables in a type
    fn ftv(&self) -> HashSet<TypeVar>;

    /// Apply substitution to a type
    fn apply(&self, subst: &Subst) -> Self;
}

impl<'a, T> Typed for Vec<T>
where
    T: Typed,
{
    // the free variables of a vector of types is the union of the free type variables of each
    // of the types in the vector
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(|t| t.ftv())
            .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
    }

    // to apply a substitution to a vector of types, apply the substitution to each type in the vector
    fn apply(&self, subst: &Subst) -> Self {
        self.iter().map(|t| t.apply(subst)).collect()
    }
}

/// A monotype representing concrete types
#[derive(Clone, Debug)]
pub enum Type {
    Var(TypeVar),
    Unit,
    Int,
    String,
    Bool,
    List(Box<Self>),
    Func(Box<Self>, Box<Self>),
    Record(HashMap<String, Box<Self>>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(var) => write!(f, "t({})", var.0),
            Type::Unit | Type::Int | Type::String | Type::Bool => write!(f, "{:?}", self),
            Type::List(r#type) => write!(f, "[{}]", r#type),
            Type::Func(arg, ret) => write!(f, "({} -> {})", arg, ret),
            Type::Record(fields) => write!(
                f,
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    /// Most general unifier, a substitution S such that S(self) is congruent to S(other)
    pub fn mgu(&self, other: &Self) -> Result<Subst> {
        match (self, other) {
            // for functions, we find the most general unifier of the inputs, apply the resulting
            // substitution to the output, find the outputs' most general unifier, and finally
            // compose the two resulting substitutions
            (Type::Func(arg1, ret1), Type::Func(arg2, ret2)) => {
                let subst1 = arg1.mgu(&arg2)?;
                let subst2 = ret1.apply(&subst1).mgu(&ret2.apply(&subst1))?;
                Ok(subst1.compose(&subst2))
            }

            (Type::List(a), Type::List(b)) => a.mgu(b),

            // if one of the types is a variable, we can bind the variable to the type. This also
            // handles the case where both are variables
            (Type::Var(var), t) => var.bind(t),
            (t, Type::Var(var)) => var.bind(t),

            // if they both are primitive types, no substitution is needed
            (Type::Unit, Type::Unit)
            | (Type::Int, Type::Int)
            | (Type::String, Type::String)
            | (Type::Bool, Type::Bool) => Ok(Subst::new()),

            // if they both are records, they must have the same fields
            (Type::Record(a), Type::Record(b)) => {
                if a.len() != b.len()
                    || a.keys().collect::<HashSet<_>>() != b.keys().collect::<HashSet<_>>()
                {
                    todo!("Struct fields mismatch")
                }

                let mut subst = Subst::new();
                for (name, t1) in a {
                    let t2 = &b[name];
                    let s = t1.apply(&subst).mgu(&t2.apply(&subst))?;
                    subst = subst.compose(&s);
                }

                Ok(subst)
            }

            _ => todo!("Type mismatch"),
        }
    }
}

impl Typed for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            // for a type variable, there is one free variable: the variable itself
            Type::Var(var) => [var.clone()].into_iter().collect(),

            // primitive types have no free variables
            Type::Unit | Type::Int | Type::String | Type::Bool => HashSet::new(),

            Type::List(inner) => inner.ftv(),

            // for functions, we take the union of the free variables of the input and output types
            Type::Func(arg, ret) => arg.ftv().union(&ret.ftv()).cloned().collect(),

            // for records, we take the union of the free variables of the fields
            Type::Record(fields) => fields.values().flat_map(|t| t.ftv()).collect(),
        }
    }

    fn apply(&self, sub: &Subst) -> Type {
        match self {
            // if this type references a variable that is in the substitution, return its replacement
            // type. Otherwise, return the type itself.
            Type::Var(var) => sub.get(var).cloned().unwrap_or(Type::Var(var.clone())),

            // a primitive type is changed by the substitution
            Type::Unit | Type::Int | Type::String | Type::Bool => self.clone(),

            Type::List(inner) => Type::List(Box::new(inner.apply(sub))),

            // to apply to a function,we simply apply to each of the input and output types
            Type::Func(arg, ret) => Type::Func(Box::new(arg.apply(sub)), Box::new(ret.apply(sub))),

            // for records, we apply to each of the fields
            Type::Record(fields) => Type::Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (k.clone(), Box::new(v.apply(sub))))
                    .collect(),
            ),
        }
    }
}

/// A polytype is a type in which there are a number of for-all quantifiers, i.e. some parts of the
/// type may not be concret but instead correct for all possible types
#[derive(Clone, Debug)]
pub struct PolyType {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl Typed for PolyType {
    /// The free type variables in a polytype are the variables are those that are free in the internal type and not
    /// bound in the variable mapping
    fn ftv(&self) -> HashSet<TypeVar> {
        self.ty
            .ftv()
            .difference(&self.vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    /// Substitutions are applied to free type variables only
    fn apply(&self, subst: &Subst) -> PolyType {
        PolyType {
            vars: self.vars.clone(),
            ty: {
                let mut sub = subst.clone();
                for var in &self.vars {
                    sub.remove(var);
                }
                self.ty.apply(&sub)
            },
        }
    }
}

impl PolyType {
    /// Instantiates a polytype into a type. Replaces all bound type variables with fresh type
    /// variables and return the resulting type
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Type {
        let newvars = self.vars.iter().map(|_| Type::Var(tvg.next()));
        self.ty
            .apply(&Subst(self.vars.iter().cloned().zip(newvars).collect()))
    }
}

/// A substitution is a mapping from type variables to types
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
    fn new() -> Self {
        Subst(HashMap::new())
    }

    fn compose(&self, other: &Subst) -> Subst {
        Subst(
            self.clone().union(
                &other
                    .iter()
                    .map(|(var, ty)| (var.clone(), ty.apply(self)))
                    .collect(),
            ),
        )
    }
}

/// A type environment is a mapping from variables to polyypes
#[derive(Clone, Default)]
pub struct TypeEnv(HashMap<NodeId, PolyType>);

impl Deref for TypeEnv {
    type Target = HashMap<NodeId, PolyType>;
    fn deref(&self) -> &HashMap<NodeId, PolyType> {
        &self.0
    }
}

impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut HashMap<NodeId, PolyType> {
        &mut self.0
    }
}

impl Typed for TypeEnv {
    /// The free variables of a type environment is the union of the free type variables of
    /// each polytype in the environment
    fn ftv(&self) -> HashSet<TypeVar> {
        self.values()
            .map(|x| x.clone())
            .collect::<Vec<PolyType>>()
            .ftv()
    }

    /// To apply a substitution, we just apply it to each polytype in the type environment
    fn apply(&self, s: &Subst) -> TypeEnv {
        TypeEnv(self.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
    }
}

impl TypeEnv {
    /// Generalize creates a polytype
    fn generalize(&self, ty: &Type) -> PolyType {
        PolyType {
            vars: ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty: ty.clone(),
        }
    }
}

#[derive(Clone)]
pub struct Globals {
    pub modules: ModuleRegistry, // Aquí van TODOS los módulos
}

impl Globals {
    pub fn new() -> Self {
        Self {
            modules: ModuleRegistry::new(),
        }
    }
}

#[derive(Clone)]
pub struct Context {
    pub globals: Globals,
    pub type_env: TypeEnv,
    pub ast: Arc<Ast>,
}

impl Context {
    pub fn new(globals: Globals, ast: Arc<Ast>) -> Self {
        Self {
            globals,
            type_env: TypeEnv::default(),
            ast,
        }
    }

    /// Perform type inference on an expression and return the resulting type, if any.
    pub fn type_inference(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<Type> {
        let (s, t) = self.infer_stmt(node_id, tvg)?;
        Ok(t.apply(&s))
    }

    fn infer_stmt(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        match &self.ast.get_stmt(node_id).kind.clone() {
            StmtKind::Import { path, alias } => {
                // populate the environment with the types of the module's exports
                todo!("Implement module opening")
            }

            // when a type definition is encountered, the type inference is as follows:
            // * infer the type of the right-hand side of the type definition
            // * insert to the environment the type of the right-hand side of the type definition, having as a
            //   name the left-hand side of the type definition
            StmtKind::Type { name, ty } => {
                let kind = self.ast.get_type(*ty).kind.clone();

                let ty = match kind {
                    TypeKind::Provider(provider_path, args) => {
                        let segments: Vec<&str> = provider_path
                            .iter()
                            .map(|id| self.ast.get_name(*id))
                            .collect();

                        let provider = match self.globals.modules.resolve(&segments) {
                            Some(crate::module::Lookup::Provider(p)) => p,
                            _ => todo!("provider not found"),
                        };

                        let name = self.ast.get_name(*name);
                        let module = provider.generate_module(name, &self.ast, &args)?;

                        self.globals.modules.register(module);

                        provider.provide_type(&self.ast, &args)?
                    }

                    _ => self.resolve_type(*ty)?,
                };

                self.type_env.insert(
                    *name,
                    PolyType {
                        vars: vec![],
                        ty: ty.clone(),
                    },
                );

                Ok((Subst::new(), ty))
            }

            // a let expression is typed by:
            // * removing any exiting type with the same name as the binding variable to prevent
            //   name clashes (shadowing).
            // * inferring the type of the binding
            // * applying the resulting substitution to the environment and generalizing to the binding type
            // * inserting the generalized type to the binding variable in the new environment
            // * applying the substitution for the binding to the environment and inferring the type of the expression
            StmtKind::Let { name, value } => {
                let (s1, t1) = self.infer_expr(*value, tvg)?;
                let env1 = self.type_env.apply(&s1);
                let poly = env1.generalize(&t1);
                self.type_env = env1;
                self.type_env.insert(*name, poly);
                Ok((s1, t1))
            }

            StmtKind::Expr(expr) => self.infer_expr(*expr, tvg),

            _ => unimplemented!(),
        }
    }

    pub fn infer_expr(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        let kind = self.ast.get_expr(node_id).kind.clone();

        match &kind {
            // a variable is typed as an instantiation of the corresponding type in the
            // environment
            ExprKind::Identifier(name) => match self.type_env.get(&name) {
                Some(poly) => Ok((Subst::new(), poly.instantiate(tvg))),
                None => todo!("Unbound variable"),
            },

            ExprKind::Path(segments) => {
                let segments: Vec<&str> =
                    segments.iter().map(|id| self.ast.get_name(*id)).collect();

                let ty = match self.globals.modules.resolve(&segments) {
                    Some(crate::module::Lookup::Function(f)) => f.ty(),
                    Some(crate::module::Lookup::Module(_)) => {
                        todo!("Cannot use module as value")
                    }
                    Some(crate::module::Lookup::Provider(_)) => {
                        todo!("Cannot use provider as value")
                    }
                    None => {
                        let path_str = segments.join(".");
                        todo!("Unresolved path: {}", path_str)
                    }
                };

                return Ok((Subst::new(), ty));
            }

            // a literal is typed as its primitive type
            ExprKind::Unit => Ok((Subst::new(), Type::Unit)),
            ExprKind::Integer(_) => Ok((Subst::new(), Type::Int)),
            ExprKind::Boolean(_) => Ok((Subst::new(), Type::Bool)),
            ExprKind::String(_) => Ok((Subst::new(), Type::String)),

            // lists are sequence of elems having the same type
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    let t = Type::Var(tvg.next());
                    Ok((Subst::new(), Type::List(Box::new(t))))
                } else {
                    let (mut s, first_ty) = self.infer_expr(elems[0], tvg)?;

                    for &elem in &elems[1..] {
                        self.type_env = self.type_env.apply(&s);
                        let (s2, ty_i) = self.infer_expr(elem, tvg)?;
                        let s3 = first_ty.apply(&s2).mgu(&ty_i)?;
                        s = s3.compose(&s2.compose(&s));
                    }

                    Ok((s.clone(), Type::List(Box::new(first_ty.apply(&s)))))
                }
            }

            // a record is a complex data structure, having two main components:
            // * a base type, which must be a Spread from another record
            // * a set of fields, each with a type
            ExprKind::Record(fields) => {
                let mut subst = Subst::new();
                let mut map = HashMap::new();

                for (name, field) in fields {
                    self.type_env = self.type_env.apply(&subst);
                    let (s_field, t_field) = self.infer_expr(*field, tvg)?;
                    subst = subst.compose(&s_field);
                    map.insert(self.ast.get_name(*name).to_string(), Box::new(t_field));
                }

                Ok((subst, Type::Record(map)))
            }

            // an application is typed by:
            // * inferring the type of the callee
            // * applying the resulting substitution to the argument and inferring its type
            // * finding the most general unifier of the callee and  a function from the
            //   argument type to a new type variable. This combines the previously known type of the
            //   function and the type as it is now being used
            // * applying the unifier to the new type variable
            ExprKind::Call { callee, arg } => {
                let (s1, t1) = self.infer_expr(*callee, tvg)?;
                self.type_env = self.type_env.apply(&s1);
                let (s2, t2) = self.infer_expr(*arg, tvg)?;
                let tv = Type::Var(tvg.next());
                let s3 = t1
                    .apply(&s2)
                    .mgu(&Type::Func(Box::new(t2), Box::new(tv.clone())))?;
                Ok((s3.compose(&s2.compose(&s1)), tv.apply(&s3)))
            }

            ExprKind::Function { param, body } => {
                let tv = match param.ty {
                    Some(ty_id) => self.resolve_type(ty_id)?,
                    None => Type::Var(tvg.next()),
                };

                let mut new_env = self.type_env.clone();
                new_env.insert(
                    param.name,
                    PolyType {
                        vars: vec![],
                        ty: tv.clone(),
                    },
                );

                let old_env = std::mem::replace(&mut self.type_env, new_env);
                let (s1, t1) = self.infer_expr(*body, tvg)?;
                self.type_env = old_env;

                Ok((
                    s1.clone(),
                    Type::Func(Box::new(tv.apply(&s1)), Box::new(t1)),
                ))
            }

            ExprKind::BinaryOp { left, op, right } => {
                let (s1, t1) = self.infer_expr(*left, tvg)?;
                let expected = self.infer_expr(*right, tvg)?;
                let s2 = t1.mgu(&expected.1)?;
                Ok((s2.compose(&s1), expected.1.apply(&s2)))
            }

            ExprKind::Pipe { left, right } => {
                let (s1, t1) = self.infer_expr(*left, tvg)?;
                self.type_env = self.type_env.apply(&s1);
                let (s2, t2) = self.infer_expr(*right, tvg)?;
                let tv = Type::Var(tvg.next());
                let s3 = t2
                    .apply(&s2)
                    .mgu(&Type::Func(Box::new(t1), Box::new(tv.clone())))?;
                Ok((s3.compose(&s2.compose(&s1)), tv.apply(&s3)))
            }

            // when the dot operator is used, the left-hand side must be a variable, while the right-hand side must be
            // another variable, a field in the struct or record
            ExprKind::MemberAccess { object, field } => {
                let (s1, t1) = self.infer_expr(*object, tvg)?;
                let fields = match t1 {
                    Type::Record(fields) => fields,
                    _ => todo!("member access on non-record"),
                };

                let field_name = self.ast.get_name(*field);
                match fields.get(field_name) {
                    Some(field_type) => Ok((s1, *field_type.clone())),
                    None => todo!("field not found"),
                }
            }

            ExprKind::Cast { expr, ty } => {
                let (s1, _) = self.infer_expr(*expr, tvg)?;
                let t2 = self.resolve_type(*ty)?;
                Ok((s1, t2))
            }

            ExprKind::TypeAnnotation { expr, ty } => {
                let (s1, t1) = self.infer_expr(*expr, tvg)?;
                let annotated_ty = self.resolve_type(*ty)?;
                let s2 = t1.mgu(&annotated_ty)?;
                Ok((s2.compose(&s1), annotated_ty.apply(&s2)))
            }

            _ => unimplemented!(),
        }
    }

    pub fn resolve_type(&mut self, type_id: NodeId) -> Result<Type> {
        let ast = self.ast.clone(); // TODO: this clone should be avoided
        let ty = ast.get_type(type_id);

        match &ty.kind {
            TypeKind::Unit => Ok(Type::Unit),
            TypeKind::Int => Ok(Type::Int),
            TypeKind::Bool => Ok(Type::Bool),
            TypeKind::String => Ok(Type::String),

            TypeKind::List(inner_id) => {
                let inner = self.resolve_type(*inner_id)?;
                Ok(Type::List(Box::new(inner)))
            }

            TypeKind::Function { from, to } => {
                let from = self.resolve_type(*from)?;
                let to = self.resolve_type(*to)?;
                Ok(Type::Func(Box::new(from), Box::new(to)))
            }

            TypeKind::Record(field_pairs) => {
                let mut fields = HashMap::new();
                for (name_id, type_id) in field_pairs {
                    let name = self.ast.get_name(*name_id).to_string();
                    let ty = self.resolve_type(*type_id)?;
                    fields.insert(name, Box::new(ty));
                }
                Ok(Type::Record(fields))
            }

            TypeKind::Var(path) => match self.type_env.get(&path[0]) {
                Some(poly) => Ok(poly.ty.clone()),
                None => todo!("Error"),
            },

            TypeKind::Provider(provider, args) => {
                let segments: Vec<&str> =
                    provider.iter().map(|id| self.ast.get_name(*id)).collect();

                let provider = match self.globals.modules.resolve(&segments) {
                    Some(crate::module::Lookup::Provider(p)) => p,
                    Some(crate::module::Lookup::Module(_)) => todo!("cannot use module as type"),
                    Some(crate::module::Lookup::Function(_)) => {
                        todo!("cannot use function as type")
                    }
                    _ => todo!("provider not found"),
                };

                let expected = provider.param_types();
                if args.len() != expected.len() {
                    todo!("expected arguments do not match actual");
                }

                for (arg, exp_ty) in args.iter().zip(expected.iter()) {
                    let (_, arg_ty) = self.infer_expr(arg.value, &mut TypeVarGen::new())?;
                    arg_ty.mgu(exp_ty)?;
                }

                provider.provide_type(&self.ast, args)
            }
        }
    }
}
