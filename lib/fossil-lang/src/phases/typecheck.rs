use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};

use crate::ast::*;
use crate::context::Symbol;
use crate::error::TypeError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingId, BindingRef, ResolutionTable, ResolvedProgram, TypedProgram};

/// A type variable generator
#[derive(Default)]
pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn next(&mut self) -> TypeVar {
        let v = TypeVar(self.supply);
        self.supply += 1;
        v
    }
}

impl TypeVar {
    /// Attempt to bind a type variable to a type, returning an appropriate substitution
    fn bind(&self, ty: TypeId, ast: &Ast) -> Result<Subst, TypeError> {
        if let Type::Var(v) = ast.types.get(ty)
            && v == self
        {
            return Ok(Subst::default());
        }

        if ty.ftv(ast).contains(self) {
            return Err(TypeError::InfiniteType(*self));
        }

        let mut s = Subst::default();
        s.insert(*self, ty);
        Ok(s)
    }
}

impl TypeId {
    /// Free type variables in a type
    fn ftv(&self, ast: &Ast) -> HashSet<TypeVar> {
        match ast.types.get(*self) {
            Type::Var(var) => HashSet::from([*var]),

            Type::List(inner) => inner.ftv(ast),

            Type::Record(fields) => fields.iter().flat_map(|(_, ty)| ty.ftv(ast)).collect(),

            Type::Function(params, ret) => {
                let mut ans: HashSet<_> = params.iter().flat_map(|param| param.ftv(ast)).collect();
                ans.extend(ret.ftv(ast));
                ans
            }

            Type::Primitive(_) => HashSet::new(),

            _ => unreachable!(),
        }
    }

    fn is_function(&self, ast: &Ast) -> bool {
        match ast.types.get(*self) {
            Type::Function(..) => true,
            Type::List(inner) => inner.is_function(ast),
            Type::Record(fields) => fields.iter().any(|(_, ty)| ty.is_function(ast)),
            _ => false,
        }
    }
}

impl Polytype {
    fn ftv(&self, ast: &Ast) -> HashSet<TypeVar> {
        let ty_ftv = self.ty.ftv(ast);
        let bound: HashSet<_> = self.forall.iter().copied().collect();
        ty_ftv.difference(&bound).cloned().collect()
    }

    /// Instantiate a polytype with fresh type variables
    fn instantiate(&self, tvg: &mut TypeVarGen, ast: &mut Ast) -> TypeId {
        if self.forall.is_empty() {
            return self.ty;
        }

        let fresh_vars: HashMap<TypeVar, TypeId> = self
            .forall
            .iter()
            .map(|old_var| (*old_var, ast.types.alloc(Type::Var(tvg.next()))))
            .collect();

        let subst = Subst(fresh_vars);
        subst.apply(self.ty, ast)
    }
}

/// A substitution is a mapping `σ: V -> T` from variables to types
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
    /// Apply substitution to a type
    pub fn apply(&self, ty_id: TypeId, ast: &mut Ast) -> TypeId {
        self.apply_with_cache(ty_id, ast, &mut HashMap::new())
    }

    fn apply_with_cache(
        &self,
        ty: TypeId,
        ast: &mut Ast,
        cache: &mut HashMap<TypeId, TypeId>,
    ) -> TypeId {
        if let Some(&cached) = cache.get(&ty) {
            return cached;
        }

        let result = match ast.types.get(ty).clone() {
            Type::Var(var) => match self.get(&var) {
                Some(&ty) => self.apply_with_cache(ty, ast, cache),
                None => ty,
            },

            Type::List(inner) => match self.apply_with_cache(inner, ast, cache) {
                new_inner if new_inner == inner => ty,
                new_inner => ast.types.alloc(Type::List(new_inner)),
            },

            Type::Record(fields) => {
                let new_fields: Vec<_> = fields
                    .iter()
                    .map(|(name, field_ty)| (*name, self.apply_with_cache(*field_ty, ast, cache)))
                    .collect();

                match new_fields == fields {
                    true => ty,
                    false => ast.types.alloc(Type::Record(new_fields)),
                }
            }

            Type::Function(params, ret) => {
                let new_params: Vec<_> = params
                    .iter()
                    .map(|param| self.apply_with_cache(*param, ast, cache))
                    .collect();

                let new_ret = self.apply_with_cache(ret, ast, cache);

                match new_ret == ret && new_params == params {
                    true => ty,
                    false => ast.types.alloc(Type::Function(new_params, new_ret)),
                }
            }

            Type::Primitive(_) => ty,

            Type::Named(_) | Type::Provider { .. } => unreachable!(),
        };

        cache.insert(ty, result);
        result
    }

    /// Compose two substitutions: self ∘ other
    pub fn compose(&self, other: &Subst, ast: &mut Ast) -> Subst {
        let mut composed = HashMap::new();

        for (&var, &ty) in other.iter() {
            composed.insert(var, self.apply(ty, ast));
        }

        for (&var, &ty) in self.iter() {
            composed.entry(var).or_insert(ty);
        }

        Subst(composed)
    }
}

/// Type environment: Γ
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
    /// Generalize a type with respect to this environment
    fn generalize(&self, ty: TypeId, ast: &Ast) -> Polytype {
        let env_ftv = self.values().flat_map(|poly| poly.ftv(ast)).collect();
        let mut forall: Vec<TypeVar> = ty.ftv(ast).difference(&env_ftv).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype { forall, ty }
    }
}

pub struct TypeChecker<'a> {
    registry: &'a ModuleRegistry,
    tvg: TypeVarGen,
    expr_types: HashMap<ExprId, TypeId>,
    function_types: HashMap<BindingId, Polytype>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            tvg: Default::default(),
            expr_types: Default::default(),
            function_types: Default::default(),
        }
    }

    pub fn check(mut self, program: ResolvedProgram) -> Result<TypedProgram, TypeError> {
        let ResolvedProgram {
            mut ast,
            symbols,
            resolution,
        } = program;

        let mut env = TypeEnv::default();

        let decl_ids: Vec<_> = ast.decls.iter().map(|(id, _)| id).collect();
        for decl_id in decl_ids {
            match ast.decls.get(decl_id).clone() {
                Decl::Type { name, ty } => {
                    env.insert(name, Polytype::mono(ty));
                }

                Decl::Let { name, value } => {
                    let (subst, ty) = self.infer(&env, value, &mut ast, &resolution)?;
                    let ty = subst.apply(ty, &mut ast);
                    let poly = env.generalize(ty, &ast);
                    env.insert(name, poly);
                }

                Decl::Expr(expr_id) => {
                    self.infer(&env, expr_id, &mut ast, &resolution)?;
                }
            }
        }

        Ok(TypedProgram {
            ast,
            symbols,
            resolution,
            expr_types: self.expr_types.clone(),
        })
    }

    fn infer(
        &mut self,
        env: &TypeEnv,
        expr_id: ExprId,
        ast: &mut Ast,
        resolution: &ResolutionTable,
    ) -> Result<(Subst, TypeId), TypeError> {
        // we have a cached type representation (memoization), which allows us to exploit
        // the capabilities of a Flattened AST, where child expressions appear before their parents.
        // Thus, recursive calls from parents can just retrieve the type from the cache.
        if let Some(&ty) = self.expr_types.get(&expr_id) {
            return Ok((Subst::default(), ty));
        }

        let (subst, ty) = match ast.exprs.get(expr_id).clone() {
            Expr::Literal(lit) => {
                let ty = match lit {
                    Literal::Boolean(_) => Type::Primitive(PrimitiveType::Bool),
                    Literal::Integer(_) => Type::Primitive(PrimitiveType::Int),
                    Literal::String(_) => Type::Primitive(PrimitiveType::String),
                };

                let ty_id = ast.types.alloc(ty);
                (Subst::default(), ty_id)
            }

            Expr::Identifier(path) => {
                let binding = resolution.exprs.get(&expr_id).unwrap();

                let poly = match binding {
                    BindingRef::Local(decl_id) => match ast.decls.get(*decl_id) {
                        Decl::Let { name, .. } => env.get(name).unwrap().clone(),
                        _ => return Err(TypeError::InvalidBinding),
                    },

                    BindingRef::Module(binding_id) => self.get_function_signature(*binding_id, ast),

                    BindingRef::Parameter { function: _ } => {
                        let name = match path {
                            Path::Simple(n) => n,
                            _ => unreachable!(),
                        };

                        env.get(&name)
                            .expect("Parameter should be in environment")
                            .clone()
                    }
                };

                let ty = poly.instantiate(&mut self.tvg, ast);
                (Subst::default(), ty)
            }

            Expr::List(items) => {
                if items.is_empty() {
                    let var = self.tvg.next();
                    let elem_ty = ast.types.alloc(Type::Var(var));
                    let list_ty = ast.types.alloc(Type::List(elem_ty));
                    (Subst::default(), list_ty)
                } else {
                    let (mut subst, elem_ty) = self.infer(env, items[0], ast, resolution)?;

                    for &item in &items[1..] {
                        let (s1, item_ty) = self.infer(env, item, ast, resolution)?;
                        subst = subst.compose(&s1, ast);

                        let elem_ty = subst.apply(elem_ty, ast);
                        let s2 = self.unify(elem_ty, item_ty, ast)?;
                        subst = subst.compose(&s2, ast);
                    }

                    let final_elem_ty = subst.apply(elem_ty, ast);

                    // we do not soport function types in lists
                    if final_elem_ty.is_function(ast) {
                        return Err(TypeError::InvalidListElement(final_elem_ty));
                    }

                    let list_ty = ast.types.alloc(Type::List(final_elem_ty));
                    (subst, list_ty)
                }
            }

            Expr::Record(fields) => {
                let mut subst = Subst::default();
                let mut field_types = Vec::new();

                for (name, field_expr) in fields {
                    let (s, field_ty) = self.infer(env, field_expr, ast, resolution)?;
                    subst = subst.compose(&s, ast);
                    let field_ty = subst.apply(field_ty, ast);

                    // we do not support function types in records' fields
                    if field_ty.is_function(ast) {
                        return Err(TypeError::InvalidRecordField(name, field_ty));
                    }

                    field_types.push((name, field_ty));
                }

                let record_ty = ast.types.alloc(Type::Record(field_types));

                (subst, record_ty)
            }

            Expr::Function { params, body } => {
                let mut param_types = Vec::new();
                let mut local_env = env.clone();

                for param in params {
                    let var = self.tvg.next();
                    let param_ty = ast.types.alloc(Type::Var(var));
                    local_env.insert(param, Polytype::mono(param_ty));
                    param_types.push(param_ty);
                }

                let (subst, ret) = self.infer(&local_env, body, ast, resolution)?;

                let params: Vec<_> = param_types
                    .into_iter()
                    .map(|ty| subst.apply(ty, ast))
                    .collect();

                let func_ty = ast.types.alloc(Type::Function(params, ret));

                (subst, func_ty)
            }

            Expr::Application { callee, args } => {
                let (mut subst, callee_ty) = self.infer(env, callee, ast, resolution)?;

                let mut params = Vec::new();
                for arg in args {
                    let (s, arg_ty) = self.infer(env, arg, ast, resolution)?;
                    subst = subst.compose(&s, ast);
                    let arg_ty = subst.apply(arg_ty, ast);
                    params.push(arg_ty);
                }

                let ret_var = self.tvg.next();
                let ret = ast.types.alloc(Type::Var(ret_var));

                let expected_func_ty = ast.types.alloc(Type::Function(params, ret));

                let callee_ty = subst.apply(callee_ty, ast);
                let s = self.unify(callee_ty, expected_func_ty, ast)?;
                subst = subst.compose(&s, ast);

                let final_ret_ty = subst.apply(ret, ast);
                (subst, final_ret_ty)
            }
        };

        let final_ty = subst.apply(ty, ast);
        self.expr_types.insert(expr_id, final_ty);

        Ok((subst, final_ty))
    }

    fn unify(&self, ty1: TypeId, ty2: TypeId, ast: &mut Ast) -> Result<Subst, TypeError> {
        if ty1 == ty2 {
            return Ok(Subst::default());
        }

        let t1 = ast.types.get(ty1).clone();
        let t2 = ast.types.get(ty2).clone();

        match (t1, t2) {
            (Type::Var(v), _) => v.bind(ty2, ast),
            (_, Type::Var(v)) => v.bind(ty1, ast),

            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(Subst::default()),

            (Type::List(inner1), Type::List(inner2)) => self.unify(inner1, inner2, ast),

            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: params1.len(),
                        found: params2.len(),
                    });
                }

                let mut subst = Subst::default();

                let params: Vec<_> = params1
                    .iter()
                    .zip(params2.iter())
                    .map(|(&a, &b)| (a, b))
                    .collect();

                for (param1, param2) in params {
                    let s = self.unify(param1, param2, ast)?;
                    subst = subst.compose(&s, ast);
                }

                let r1 = subst.apply(ret1, ast);
                let r2 = subst.apply(ret2, ast);
                let s = self.unify(r1, r2, ast)?;

                Ok(subst.compose(&s, ast))
            }

            (Type::Record(fields1), Type::Record(fields2)) => {
                if fields1.len() != fields2.len() {
                    return Err(TypeError::RecordSizeMismatch);
                }

                let mut subst = Subst::default();

                let field_pairs: Vec<_> = fields1
                    .iter()
                    .zip(fields2.iter())
                    .map(|((n1, t1), (n2, t2))| ((*n1, *t1), (*n2, *t2)))
                    .collect();

                for ((name1, ty1), (name2, ty2)) in field_pairs {
                    if name1 != name2 {
                        return Err(TypeError::RecordFieldMismatch);
                    }

                    let ty1 = subst.apply(ty1, ast);
                    let ty2 = subst.apply(ty2, ast);
                    let s = self.unify(ty1, ty2, ast)?;
                    subst = subst.compose(&s, ast);
                }

                Ok(subst)
            }

            _ => Err(TypeError::TypeMismatch {
                expected: ty1,
                found: ty2,
            }),
        }
    }

    fn get_function_signature(&mut self, binding_id: BindingId, ast: &mut Ast) -> Polytype {
        if let Some(poly) = self.function_types.get(&binding_id) {
            return poly.clone();
        }

        let poly = match self.registry.get(binding_id) {
            Binding::Function(func) => func.signature(ast, &mut self.tvg),
            _ => panic!("Expected function"),
        };

        self.function_types.insert(binding_id, poly.clone());
        poly
    }
}
