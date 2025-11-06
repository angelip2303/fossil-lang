use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use indexmap::IndexMap;

use crate::ast::*;
use crate::error::{CompileError, Result};
use crate::module::Lookup;
use crate::module::ModuleRegistry;

// ============================================================================
// TYPE VARIABLES
// ============================================================================

/// Variables de tipo con distinción semántica
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeVar {
    /// Variable fresca generada por inferencia ('t0, 't1, 't2, ...)
    Generated(usize),
    /// Variable genérica nombrada para polimorfismo ('a, 'b, 'c, ...)
    Named(&'static str),
}

impl TypeVar {
    pub fn fresh(id: usize) -> Self {
        Self::Generated(id)
    }

    pub fn generic(name: &'static str) -> Self {
        Self::Named(name)
    }
}

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVar::Generated(id) => write!(f, "'t{}", id),
            TypeVar::Named(name) => write!(f, "'{}", name),
        }
    }
}

// ============================================================================
// TYPE VAR GENERATOR
// ============================================================================

pub struct TypeVarGen {
    supply: usize,
}

impl TypeVarGen {
    pub fn new() -> Self {
        TypeVarGen { supply: 0 }
    }

    pub fn next(&mut self) -> usize {
        let var = self.supply;
        self.supply += 1;
        var
    }
}

// ============================================================================
// TYPES
// ============================================================================

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(TypeVar),
    Unit,
    Bool,
    Int,
    Float,
    String,
    List(Box<Type>),
    Func(Box<Type>, Box<Type>),
    Record(IndexMap<String, Box<Type>>),
}

impl Type {
    /// Retorna variables de tipo libres (Free Type Variables)
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Var(v) => {
                let mut set = HashSet::new();
                set.insert(*v);
                set
            }
            Type::Unit | Type::Bool | Type::Int | Type::Float | Type::String => HashSet::new(),
            Type::List(inner) => inner.free_vars(),
            Type::Func(from, to) => {
                let mut set = from.free_vars();
                set.extend(to.free_vars());
                set
            }
            Type::Record(fields) => fields.values().flat_map(|t| t.free_vars()).collect(),
        }
    }

    /// Aplica una sustitución de tipos
    pub fn substitute(&self, subst: &HashMap<TypeVar, Type>) -> Type {
        match self {
            Type::Var(v) => subst.get(v).cloned().unwrap_or_else(|| Type::Var(*v)),
            Type::Unit | Type::Bool | Type::Int | Type::Float | Type::String => self.clone(),
            Type::List(inner) => Type::List(Box::new(inner.substitute(subst))),
            Type::Func(from, to) => Type::Func(
                Box::new(from.substitute(subst)),
                Box::new(to.substitute(subst)),
            ),
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), Box::new(v.substitute(subst))))
                    .collect(),
            ),
        }
    }

    /// Unificación con threading de sustitución
    pub fn unify(&self, other: &Type, subst: &mut HashMap<TypeVar, Type>) -> Result<()> {
        let t1 = self.substitute(subst);
        let t2 = other.substitute(subst);

        match (&t1, &t2) {
            // Regla VAR-VAR
            (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),

            // Regla VAR-TERM (con occurs check)
            (Type::Var(v), t) | (t, Type::Var(v)) => {
                if t.free_vars().contains(v) {
                    return Err(CompileError::OccursCheckFailed);
                }
                subst.insert(*v, t.clone());
                Ok(())
            }

            // Regla FUNC
            (Type::Func(a1, r1), Type::Func(a2, r2)) => {
                a1.unify(a2, subst)?;
                r1.substitute(subst).unify(&r2.substitute(subst), subst)
            }

            // Regla LIST
            (Type::List(t1), Type::List(t2)) => t1.unify(t2, subst),

            // Regla RECORD
            (Type::Record(f1), Type::Record(f2)) => {
                if f1.len() != f2.len() {
                    return Err(CompileError::RecordSchemaMismatch);
                }

                for (k, v1) in f1 {
                    match f2.get(k) {
                        Some(v2) => v1.substitute(subst).unify(&v2.substitute(subst), subst)?,
                        None => return Err(CompileError::FieldNotFound(k.clone())),
                    }
                }
                Ok(())
            }

            // Reglas BASE
            (Type::Unit, Type::Unit)
            | (Type::Bool, Type::Bool)
            | (Type::Int, Type::Int)
            | (Type::Float, Type::Float)
            | (Type::String, Type::String) => Ok(()),

            // Regla FAIL
            _ => Err(CompileError::TypeMismatch {
                expected: format!("{}", self),
                found: format!("{}", other),
            }),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(v) => write!(f, "{}", v),
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::List(inner) => write!(f, "[{}]", inner),
            Type::Func(arg, ret) => match **arg {
                Type::Func(_, _) => write!(f, "({}) -> {}", arg, ret),
                _ => write!(f, "{} -> {}", arg, ret),
            },
            Type::Record(fields) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for (name, ty) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty)?;
                    first = false;
                }
                write!(f, " }}")
            }
        }
    }
}

// ============================================================================
// TYPE SCHEMES (Polimorfismo)
// ============================================================================

/// Esquema de tipo polimórfico: ∀α₁...αₙ. τ
#[derive(Clone, Debug)]
pub struct TypeScheme {
    /// Variables cuantificadas universalmente (∀)
    pub forall: Vec<TypeVar>,
    /// Tipo base
    pub ty: Type,
}

impl TypeScheme {
    /// Crea un esquema monomórfico (sin ∀)
    pub fn mono(ty: Type) -> Self {
        Self { forall: vec![], ty }
    }

    /// Instancia el esquema reemplazando variables cuantificadas con frescas
    pub fn instantiate(&self, tvg: &mut TypeVarGen) -> Type {
        if self.forall.is_empty() {
            return self.ty.clone();
        }

        let mut subst = HashMap::new();
        for var in &self.forall {
            subst.insert(*var, Type::Var(TypeVar::fresh(tvg.next())));
        }

        self.ty.substitute(&subst)
    }

    /// Generaliza un tipo (Algorithm W)
    /// ∀α. α ∈ ftv(τ) \ ftv(Γ)
    pub fn generalize(env_ftv: &HashSet<TypeVar>, ty: &Type) -> Self {
        let type_ftv = ty.free_vars();
        let forall: Vec<_> = type_ftv.difference(env_ftv).copied().collect();

        Self {
            forall,
            ty: ty.clone(),
        }
    }

    /// Variables libres del esquema (excluye las cuantificadas)
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        let bound: HashSet<_> = self.forall.iter().copied().collect();
        self.ty.free_vars().difference(&bound).copied().collect()
    }
}

impl std::fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.forall.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "∀")?;
            for (i, var) in self.forall.iter().enumerate() {
                if i > 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", var)?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

// ============================================================================
// TYPE ENVIRONMENT
// ============================================================================

#[derive(Clone, Default)]
pub struct TypeEnv(HashMap<NodeId, TypeScheme>);

impl TypeEnv {
    pub fn insert(&mut self, name: NodeId, scheme: TypeScheme) {
        self.0.insert(name, scheme);
    }

    pub fn get(&self, name: &NodeId) -> Option<&TypeScheme> {
        self.0.get(name)
    }

    /// Variables libres del entorno
    pub fn free_vars(&self) -> HashSet<TypeVar> {
        self.0
            .values()
            .flat_map(|scheme| scheme.free_vars())
            .collect()
    }
}

// ============================================================================
// GLOBALS & CONTEXT
// ============================================================================

#[derive(Clone)]
pub struct Globals {
    pub modules: ModuleRegistry,
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

    pub fn type_inference(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<Type> {
        let (subst, t) = self.infer_stmt(node_id, tvg)?;
        Ok(t.substitute(&subst))
    }

    fn infer_stmt(
        &mut self,
        node_id: NodeId,
        tvg: &mut TypeVarGen,
    ) -> Result<(HashMap<TypeVar, Type>, Type)> {
        let stmt = self.ast.get_stmt(node_id).kind.clone();

        match stmt {
            StmtKind::Type { name, ty } => {
                let infered_type = self.resolve_type(ty)?;

                // En caso de provider type, generar el módulo y registrarlo
                if let TypeKind::Provider(path, _) = self.ast.get_type(ty).kind.clone() {
                    let segments: Vec<&str> =
                        path.iter().map(|id| self.ast.get_name(*id)).collect();

                    let provider = match self.globals.modules.resolve(&segments) {
                        Some(Lookup::Provider(p)) => p,
                        _ => unreachable!(),
                    };

                    let name_str = self.ast.get_name(name);
                    let module = provider.generate_module(name_str, infered_type.clone())?;
                    self.globals.modules.register(module);
                }

                self.type_env
                    .insert(name, TypeScheme::mono(infered_type.clone()));

                Ok((HashMap::new(), infered_type))
            }

            StmtKind::Let { name, value } => {
                let (subst, t1) = self.infer_expr(value, tvg)?;

                // Generalizar el tipo
                let env_ftv = self.type_env.free_vars();
                let t1_normalized = t1.substitute(&subst);
                let scheme = TypeScheme::generalize(&env_ftv, &t1_normalized);

                self.type_env.insert(name, scheme);
                Ok((subst, t1))
            }

            StmtKind::Expr(expr) => self.infer_expr(expr, tvg),

            _ => Ok((HashMap::new(), Type::Unit)),
        }
    }

    pub fn infer_expr(
        &mut self,
        node_id: NodeId,
        tvg: &mut TypeVarGen,
    ) -> Result<(HashMap<TypeVar, Type>, Type)> {
        let kind = self.ast.get_expr(node_id).kind.clone();

        match kind {
            ExprKind::Unit => Ok((HashMap::new(), Type::Unit)),
            ExprKind::Integer(_) => Ok((HashMap::new(), Type::Int)),
            ExprKind::Boolean(_) => Ok((HashMap::new(), Type::Bool)),
            ExprKind::String(_) => Ok((HashMap::new(), Type::String)),

            ExprKind::Identifier(name) => match self.type_env.get(&name) {
                Some(scheme) => {
                    // Instanciar con variables frescas
                    let ty = scheme.instantiate(tvg);
                    Ok((HashMap::new(), ty))
                }
                None => {
                    let name_str = self.ast.get_name(name);
                    Err(CompileError::UnboundVariable(name_str.to_string()))
                }
            },

            ExprKind::Path(segments) => {
                let segments: Vec<&str> =
                    segments.iter().map(|id| self.ast.get_name(*id)).collect();

                match self.globals.modules.resolve(&segments) {
                    Some(Lookup::Function(f)) => {
                        // Obtener esquema e instanciar
                        let scheme = f.type_scheme();
                        let ty = scheme.instantiate(tvg);
                        Ok((HashMap::new(), ty))
                    }
                    _ => {
                        let path = segments.join(".");
                        Err(CompileError::UnboundVariable(path))
                    }
                }
            }

            ExprKind::List(elems) => {
                if elems.is_empty() {
                    let elem_ty = Type::Var(TypeVar::fresh(tvg.next()));
                    return Ok((HashMap::new(), Type::List(Box::new(elem_ty))));
                }

                let (mut subst, first_ty) = self.infer_expr(elems[0], tvg)?;

                for &elem in &elems[1..] {
                    let (s2, ty_i) = self.infer_expr(elem, tvg)?;
                    first_ty.substitute(&s2).unify(&ty_i, &mut subst)?;

                    // Componer sustituciones
                    for (k, v) in s2 {
                        subst.insert(k, v.substitute(&subst));
                    }
                }

                Ok((
                    subst.clone(),
                    Type::List(Box::new(first_ty.substitute(&subst))),
                ))
            }

            ExprKind::Record(fields) => {
                let mut subst = HashMap::new();
                let mut map = IndexMap::new();

                for (name_id, field_id) in fields {
                    let (s_field, t_field) = self.infer_expr(field_id, tvg)?;

                    // Componer sustituciones
                    for (k, v) in s_field {
                        subst.insert(k, v.substitute(&subst));
                    }

                    let name = self.ast.get_name(name_id).to_string();
                    map.insert(name, Box::new(t_field.substitute(&subst)));
                }

                Ok((subst, Type::Record(map)))
            }

            ExprKind::Function { param, body } => {
                let tv = match param.ty {
                    Some(ty_id) => self.resolve_type(ty_id)?,
                    None => Type::Var(TypeVar::fresh(tvg.next())),
                };

                let mut new_env = self.type_env.clone();
                new_env.insert(param.name, TypeScheme::mono(tv.clone()));

                let old_env = std::mem::replace(&mut self.type_env, new_env);
                let (s1, t1) = self.infer_expr(body, tvg)?;
                self.type_env = old_env;

                Ok((
                    s1.clone(),
                    Type::Func(Box::new(tv.substitute(&s1)), Box::new(t1)),
                ))
            }

            ExprKind::Call { callee, arg } => {
                let (s1, t1) = self.infer_expr(callee, tvg)?;
                let (mut s2, t2) = self.infer_expr(arg, tvg)?;

                // Componer s1 en s2
                for (k, v) in s1 {
                    s2.insert(k, v.substitute(&s2));
                }

                let tv = Type::Var(TypeVar::fresh(tvg.next()));
                t1.substitute(&s2)
                    .unify(&Type::Func(Box::new(t2), Box::new(tv.clone())), &mut s2)?;

                Ok((s2.clone(), tv.substitute(&s2)))
            }

            ExprKind::Pipe { left, right } => {
                let (s1, t1) = self.infer_expr(left, tvg)?;
                let (mut s2, t2) = self.infer_expr(right, tvg)?;

                // Componer s1 en s2
                for (k, v) in s1 {
                    s2.insert(k, v.substitute(&s2));
                }

                let tv = Type::Var(TypeVar::fresh(tvg.next()));
                t2.substitute(&s2)
                    .unify(&Type::Func(Box::new(t1), Box::new(tv.clone())), &mut s2)?;

                Ok((s2.clone(), tv.substitute(&s2)))
            }

            ExprKind::MemberAccess { object, field } => {
                let (s1, t1) = self.infer_expr(object, tvg)?;
                let fields = match t1.substitute(&s1) {
                    Type::Record(fields) => fields,
                    _ => {
                        return Err(CompileError::ExpectedRecord(format!("{}", t1)));
                    }
                };

                let field_name = self.ast.get_name(field);
                match fields.get(field_name) {
                    Some(field_type) => Ok((s1, *field_type.clone())),
                    None => Err(CompileError::FieldNotFound(field_name.to_string())),
                }
            }

            ExprKind::Cast { expr, ty } => {
                let (s1, _) = self.infer_expr(expr, tvg)?;
                let t2 = self.resolve_type(ty)?;
                Ok((s1, t2))
            }

            ExprKind::TypeAnnotation { expr, ty } => {
                let (mut s1, t1) = self.infer_expr(expr, tvg)?;
                let annotated_ty = self.resolve_type(ty)?;
                t1.unify(&annotated_ty, &mut s1)?;
                Ok((s1.clone(), annotated_ty.substitute(&s1)))
            }

            _ => Ok((HashMap::new(), Type::Unit)),
        }
    }

    pub fn resolve_type(&mut self, type_id: NodeId) -> Result<Type> {
        let ty = self.ast.get_type(type_id).kind.clone();

        match ty {
            TypeKind::Unit => Ok(Type::Unit),
            TypeKind::Int => Ok(Type::Int),
            TypeKind::Bool => Ok(Type::Bool),
            TypeKind::String => Ok(Type::String),

            TypeKind::List(inner_id) => {
                let inner = self.resolve_type(inner_id)?;
                Ok(Type::List(Box::new(inner)))
            }

            TypeKind::Function { from, to } => {
                let from_ty = self.resolve_type(from)?;
                let to_ty = self.resolve_type(to)?;
                Ok(Type::Func(Box::new(from_ty), Box::new(to_ty)))
            }

            TypeKind::Record(field_pairs) => {
                let mut fields = IndexMap::new();
                for (name_id, type_id) in field_pairs {
                    let name = self.ast.get_name(name_id).to_string();
                    let ty = self.resolve_type(type_id)?;
                    fields.insert(name, Box::new(ty));
                }
                Ok(Type::Record(fields))
            }

            TypeKind::Var(path) => match self.type_env.get(&path[0]) {
                Some(scheme) => Ok(scheme.ty.clone()),
                None => {
                    let name = self.ast.get_name(path[0]);
                    Err(CompileError::UnboundVariable(name.to_string()))
                }
            },

            TypeKind::Provider(provider_path, args) => {
                let segments: Vec<&str> = provider_path
                    .iter()
                    .map(|id| self.ast.get_name(*id))
                    .collect();

                let provider = match self.globals.modules.resolve(&segments) {
                    Some(Lookup::Provider(p)) => p,
                    _ => return Err(CompileError::ProviderNotFound(segments.join("."))),
                };

                let expected = provider.param_types();
                if args.len() != expected.len() {
                    return Err(CompileError::InvalidArgumentCount {
                        expected: expected.len(),
                        actual: args.len(),
                    });
                }

                for (arg, exp_ty) in args.iter().zip(expected.iter()) {
                    let (_, arg_ty) = self.infer_expr(arg.value, &mut TypeVarGen::new())?;
                    let mut subst = HashMap::new();
                    arg_ty.unify(exp_ty, &mut subst)?;
                }

                provider.provide_type(&self.ast, &args)
            }
        }
    }
}
