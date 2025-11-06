use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use indexmap::IndexMap;

use crate::ast::*;
use crate::error::{CompileError, Result};
use crate::module::Lookup;
use crate::module::ModuleRegistry;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl TypeVar {
    fn bind(&self, ty: &Type) -> Result<Subst> {
        if let Type::Var(var) = ty {
            if var == self {
                return Ok(Subst::new());
            }
        }

        if ty.ftv().contains(self) {
            return Err(CompileError::OccursCheckFailed);
        }

        let mut s = Subst::new();
        s.insert(*self, ty.clone());
        Ok(s)
    }
}

impl std::fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'t{}", self.0)
    }
}

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

trait Typed {
    fn ftv(&self) -> HashSet<TypeVar>;
    fn apply(&self, subst: &Subst) -> Self;
}

impl<T: Typed> Typed for Vec<T> {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter().flat_map(|t| t.ftv()).collect()
    }

    fn apply(&self, subst: &Subst) -> Self {
        self.iter().map(|t| t.apply(subst)).collect()
    }
}

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
    pub fn mgu(&self, other: &Type) -> Result<Subst> {
        match (self, other) {
            (Type::Func(arg1, ret1), Type::Func(arg2, ret2)) => {
                let s1 = arg1.mgu(arg2)?;
                let s2 = ret1.apply(&s1).mgu(&ret2.apply(&s1))?;
                Ok(s1.compose(&s2))
            }

            (Type::List(a), Type::List(b)) => a.mgu(b),

            (Type::Var(var), t) | (t, Type::Var(var)) => var.bind(t),

            (Type::Unit, Type::Unit)
            | (Type::Bool, Type::Bool)
            | (Type::Int, Type::Int)
            | (Type::Float, Type::Float)
            | (Type::String, Type::String) => Ok(Subst::new()),

            (Type::Record(fields1), Type::Record(fields2)) => {
                if fields1.len() != fields2.len() {
                    return Err(CompileError::RecordSchemaMismatch);
                }

                let keys1: HashSet<_> = fields1.keys().collect();
                let keys2: HashSet<_> = fields2.keys().collect();

                if keys1 != keys2 {
                    return Err(CompileError::RecordSchemaMismatch);
                }

                let mut subst = Subst::new();
                for (name, t1) in fields1 {
                    let t2 = &fields2[name];
                    let s = t1.apply(&subst).mgu(&t2.apply(&subst))?;
                    subst = subst.compose(&s);
                }

                Ok(subst)
            }

            _ => Err(CompileError::TypeMismatch {
                expected: format!("{}", self),
                found: format!("{}", other),
            }),
        }
    }
}

impl Typed for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            Type::Var(var) => {
                let mut set = HashSet::new();
                set.insert(*var);
                set
            }
            Type::Unit | Type::Bool | Type::Int | Type::Float | Type::String => HashSet::new(),
            Type::List(inner) => inner.ftv(),
            Type::Func(arg, ret) => arg.ftv().union(&ret.ftv()).copied().collect(),
            Type::Record(fields) => fields.values().flat_map(|t| t.ftv()).collect(),
        }
    }

    fn apply(&self, subst: &Subst) -> Type {
        match self {
            Type::Var(var) => subst.get(var).cloned().unwrap_or(Type::Var(*var)),
            Type::Unit | Type::Bool | Type::Int | Type::Float | Type::String => self.clone(),
            Type::List(inner) => Type::List(Box::new(inner.apply(subst))),
            Type::Func(arg, ret) => {
                Type::Func(Box::new(arg.apply(subst)), Box::new(ret.apply(subst)))
            }
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), Box::new(v.apply(subst))))
                    .collect(),
            ),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(var) => write!(f, "{}", var),
            Type::Unit => write!(f, "()"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::List(inner) => write!(f, "[{}]", inner),
            Type::Func(arg, ret) => write!(f, "({} -> {})", arg, ret),
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

#[derive(Clone, Debug)]
pub struct PolyType {
    pub vars: Vec<TypeVar>,
    pub ty: Type,
}

impl Typed for PolyType {
    fn ftv(&self) -> HashSet<TypeVar> {
        let bound: HashSet<_> = self.vars.iter().copied().collect();
        self.ty.ftv().difference(&bound).copied().collect()
    }

    fn apply(&self, subst: &Subst) -> PolyType {
        let mut filtered_subst = subst.clone();
        for var in &self.vars {
            filtered_subst.remove(var);
        }

        PolyType {
            vars: self.vars.clone(),
            ty: self.ty.apply(&filtered_subst),
        }
    }
}

impl PolyType {
    fn instantiate(&self, tvg: &mut TypeVarGen) -> Type {
        let new_vars: HashMap<_, _> = self
            .vars
            .iter()
            .map(|v| (*v, Type::Var(tvg.next())))
            .collect();

        let subst = Subst(new_vars);
        self.ty.apply(&subst)
    }
}

#[derive(Clone, Debug)]
pub struct Subst(HashMap<TypeVar, Type>);

impl Subst {
    fn new() -> Self {
        Subst(HashMap::new())
    }

    fn insert(&mut self, var: TypeVar, ty: Type) {
        self.0.insert(var, ty);
    }

    fn get(&self, var: &TypeVar) -> Option<&Type> {
        self.0.get(var)
    }

    fn remove(&mut self, var: &TypeVar) {
        self.0.remove(var);
    }

    fn compose(&self, other: &Subst) -> Subst {
        let mut result = Subst::new();

        for (var, ty) in &other.0 {
            result.insert(*var, ty.apply(self));
        }

        for (var, ty) in &self.0 {
            if !result.0.contains_key(var) {
                result.insert(*var, ty.clone());
            }
        }

        result
    }
}

#[derive(Clone, Default)]
pub struct TypeEnv(HashMap<NodeId, PolyType>);

impl Typed for TypeEnv {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.0.values().flat_map(|pt| pt.ftv()).collect()
    }

    fn apply(&self, subst: &Subst) -> TypeEnv {
        TypeEnv(self.0.iter().map(|(k, v)| (*k, v.apply(subst))).collect())
    }
}

impl TypeEnv {
    fn insert(&mut self, name: NodeId, poly: PolyType) {
        self.0.insert(name, poly);
    }

    fn get(&self, name: &NodeId) -> Option<&PolyType> {
        self.0.get(name)
    }

    fn generalize(&self, ty: &Type) -> PolyType {
        let vars: Vec<_> = ty.ftv().difference(&self.ftv()).copied().collect();

        PolyType {
            vars,
            ty: ty.clone(),
        }
    }
}

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
        let (s, t) = self.infer_stmt(node_id, tvg)?;
        Ok(t.apply(&s))
    }

    fn infer_stmt(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        let stmt = self.ast.get_stmt(node_id).kind.clone();

        match stmt {
            StmtKind::Type { name, ty } => {
                let infered_type = self.resolve_type(ty)?;

                // in case of a provider type, generate the module and register it
                // TODO: maybe there is a better way to handle this
                if let TypeKind::Provider(path, _) = self.ast.get_type(ty).kind.clone() {
                    let segments: Vec<&str> =
                        path.iter().map(|id| self.ast.get_name(*id)).collect();

                    let provider = match self.globals.modules.resolve(&segments) {
                        Some(Lookup::Provider(p)) => p,
                        _ => unreachable!(),
                    };

                    let name = self.ast.get_name(name);
                    let module = provider.generate_module(name, infered_type.clone())?;
                    self.globals.modules.register(module);
                }

                self.type_env.insert(
                    name,
                    PolyType {
                        vars: vec![],
                        ty: infered_type.clone(),
                    },
                );

                Ok((Subst::new(), infered_type))
            }

            StmtKind::Let { name, value } => {
                let (s1, t1) = self.infer_expr(value, tvg)?;
                let env1 = self.type_env.apply(&s1);
                let poly = env1.generalize(&t1);
                self.type_env = env1;
                self.type_env.insert(name, poly);
                Ok((s1, t1))
            }

            StmtKind::Expr(expr) => self.infer_expr(expr, tvg),

            _ => Ok((Subst::new(), Type::Unit)),
        }
    }

    pub fn infer_expr(&mut self, node_id: NodeId, tvg: &mut TypeVarGen) -> Result<(Subst, Type)> {
        let kind = self.ast.get_expr(node_id).kind.clone();

        match kind {
            ExprKind::Unit => Ok((Subst::new(), Type::Unit)),
            ExprKind::Integer(_) => Ok((Subst::new(), Type::Int)),
            ExprKind::Boolean(_) => Ok((Subst::new(), Type::Bool)),
            ExprKind::String(_) => Ok((Subst::new(), Type::String)),

            ExprKind::Identifier(name) => match self.type_env.get(&name) {
                Some(poly) => Ok((Subst::new(), poly.instantiate(tvg))),
                None => {
                    let name_str = self.ast.get_name(name);
                    Err(CompileError::UnboundVariable(name_str.to_string()))
                }
            },

            ExprKind::Path(segments) => {
                let segments: Vec<&str> =
                    segments.iter().map(|id| self.ast.get_name(*id)).collect();

                let ty = match self.globals.modules.resolve(&segments) {
                    Some(Lookup::Function(f)) => f.ty(),
                    _ => {
                        let path = segments.join(".");
                        return Err(CompileError::UnboundVariable(path));
                    }
                };

                Ok((Subst::new(), ty))
            }

            ExprKind::List(elems) => {
                if elems.is_empty() {
                    let elem_ty = Type::Var(tvg.next());
                    return Ok((Subst::new(), Type::List(Box::new(elem_ty))));
                }

                let (mut s, first_ty) = self.infer_expr(elems[0], tvg)?;

                for &elem in &elems[1..] {
                    self.type_env = self.type_env.apply(&s);
                    let (s2, ty_i) = self.infer_expr(elem, tvg)?;
                    let s3 = first_ty.apply(&s2).mgu(&ty_i)?;
                    s = s3.compose(&s2.compose(&s));
                }

                Ok((s.clone(), Type::List(Box::new(first_ty.apply(&s)))))
            }

            ExprKind::Record(fields) => {
                let mut subst = Subst::new();
                let mut map = IndexMap::new();

                for (name_id, field_id) in fields {
                    self.type_env = self.type_env.apply(&subst);
                    let (s_field, t_field) = self.infer_expr(field_id, tvg)?;
                    subst = subst.compose(&s_field);
                    let name = self.ast.get_name(name_id).to_string();
                    map.insert(name, Box::new(t_field));
                }

                Ok((subst, Type::Record(map)))
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
                let (s1, t1) = self.infer_expr(body, tvg)?;
                self.type_env = old_env;

                Ok((
                    s1.clone(),
                    Type::Func(Box::new(tv.apply(&s1)), Box::new(t1)),
                ))
            }

            ExprKind::Call { callee, arg } => {
                let (s1, t1) = self.infer_expr(callee, tvg)?;
                self.type_env = self.type_env.apply(&s1);
                let (s2, t2) = self.infer_expr(arg, tvg)?;
                let tv = Type::Var(tvg.next());
                let s3 = t1
                    .apply(&s2)
                    .mgu(&Type::Func(Box::new(t2), Box::new(tv.clone())))?;
                Ok((s3.compose(&s2.compose(&s1)), tv.apply(&s3)))
            }

            ExprKind::Pipe { left, right } => {
                let (s1, t1) = self.infer_expr(left, tvg)?;
                self.type_env = self.type_env.apply(&s1);
                let (s2, t2) = self.infer_expr(right, tvg)?;
                let tv = Type::Var(tvg.next());
                let s3 = t2
                    .apply(&s2)
                    .mgu(&Type::Func(Box::new(t1), Box::new(tv.clone())))?;
                Ok((s3.compose(&s2.compose(&s1)), tv.apply(&s3)))
            }

            ExprKind::MemberAccess { object, field } => {
                let (s1, t1) = self.infer_expr(object, tvg)?;
                let fields = match t1 {
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
                let (s1, t1) = self.infer_expr(expr, tvg)?;
                let annotated_ty = self.resolve_type(ty)?;
                let s2 = t1.mgu(&annotated_ty)?;
                Ok((s2.compose(&s1), annotated_ty.apply(&s2)))
            }

            _ => Ok((Subst::new(), Type::Unit)),
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
                Some(poly) => Ok(poly.ty.clone()),
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

                // TODO: is this the best way to check arguments? I think we could rely on signature and work as a Function
                let expected = provider.param_types();
                if args.len() != expected.len() {
                    return Err(CompileError::InvalidArgumentCount {
                        expected: expected.len(),
                        actual: args.len(),
                    });
                }

                for (arg, exp_ty) in args.iter().zip(expected.iter()) {
                    let (_, arg_ty) = self.infer_expr(arg.value, &mut TypeVarGen::new())?;
                    arg_ty.mgu(exp_ty)?;
                }

                provider.provide_type(&self.ast, &args)
            }
        }
    }
}
