// typecheck.rs - Versión mejorada
use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::context::Symbol;
use crate::error::TypeError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::{BindingRef, ResolutionTable, ResolvedProgram, TypedProgram};

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

/// A substitution is a mapping `σ: V -> T` from variables to types
#[derive(Clone, Debug, Default)]
pub struct Subst(HashMap<TypeVar, TypeId>);

impl Subst {
    pub fn singleton(var: TypeVar, ty: TypeId) -> Self {
        Self([(var, ty)].into_iter().collect())
    }

    pub fn new(vars: impl IntoIterator<Item = (TypeVar, TypeId)>) -> Self {
        Self(vars.into_iter().collect())
    }

    pub fn get(&self, var: &TypeVar) -> Option<TypeId> {
        self.0.get(var).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&TypeVar, &TypeId)> {
        self.0.iter()
    }

    /// Apply substitution to a type
    /// Returns the same TypeId if no substitution needed
    pub fn apply(&self, ty_id: TypeId, ast: &Ast) -> TypeId {
        match ast.types.get(ty_id) {
            Type::Var(var) => self.get(var).unwrap_or(ty_id),
            _ => ty_id, // Tipos estructurales no se sustituyen directamente
        }
    }

    /// Compose two substitutions: self ∘ other
    pub fn compose(&self, other: &Subst, ast: &Ast) -> Subst {
        let mut composed = HashMap::new();

        // Apply self to other's mappings
        for (var, ty) in other.iter() {
            composed.insert(*var, self.apply(*ty, ast));
        }

        // Add self's mappings that aren't in other
        for (var, ty) in self.iter() {
            composed.entry(*var).or_insert(*ty);
        }

        Subst(composed)
    }
}

/// Type environment: Γ
#[derive(Clone, Debug, Default)]
pub struct TypeEnv(HashMap<Symbol, Polytype>);

impl TypeEnv {
    fn get(&self, name: &Symbol) -> Option<&Polytype> {
        self.0.get(name)
    }

    fn insert(&mut self, name: Symbol, poly: Polytype) {
        self.0.insert(name, poly);
    }

    fn values(&self) -> impl Iterator<Item = &Polytype> {
        self.0.values()
    }

    /// Free type variables in the environment
    fn ftv(&self, ast: &Ast) -> HashSet<TypeVar> {
        let mut ans = HashSet::default();
        for poly in self.values() {
            let ftv = ftv_type(poly.ty, ast);
            let bound: HashSet<_> = poly.forall.iter().copied().collect();
            ans.extend(ftv.difference(&bound));
        }
        ans
    }

    /// Generalize: ∀(ftv(τ) \ ftv(Γ)). τ
    fn generalize(&self, ty: TypeId, ast: &Ast) -> Polytype {
        let env_ftv = self.ftv(ast);
        let ty_ftv = ftv_type(ty, ast);
        let mut forall: Vec<TypeVar> = ty_ftv.difference(&env_ftv).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype { forall, ty }
    }
}

/// Free type variables in a type (flat computation)
fn ftv_type(ty_id: TypeId, ast: &Ast) -> HashSet<TypeVar> {
    let mut result = HashSet::new();
    let mut stack = vec![ty_id];

    while let Some(current) = stack.pop() {
        match ast.types.get(current) {
            Type::Var(var) => {
                result.insert(*var);
            }
            Type::List(inner) => {
                stack.push(*inner);
            }
            Type::Record(fields) => {
                for (_, field_ty) in fields {
                    stack.push(*field_ty);
                }
            }
            Type::Function { params, ret } => {
                stack.push(*ret);
                for param in params {
                    stack.push(*param);
                }
            }
            Type::Primitive(_) => {}
            Type::Named(_) | Type::Provider { .. } => {
                panic!(
                    "Unresolved type in type checking phase - TypeGen should have resolved these"
                )
            }
        }
    }

    result
}

#[derive(Debug, Clone)]
struct Constraint {
    expected: TypeId,
    actual: TypeId,
}

pub struct TypeChecker<'a> {
    registry: &'a ModuleRegistry,
    env: TypeEnv,
    tvg: TypeVarGen,
    expr_types: HashMap<ExprId, TypeId>,
    constraints: Vec<Constraint>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self {
            registry,
            env: TypeEnv::default(),
            tvg: TypeVarGen::default(),
            expr_types: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    /// Type check a resolved program
    /// AST is NOT mutated - only type annotations are produced
    pub fn check(mut self, program: ResolvedProgram) -> Result<TypedProgram, TypeError> {
        let ResolvedProgram {
            mut ast,
            symbols,
            resolution,
        } = program;

        // PASADA 1: Asignar types a todas las expresiones (flat loop)
        self.assign_types(&mut ast, &resolution)?;

        // PASADA 2: Resolver constraints
        let subst = self.solve_constraints(&ast)?;

        // PASADA 3: Aplicar substitution final
        let final_types = self.apply_final_subst(&subst, &ast);

        Ok(TypedProgram {
            ast,
            symbols,
            resolution,
            expr_types: final_types,
        })
    }

    // =========================================
    // PASADA 1: ASSIGN TYPES (Flat loop)
    // =========================================

    fn assign_types(
        &mut self,
        ast: &mut Ast,
        resolution: &ResolutionTable,
    ) -> Result<(), TypeError> {
        let decl_ids: Vec<_> = ast.decls.iter().map(|(id, _)| id).collect();
        for decl_id in &decl_ids {
            let decl = ast.decls.get(*decl_id);
            if let Decl::Type { name, ty } = decl {
                self.env.insert(*name, Polytype::mono(*ty));
            }
        }

        let exprs: Vec<_> = ast.exprs.iter().map(|(id, _)| id).collect();
        for expr in exprs {
            let ty = self.assign_expr_type(expr, ast, resolution)?;
            self.expr_types.insert(expr, ty);
        }

        for decl_id in &decl_ids {
            let decl = ast.decls.get(*decl_id);
            if let Decl::Let { name, value } = decl {
                let inferred_ty = self.expr_types[value];
                self.env.insert(*name, Polytype::mono(inferred_ty));
            }
        }

        Ok(())
    }

    fn assign_expr_type(
        &mut self,
        expr_id: ExprId,
        ast: &mut Ast,
        resolution: &ResolutionTable,
    ) -> Result<TypeId, TypeError> {
        let expr = ast.exprs.get(expr_id);

        let ty = match expr {
            Expr::Identifier(_) => {
                let binding = resolution
                    .exprs
                    .get(&expr_id)
                    .ok_or(TypeError::InternalError)?;

                match binding {
                    BindingRef::Local(id) => {
                        let decl = ast.decls.get(*id);
                        match decl {
                            Decl::Let { name, .. } => {
                                if let Some(poly) = self.env.get(name).cloned() {
                                    self.instantiate(&poly, ast)
                                } else {
                                    let var = self.tvg.next();
                                    ast.types.alloc(Type::Var(var))
                                }
                            }

                            _ => return Err(TypeError::InvalidBinding),
                        }
                    }

                    BindingRef::Module(binding) => match self.registry.get(*binding) {
                        Binding::Function(func) => {
                            let signature = func.signature();
                            self.instantiate(&signature, ast)
                        }

                        _ => return Err(TypeError::InvalidBinding),
                    },
                }
            }

            Expr::Literal(lit) => {
                let ty = match lit {
                    Literal::Boolean(_) => Type::Primitive(PrimitiveType::Bool),
                    Literal::Integer(_) => Type::Primitive(PrimitiveType::Int),
                    Literal::String(_) => Type::Primitive(PrimitiveType::String),
                };
                ast.types.alloc(ty)
            }

            Expr::List(items) => {
                if items.is_empty() {
                    let var = self.tvg.next();
                    let inner = ast.types.alloc(Type::Var(var));
                    ast.types.alloc(Type::List(inner))
                } else {
                    let elem_var = self.tvg.next();
                    let elem_ty = ast.types.alloc(Type::Var(elem_var));

                    for item in items {
                        self.constraints.push(Constraint {
                            expected: elem_ty,
                            actual: self.retrieve(item)?,
                        });
                    }

                    ast.types.alloc(Type::List(elem_ty))
                }
            }

            Expr::Record(fields) => {
                let mut field_types = Vec::new();
                for (name, field_expr) in fields {
                    let ty = self.retrieve(field_expr)?;
                    field_types.push((*name, ty));
                }
                ast.types.alloc(Type::Record(field_types))
            }

            Expr::Function { params, body } => {
                let mut param_types = Vec::new();
                for param in params {
                    let var = self.tvg.next();
                    let ty = ast.types.alloc(Type::Var(var));
                    self.env.insert(*param, Polytype::mono(ty));
                    param_types.push(ty);
                }

                let ret_ty = self.retrieve(body)?;

                ast.types.alloc(Type::Function {
                    params: param_types,
                    ret: ret_ty,
                })
            }

            Expr::Application { callee, args } => {
                let callee_ty = self.retrieve(callee)?;

                let mut arg_types = Vec::new();
                for arg in args {
                    arg_types.push(self.retrieve(arg)?);
                }

                let ret_var = self.tvg.next();
                let ret_ty = ast.types.alloc(Type::Var(ret_var));

                let expected_func = ast.types.alloc(Type::Function {
                    params: arg_types,
                    ret: ret_ty,
                });

                self.constraints.push(Constraint {
                    expected: expected_func,
                    actual: callee_ty,
                });

                ret_ty
            }
        };

        Ok(ty)
    }

    // =========================================
    // PASADA 2: SOLVE CONSTRAINTS
    // =========================================

    fn solve_constraints(&mut self, ast: &Ast) -> Result<Subst, TypeError> {
        let mut subst = Subst::default();

        for constraint in &self.constraints {
            let expected = subst.apply(constraint.expected, ast);
            let actual = subst.apply(constraint.actual, ast);

            let unifier = self.unify(expected, actual, ast)?;
            subst = subst.compose(&unifier, ast);
        }

        Ok(subst)
    }

    /// Most General Unifier (MGU)
    /// Encuentra la substitución más general que unifica dos tipos
    fn unify(&self, ty1: TypeId, ty2: TypeId, ast: &Ast) -> Result<Subst, TypeError> {
        if ty1 == ty2 {
            return Ok(Subst::default());
        }

        let t1 = ast.types.get(ty1);
        let t2 = ast.types.get(ty2);

        match (t1, t2) {
            // Unificación de variables
            (Type::Var(v), _) => self.bind(*v, ty2, ast),
            (_, Type::Var(v)) => self.bind(*v, ty1, ast),

            // Primitivos
            (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(Subst::default()),

            // Listas
            (Type::List(inner1), Type::List(inner2)) => self.unify(*inner1, *inner2, ast),

            // Funciones
            (
                Type::Function {
                    params: p1,
                    ret: r1,
                },
                Type::Function {
                    params: p2,
                    ret: r2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: p1.len(),
                        found: p2.len(),
                    });
                }

                let mut subst = Subst::default();

                // Unifica parámetros
                for (param1, param2) in p1.iter().zip(p2.iter()) {
                    let param1 = subst.apply(*param1, ast);
                    let param2 = subst.apply(*param2, ast);
                    let s = self.unify(param1, param2, ast)?;
                    subst = subst.compose(&s, ast);
                }

                // Unifica returns
                let r1 = subst.apply(*r1, ast);
                let r2 = subst.apply(*r2, ast);
                let s = self.unify(r1, r2, ast)?;
                Ok(subst.compose(&s, ast))
            }

            // Records
            (Type::Record(fields1), Type::Record(fields2)) => {
                if fields1.len() != fields2.len() {
                    return Err(TypeError::RecordSizeMismatch);
                }

                let mut subst = Subst::default();

                for ((name1, ty1), (name2, ty2)) in fields1.iter().zip(fields2.iter()) {
                    if name1 != name2 {
                        return Err(TypeError::RecordFieldMismatch);
                    }

                    let ty1 = subst.apply(*ty1, ast);
                    let ty2 = subst.apply(*ty2, ast);
                    let s = self.unify(ty1, ty2, ast)?;
                    subst = subst.compose(&s, ast);
                }

                Ok(subst)
            }

            // Tipos no resueltos (deberían haber sido resueltos por TypeGen)
            (Type::Named(_), _) | (_, Type::Named(_)) => {
                panic!("Named type should have been resolved by TypeGen")
            }

            (Type::Provider { .. }, _) | (_, Type::Provider { .. }) => {
                panic!("Provider type should have been generated by TypeGen")
            }

            // Tipos incompatibles
            _ => Err(TypeError::TypeMismatch {
                expected: ty1,
                found: ty2,
            }),
        }
    }

    fn bind(&self, var: TypeVar, ty: TypeId, ast: &Ast) -> Result<Subst, TypeError> {
        if let Type::Var(v) = ast.types.get(ty) {
            if *v == var {
                return Ok(Subst::default());
            }
        }

        if ftv_type(ty, ast).contains(&var) {
            return Err(TypeError::InfiniteType(var));
        }

        Ok(Subst::singleton(var, ty))
    }

    fn apply_final_subst(&self, subst: &Subst, ast: &Ast) -> HashMap<ExprId, TypeId> {
        self.expr_types
            .iter()
            .map(|(expr_id, ty)| (*expr_id, subst.apply(*ty, ast)))
            .collect()
    }

    /// Instantiate a polytype (replaces bound variables with fresh ones)
    fn instantiate(&mut self, poly: &Polytype, ast: &mut Ast) -> TypeId {
        if poly.forall.is_empty() {
            return poly.ty;
        }

        // Crea variables frescas para cada variable ligada
        let fresh_vars: HashMap<TypeVar, TypeId> = poly
            .forall
            .iter()
            .map(|old_var| {
                let fresh = self.tvg.next();
                (*old_var, ast.types.alloc(Type::Var(fresh)))
            })
            .collect();

        let subst = Subst(fresh_vars);

        // Aplica la substitución al tipo
        subst.apply(poly.ty, ast)
    }

    fn retrieve(&self, expr_id: &ExprId) -> Result<TypeId, TypeError> {
        self.expr_types
            .get(expr_id)
            .copied()
            .ok_or(TypeError::InternalError)
    }
}
