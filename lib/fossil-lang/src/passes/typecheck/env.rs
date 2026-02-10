//! Type environment for let-polymorphism
//!
//! The type environment tracks bindings from DefIds to polytypes,
//! supporting generalization and instantiation for let-polymorphism.

use std::collections::{HashMap, HashSet};

use crate::context::DefId;
use crate::ir::{Ir, Polytype, RecordFields, TypeId, TypeKind, TypeVar};

/// Type environment: maps DefIds to polytypes
#[derive(Clone, Debug, Default)]
pub struct TypeEnv {
    bindings: HashMap<DefId, Polytype>,
}

impl TypeEnv {
    pub fn insert(&mut self, def_id: DefId, poly: Polytype) {
        self.bindings.insert(def_id, poly);
    }

    pub fn lookup(&self, def_id: DefId) -> Option<&Polytype> {
        self.bindings.get(&def_id)
    }

    /// Get free type variables in the environment
    pub fn free_type_vars(&self, ir: &Ir) -> HashSet<TypeVar> {
        self.bindings
            .values()
            .flat_map(|poly| self.free_vars_polytype(poly, ir))
            .collect()
    }

    fn free_vars_polytype(&self, poly: &Polytype, ir: &Ir) -> HashSet<TypeVar> {
        let ty_vars = self.free_vars_type(poly.ty, ir);
        let bound: HashSet<_> = poly.forall.iter().copied().collect();
        ty_vars.difference(&bound).copied().collect()
    }

    pub fn free_vars_type(&self, ty_id: TypeId, ir: &Ir) -> HashSet<TypeVar> {
        let ty = ir.types.get(ty_id);
        match &ty.kind {
            TypeKind::Var(var) => {
                let mut set = HashSet::new();
                set.insert(*var);
                set
            }
            TypeKind::App { args, .. } => args
                .iter()
                .flat_map(|arg| self.free_vars_type(*arg, ir))
                .collect(),
            TypeKind::Record(fields) => self.free_vars_fields(fields, ir),
            TypeKind::Function(params, ret) => {
                let mut vars: HashSet<_> = params
                    .iter()
                    .flat_map(|param| self.free_vars_type(*param, ir))
                    .collect();
                vars.extend(self.free_vars_type(*ret, ir));
                vars
            }
            TypeKind::List(inner) => self.free_vars_type(*inner, ir),
            TypeKind::Optional(inner) => self.free_vars_type(*inner, ir),
            TypeKind::Primitive(_)
            | TypeKind::Named(_)
            | TypeKind::Unit
            | TypeKind::Provider { .. } => HashSet::new(),
        }
    }

    pub fn free_vars_fields(&self, fields: &RecordFields, ir: &Ir) -> HashSet<TypeVar> {
        fields
            .fields
            .iter()
            .flat_map(|(_, ty)| self.free_vars_type(*ty, ir))
            .collect()
    }

    /// Generalize a type to a polytype
    pub fn generalize(&self, ty_id: TypeId, ir: &Ir) -> Polytype {
        let env_vars = self.free_type_vars(ir);
        let ty_vars = self.free_vars_type(ty_id, ir);
        let mut forall: Vec<_> = ty_vars.difference(&env_vars).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype { forall, ty: ty_id }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::DefId;
    use crate::ir::{Ir, Polytype, TypeVar};

    #[test]
    fn insert_and_lookup() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let def_id = DefId::new(0);
        let poly = Polytype::mono(int_ty);

        let mut env = TypeEnv::default();
        env.insert(def_id, poly);

        let result = env.lookup(def_id);
        assert!(result.is_some());
        assert_eq!(result.unwrap().ty, int_ty);
        assert!(result.unwrap().forall.is_empty());
    }

    #[test]
    fn lookup_missing() {
        let env = TypeEnv::default();
        let def_id = DefId::new(0);

        let result = env.lookup(def_id);
        assert!(result.is_none());
    }

    #[test]
    fn generalize_no_free_vars() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();

        let env = TypeEnv::default();
        let poly = env.generalize(int_ty, &ir);

        assert!(poly.forall.is_empty());
        assert_eq!(poly.ty, int_ty);
    }

    #[test]
    fn generalize_with_free_vars() {
        let mut ir = Ir::default();
        let var0 = TypeVar(0);
        let var_ty = ir.var_type(var0);

        // Empty env has no Var(0), so Var(0) is free and should be generalized
        let env = TypeEnv::default();
        let poly = env.generalize(var_ty, &ir);

        assert_eq!(poly.forall, vec![var0]);
        assert_eq!(poly.ty, var_ty);
    }

    #[test]
    fn generalize_env_constrains() {
        let mut ir = Ir::default();
        let var0 = TypeVar(0);
        let var_ty = ir.var_type(var0);
        let def_id = DefId::new(0);

        // Insert a binding that uses Var(0) into the env (as a monotype).
        // This makes Var(0) free in the env, so generalizing another type
        // that contains Var(0) should NOT quantify it.
        let mut env = TypeEnv::default();
        env.insert(def_id, Polytype::mono(var_ty));

        // Generalize the same Var(0) type -- Var(0) is in the env,
        // so it should NOT appear in forall.
        let poly = env.generalize(var_ty, &ir);

        assert!(poly.forall.is_empty());
        assert_eq!(poly.ty, var_ty);
    }

    #[test]
    fn free_vars_primitive() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();

        let env = TypeEnv::default();
        let free = env.free_vars_type(int_ty, &ir);

        assert!(free.is_empty());
    }

    #[test]
    fn free_vars_type_var() {
        let mut ir = Ir::default();
        let var0 = TypeVar(0);
        let var_ty = ir.var_type(var0);

        let env = TypeEnv::default();
        let free = env.free_vars_type(var_ty, &ir);

        assert_eq!(free.len(), 1);
        assert!(free.contains(&var0));
    }

    #[test]
    fn free_vars_optional() {
        let mut ir = Ir::default();
        let var0 = TypeVar(0);
        let var_ty = ir.var_type(var0);
        let opt_ty = ir.optional_type(var_ty);

        let env = TypeEnv::default();
        let free = env.free_vars_type(opt_ty, &ir);

        assert_eq!(free.len(), 1);
        assert!(free.contains(&var0));
    }

    #[test]
    fn free_vars_function() {
        let mut ir = Ir::default();
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = ir.var_type(var0);
        let var1_ty = ir.var_type(var1);

        // Build function type: (Var(0)) -> Var(1)
        let fn_ty = ir.fn_type(vec![var0_ty], var1_ty);

        let env = TypeEnv::default();
        let free = env.free_vars_type(fn_ty, &ir);

        assert_eq!(free.len(), 2);
        assert!(free.contains(&var0));
        assert!(free.contains(&var1));
    }
}
