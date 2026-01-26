//! Type environment for let-polymorphism
//!
//! The type environment tracks bindings from DefIds to polytypes,
//! supporting generalization and instantiation for let-polymorphism.

use std::collections::{HashMap, HashSet};

use crate::context::DefId;
use crate::ir::{Ir, Polytype, RecordRow, TypeId, TypeKind, TypeVar};

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
            TypeKind::Record(row) => self.free_vars_row(row, ir),
            TypeKind::Function(params, ret) => {
                let mut vars: HashSet<_> = params
                    .iter()
                    .flat_map(|param| self.free_vars_type(*param, ir))
                    .collect();
                vars.extend(self.free_vars_type(*ret, ir));
                vars
            }
            TypeKind::FieldSelector {
                record_ty,
                field_ty,
                ..
            } => {
                let mut vars = self.free_vars_type(*record_ty, ir);
                vars.extend(self.free_vars_type(*field_ty, ir));
                vars
            }
            TypeKind::List(inner) => self.free_vars_type(*inner, ir),
            TypeKind::Primitive(_)
            | TypeKind::Named(_)
            | TypeKind::Unit
            | TypeKind::Provider { .. } => HashSet::new(),
        }
    }

    pub fn free_vars_row(&self, row: &RecordRow, ir: &Ir) -> HashSet<TypeVar> {
        match row {
            RecordRow::Empty => HashSet::new(),
            RecordRow::Extend { ty, rest, .. } => {
                let mut vars = self.free_vars_type(*ty, ir);
                vars.extend(self.free_vars_row(rest, ir));
                vars
            }
            RecordRow::Var(v) => {
                let mut set = HashSet::new();
                set.insert(*v);
                set
            }
        }
    }

    /// Generalize a type to a polytype
    pub fn generalize(&self, ty_id: TypeId, ir: &Ir) -> Polytype {
        let env_vars = self.free_type_vars(ir);
        let ty_vars = self.free_vars_type(ty_id, ir);
        let mut forall: Vec<_> = ty_vars.difference(&env_vars).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype {
            forall,
            constraints: vec![],
            ty: ty_id,
        }
    }
}
