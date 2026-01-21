//! Type environment for let-polymorphism
//!
//! The type environment tracks bindings from DefIds to polytypes,
//! supporting generalization and instantiation for let-polymorphism.

use std::collections::{HashMap, HashSet};

use crate::ast::thir;
use crate::context::DefId;

/// Type environment: maps DefIds to polytypes
#[derive(Clone, Debug, Default)]
pub struct TypeEnv {
    bindings: HashMap<DefId, thir::Polytype>,
}

impl TypeEnv {
    pub fn insert(&mut self, def_id: DefId, poly: thir::Polytype) {
        self.bindings.insert(def_id, poly);
    }

    pub fn lookup(&self, def_id: DefId) -> Option<&thir::Polytype> {
        self.bindings.get(&def_id)
    }

    /// Get free type variables in the environment
    pub fn free_type_vars(&self, thir: &thir::TypedHir) -> HashSet<thir::TypeVar> {
        self.bindings
            .values()
            .flat_map(|poly| self.free_vars_polytype(poly, thir))
            .collect()
    }

    fn free_vars_polytype(
        &self,
        poly: &thir::Polytype,
        thir: &thir::TypedHir,
    ) -> HashSet<thir::TypeVar> {
        let ty_vars = self.free_vars_type(poly.ty, thir);
        let bound: HashSet<_> = poly.forall.iter().copied().collect();
        ty_vars.difference(&bound).copied().collect()
    }

    pub fn free_vars_type(&self, ty_id: thir::TypeId, thir: &thir::TypedHir) -> HashSet<thir::TypeVar> {
        let ty = thir.types.get(ty_id);
        match &ty.kind {
            thir::TypeKind::Var(var) => {
                let mut set = HashSet::new();
                set.insert(*var);
                set
            }
            thir::TypeKind::App { args, .. } => {
                args.iter()
                    .flat_map(|arg| self.free_vars_type(*arg, thir))
                    .collect()
            }
            thir::TypeKind::Record(row) => self.free_vars_row(row, thir),
            thir::TypeKind::Function(params, ret) => {
                let mut vars: HashSet<_> = params
                    .iter()
                    .flat_map(|param| self.free_vars_type(*param, thir))
                    .collect();
                vars.extend(self.free_vars_type(*ret, thir));
                vars
            }
            thir::TypeKind::FieldSelector { record_ty, field_ty, .. } => {
                let mut vars = self.free_vars_type(*record_ty, thir);
                vars.extend(self.free_vars_type(*field_ty, thir));
                vars
            }
            thir::TypeKind::Primitive(_) | thir::TypeKind::Named(_) => HashSet::new(),
        }
    }

    pub fn free_vars_row(&self, row: &thir::RecordRow, thir: &thir::TypedHir) -> HashSet<thir::TypeVar> {
        match row {
            thir::RecordRow::Empty => HashSet::new(),
            thir::RecordRow::Extend { ty, rest, .. } => {
                let mut vars = self.free_vars_type(*ty, thir);
                vars.extend(self.free_vars_row(rest, thir));
                vars
            }
            thir::RecordRow::Var(v) => {
                let mut set = HashSet::new();
                set.insert(*v);
                set
            }
        }
    }

    /// Generalize a type to a polytype
    pub fn generalize(&self, ty_id: thir::TypeId, thir: &thir::TypedHir) -> thir::Polytype {
        let env_vars = self.free_type_vars(thir);
        let ty_vars = self.free_vars_type(ty_id, thir);
        let mut forall: Vec<_> = ty_vars.difference(&env_vars).copied().collect();
        forall.sort_by_key(|v| v.0);
        thir::Polytype { forall, ty: ty_id }
    }
}
