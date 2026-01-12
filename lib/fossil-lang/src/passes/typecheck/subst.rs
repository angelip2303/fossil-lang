//! Type substitution
//!
//! This module implements substitution (mapping from type variables to types)
//! which is a core component of Hindley-Milner type inference.

use std::collections::HashMap;

use crate::ast::thir;

/// Substitution: mapping from type variables to types
#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub(crate) map: HashMap<thir::TypeVar, thir::TypeId>,
}

impl Subst {
    /// Apply substitution to a type
    pub fn apply(&self, ty_id: thir::TypeId, thir: &mut thir::TypedHir) -> thir::TypeId {
        self.apply_with_cache(ty_id, thir, &mut HashMap::new())
    }

    fn apply_with_cache(
        &self,
        ty_id: thir::TypeId,
        thir_ast: &mut thir::TypedHir,
        cache: &mut HashMap<thir::TypeId, thir::TypeId>,
    ) -> thir::TypeId {
        if let Some(&cached) = cache.get(&ty_id) {
            return cached;
        }

        // Clone what we need before any mutable operations
        let ty = thir_ast.types.get(ty_id);
        let kind = ty.kind.clone();
        let loc = ty.loc.clone();

        let result = match kind {
            thir::TypeKind::Var(var) => match self.map.get(&var) {
                Some(&subst_ty) => self.apply_with_cache(subst_ty, thir_ast, cache),
                None => ty_id,
            },

            thir::TypeKind::App { ctor, args } => {
                let old_args = args.clone();
                let new_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.apply_with_cache(*arg, thir_ast, cache))
                    .collect();

                if new_args == old_args {
                    ty_id
                } else {
                    thir_ast.types.alloc(thir::Type {
                        loc: loc.clone(),
                        kind: thir::TypeKind::App { ctor, args: new_args },
                    })
                }
            }

            thir::TypeKind::Record(row) => {
                let old_row = row.clone();
                let new_row = self.apply_row(row, thir_ast, cache);

                if new_row == old_row {
                    ty_id
                } else {
                    thir_ast.types.alloc(thir::Type {
                        loc: loc.clone(),
                        kind: thir::TypeKind::Record(new_row),
                    })
                }
            }

            thir::TypeKind::Function(params, ret) => {
                let old_params = params.clone();
                let new_params: Vec<_> = params
                    .iter()
                    .map(|param| self.apply_with_cache(*param, thir_ast, cache))
                    .collect();
                let new_ret = self.apply_with_cache(ret, thir_ast, cache);

                if new_params == old_params && new_ret == ret {
                    ty_id
                } else {
                    thir_ast.types.alloc(thir::Type {
                        loc: loc.clone(),
                        kind: thir::TypeKind::Function(new_params, new_ret),
                    })
                }
            }

            thir::TypeKind::Primitive(_) | thir::TypeKind::Named(_) => ty_id,
        };

        cache.insert(ty_id, result);
        result
    }

    /// Compose two substitutions: self ∘ other
    pub fn compose(&self, other: &Subst, thir: &mut thir::TypedHir) -> Subst {
        let mut composed = HashMap::new();

        // Apply self to all mappings in other
        for (&var, &ty) in &other.map {
            composed.insert(var, self.apply(ty, thir));
        }

        // Add mappings from self that aren't in other
        for (&var, &ty) in &self.map {
            composed.entry(var).or_insert(ty);
        }

        Subst { map: composed }
    }

    pub fn insert(&mut self, var: thir::TypeVar, ty: thir::TypeId) {
        self.map.insert(var, ty);
    }

    /// Apply substitution to a record row
    pub fn apply_row(
        &self,
        row: thir::RecordRow,
        thir_ast: &mut thir::TypedHir,
        cache: &mut HashMap<thir::TypeId, thir::TypeId>,
    ) -> thir::RecordRow {
        match row {
            thir::RecordRow::Empty => thir::RecordRow::Empty,
            thir::RecordRow::Extend { field, ty, rest } => {
                let new_ty = self.apply_with_cache(ty, thir_ast, cache);
                let new_rest = Box::new(self.apply_row(*rest, thir_ast, cache));
                thir::RecordRow::Extend {
                    field,
                    ty: new_ty,
                    rest: new_rest,
                }
            }
            thir::RecordRow::Var(v) => {
                // Check if v is bound in the substitution
                if let Some(&bound_ty) = self.map.get(&v) {
                    // Extract the row from the bound type
                    let bound = thir_ast.types.get(bound_ty);
                    if let thir::TypeKind::Record(row) = &bound.kind {
                        row.clone()
                    } else {
                        thir::RecordRow::Var(v)
                    }
                } else {
                    thir::RecordRow::Var(v)
                }
            }
        }
    }
}
