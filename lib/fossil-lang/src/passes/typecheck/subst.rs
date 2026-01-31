//! Type substitution
//!
//! This module implements substitution (mapping from type variables to types)
//! which is a core component of Hindley-Milner type inference.

use std::collections::HashMap;

use crate::ir::{Ir, RecordFields, Type, TypeId, TypeKind, TypeVar};

/// Substitution: mapping from type variables to types
#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub(crate) map: HashMap<TypeVar, TypeId>,
}

impl Subst {
    /// Apply substitution to a type
    pub fn apply(&self, ty_id: TypeId, ir: &mut Ir) -> TypeId {
        self.apply_with_cache(ty_id, ir, &mut HashMap::new())
    }

    fn apply_with_cache(
        &self,
        ty_id: TypeId,
        ir: &mut Ir,
        cache: &mut HashMap<TypeId, TypeId>,
    ) -> TypeId {
        if let Some(&cached) = cache.get(&ty_id) {
            return cached;
        }

        // Clone what we need before any mutable operations
        let ty = ir.types.get(ty_id);
        let kind = ty.kind.clone();
        let loc = ty.loc;

        let result = match kind {
            TypeKind::Var(var) => match self.map.get(&var) {
                Some(&subst_ty) => self.apply_with_cache(subst_ty, ir, cache),
                None => ty_id,
            },

            TypeKind::App { ctor, args } => {
                let old_args = args.clone();
                let new_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.apply_with_cache(*arg, ir, cache))
                    .collect();

                if new_args == old_args {
                    ty_id
                } else {
                    ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::App { ctor, args: new_args },
                    })
                }
            }

            TypeKind::Record(fields) => {
                let old_fields = fields.clone();
                let new_fields = self.apply_fields(fields, ir, cache);

                if new_fields.fields == old_fields.fields {
                    ty_id
                } else {
                    ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::Record(new_fields),
                    })
                }
            }

            TypeKind::Function(params, ret) => {
                let old_params = params.clone();
                let new_params: Vec<_> = params
                    .iter()
                    .map(|param| self.apply_with_cache(*param, ir, cache))
                    .collect();
                let new_ret = self.apply_with_cache(ret, ir, cache);

                if new_params == old_params && new_ret == ret {
                    ty_id
                } else {
                    ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::Function(new_params, new_ret),
                    })
                }
            }

            TypeKind::List(inner) => {
                let new_inner = self.apply_with_cache(inner, ir, cache);
                if new_inner == inner {
                    ty_id
                } else {
                    ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::List(new_inner),
                    })
                }
            }

            TypeKind::Primitive(_)
            | TypeKind::Named(_)
            | TypeKind::Unit
            | TypeKind::Provider { .. } => ty_id,
        };

        cache.insert(ty_id, result);
        result
    }

    /// Compose two substitutions: self ∘ other
    pub fn compose(&self, other: &Subst, ir: &mut Ir) -> Subst {
        let mut composed = HashMap::new();

        // Apply self to all mappings in other
        for (&var, &ty) in &other.map {
            composed.insert(var, self.apply(ty, ir));
        }

        // Add mappings from self that aren't in other
        for (&var, &ty) in &self.map {
            composed.entry(var).or_insert(ty);
        }

        Subst { map: composed }
    }

    pub fn insert(&mut self, var: TypeVar, ty: TypeId) {
        self.map.insert(var, ty);
    }

    /// Apply substitution to record fields
    pub fn apply_fields(
        &self,
        fields: RecordFields,
        ir: &mut Ir,
        cache: &mut HashMap<TypeId, TypeId>,
    ) -> RecordFields {
        let new_fields = fields
            .fields
            .iter()
            .map(|(name, ty)| (*name, self.apply_with_cache(*ty, ir, cache)))
            .collect();
        RecordFields { fields: new_fields }
    }
}
