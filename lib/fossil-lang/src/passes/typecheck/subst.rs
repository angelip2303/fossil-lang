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

            TypeKind::Optional(inner) => {
                let new_inner = self.apply_with_cache(inner, ir, cache);
                if new_inner == inner {
                    ty_id
                } else {
                    ir.types.alloc(Type {
                        loc,
                        kind: TypeKind::Optional(new_inner),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Symbol;
    use crate::ir::{PrimitiveType, Type, TypeKind, TypeVar};

    #[test]
    fn apply_identity() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let subst = Subst::default();

        let result = subst.apply(int_ty, &mut ir);
        assert_eq!(result, int_ty);
    }

    #[test]
    fn apply_var_to_concrete() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var_ty = ir.var_type(var0);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(var_ty, &mut ir);
        assert_eq!(result, int_ty);
    }

    #[test]
    fn apply_unbound_var() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var1_ty = ir.var_type(var1);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(var1_ty, &mut ir);
        assert_eq!(result, var1_ty);
    }

    #[test]
    fn apply_nested_function() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let str_ty = ir.string_type();
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = ir.var_type(var0);
        let var1_ty = ir.var_type(var1);

        // Build function type: (Var(0)) -> Var(1)
        let fn_ty = ir.fn_type(vec![var0_ty], var1_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);
        subst.insert(var1, str_ty);

        let result = subst.apply(fn_ty, &mut ir);

        let result_kind = &ir.types.get(result).kind;
        match result_kind {
            TypeKind::Function(params, ret) => {
                assert_eq!(
                    ir.types.get(params[0]).kind,
                    TypeKind::Primitive(PrimitiveType::Int)
                );
                assert_eq!(
                    ir.types.get(*ret).kind,
                    TypeKind::Primitive(PrimitiveType::String)
                );
            }
            other => panic!("Expected Function type, got {:?}", other),
        }
    }

    #[test]
    fn apply_nested_list() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var0_ty = ir.var_type(var0);

        // Build list type: [Var(0)]
        let list_ty = ir.list_type(var0_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(list_ty, &mut ir);

        let result_kind = &ir.types.get(result).kind;
        match result_kind {
            TypeKind::List(inner) => {
                assert_eq!(
                    ir.types.get(*inner).kind,
                    TypeKind::Primitive(PrimitiveType::Int)
                );
            }
            other => panic!("Expected List type, got {:?}", other),
        }
    }

    #[test]
    fn apply_nested_record() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var0_ty = ir.var_type(var0);
        let field_name = Symbol::synthetic();

        // Build record type with a Var(0) field
        let record_ty = ir.types.alloc(Type {
            loc: crate::ast::Loc::generated(),
            kind: TypeKind::Record(RecordFields::from_fields(vec![(field_name, var0_ty)])),
        });

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(record_ty, &mut ir);

        let result_kind = &ir.types.get(result).kind;
        match result_kind {
            TypeKind::Record(fields) => {
                let (_, field_ty) = &fields.fields[0];
                assert_eq!(
                    ir.types.get(*field_ty).kind,
                    TypeKind::Primitive(PrimitiveType::Int)
                );
            }
            other => panic!("Expected Record type, got {:?}", other),
        }
    }

    #[test]
    fn apply_preserves_named() {
        let mut ir = Ir::default();
        let named_ty = ir.named_type(crate::context::DefId::new(0));

        // Substitution with unrelated var should leave Named type unchanged
        let var0 = TypeVar(0);
        let int_ty = ir.int_type();
        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(named_ty, &mut ir);
        assert_eq!(result, named_ty);
    }

    #[test]
    fn apply_nested_optional() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var0_ty = ir.var_type(var0);

        let opt_ty = ir.optional_type(var0_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(opt_ty, &mut ir);
        match &ir.types.get(result).kind {
            TypeKind::Optional(inner) => {
                assert_eq!(
                    ir.types.get(*inner).kind,
                    TypeKind::Primitive(PrimitiveType::Int)
                );
            }
            other => panic!("Expected Optional type, got {:?}", other),
        }
    }

    #[test]
    fn compose_applies_to_values() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = ir.var_type(var0);
        let var1_ty = ir.var_type(var1);

        // S1 = {Var(0) -> int}
        let mut s1 = Subst::default();
        s1.insert(var0, int_ty);

        // S2 = {Var(1) -> Var(0)}
        let mut s2 = Subst::default();
        s2.insert(var1, var0_ty);

        // compose(S1, S2) should give us a substitution where
        // Var(1) -> int (because S1 is applied to S2's values)
        let composed = s1.compose(&s2, &mut ir);

        let result = composed.apply(var1_ty, &mut ir);
        assert_eq!(
            ir.types.get(result).kind,
            TypeKind::Primitive(PrimitiveType::Int)
        );
    }
}
