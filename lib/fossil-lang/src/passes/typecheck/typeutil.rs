//! Type utilities: environment, substitution, and helpers.
//!
//! Consolidates all type-level utilities needed by the type checker:
//! - `TypeEnv`: type environment (DefId → Polytype bindings, generalization)
//! - `Subst`: type substitution (TypeVar → TypeId mapping, composition)
//! - Helpers: fresh type vars, type formatting, source type extraction

use std::collections::{HashMap, HashSet};

use crate::ast::Loc;
use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ir, Polytype, RecordFields, Type, TypeId, TypeKind, TypeVar};

use super::TypeChecker;

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
            TypeKind::Record(fields) => self.free_vars_fields(fields, ir),
            TypeKind::Function(params, ret) => {
                let mut vars: HashSet<_> = params
                    .iter()
                    .flat_map(|param| self.free_vars_type(*param, ir))
                    .collect();
                vars.extend(self.free_vars_type(*ret, ir));
                vars
            }
            TypeKind::Optional(inner) => self.free_vars_type(*inner, ir),
            TypeKind::Primitive(_)
            | TypeKind::Named(_)
            | TypeKind::Unresolved(_)
            | TypeKind::Unit => HashSet::new(),
        }
    }

    pub fn free_vars_fields(&self, fields: &RecordFields, ir: &Ir) -> HashSet<TypeVar> {
        fields
            .fields
            .iter()
            .flat_map(|(_, ty)| self.free_vars_type(*ty, ir))
            .collect()
    }

    pub fn generalize(&self, ty_id: TypeId, ir: &Ir) -> Polytype {
        let env_vars = self.free_type_vars(ir);
        let ty_vars = self.free_vars_type(ty_id, ir);
        let mut forall: Vec<_> = ty_vars.difference(&env_vars).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype { forall, ty: ty_id }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub(crate) map: HashMap<TypeVar, TypeId>,
}

impl Subst {
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

        let ty = ir.types.get(ty_id);
        let kind = ty.kind.clone();
        let loc = ty.loc;

        let result = match kind {
            TypeKind::Var(var) => match self.map.get(&var) {
                Some(&subst_ty) => self.apply_with_cache(subst_ty, ir, cache),
                None => ty_id,
            },

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
            | TypeKind::Unresolved(_)
            | TypeKind::Unit => ty_id,
        };

        cache.insert(ty_id, result);
        result
    }

    pub fn compose(&self, other: &Subst, ir: &mut Ir) -> Subst {
        let mut composed = HashMap::new();

        for (&var, &ty) in &other.map {
            composed.insert(var, self.apply(ty, ir));
        }

        for (&var, &ty) in &self.map {
            composed.entry(var).or_insert(ty);
        }

        Subst { map: composed }
    }

    pub fn insert(&mut self, var: TypeVar, ty: TypeId) {
        self.map.insert(var, ty);
    }

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

impl TypeChecker {
    pub fn fresh_type_var(&mut self, loc: Loc) -> TypeId {
        let var = self.tvg.fresh();
        self.ir.types.alloc(Type {
            loc,
            kind: TypeKind::Var(var),
        })
    }

    pub fn fresh_type_var_generated(&mut self) -> TypeId {
        self.ir.var_type(self.tvg.fresh())
    }

    pub fn format_type(&self, ty_id: TypeId) -> String {
        let ty = self.ir.types.get(ty_id);
        match &ty.kind {
            TypeKind::Primitive(p) => format!("{:?}", p),
            TypeKind::Var(v) => format!("'{}", v.0),
            TypeKind::Unit => "()".to_string(),
            TypeKind::Named(def_id) => {
                let def = self.gcx.definitions.get(*def_id);
                self.gcx.interner.resolve(def.name).to_string()
            }
            TypeKind::Unresolved(path) => path.display(&self.gcx.interner),
            TypeKind::Function(params, ret) => {
                let param_strs: Vec<_> = params.iter().map(|p| self.format_type(*p)).collect();
                format!("({}) -> {}", param_strs.join(", "), self.format_type(*ret))
            }
            TypeKind::Record(fields) => {
                format!("{{{}}}", self.format_fields(fields))
            }
            TypeKind::Optional(inner) => format!("{}?", self.format_type(*inner)),
        }
    }

    fn format_fields(&self, fields: &RecordFields) -> String {
        fields
            .fields
            .iter()
            .map(|(name, ty)| {
                let field_name = self.gcx.interner.resolve(*name);
                let ty_str = self.format_type(*ty);
                format!("{}: {}", field_name, ty_str)
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn get_type_ctor_param_count(&self, type_def_id: DefId) -> usize {
        self.lookup_type_info(type_def_id)
            .map(|info| info.ctor_param_count)
            .unwrap_or(0)
    }

    pub fn check_ctor_arg_count(
        &self,
        type_def_id: DefId,
        actual_args: usize,
        loc: Loc,
    ) -> Result<(), FossilError> {
        let expected_params = self.get_type_ctor_param_count(type_def_id);

        if actual_args != expected_params {
            return Err(FossilError::arity_mismatch(
                expected_params,
                actual_args,
                loc,
            ));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{DefId, Symbol};
    use crate::ir::{Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};

    #[test]
    fn env_insert_and_lookup() {
        let ir = Ir::default();
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
    fn generalize_no_free_vars() {
        let ir = Ir::default();
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

        let mut env = TypeEnv::default();
        env.insert(def_id, Polytype::mono(var_ty));

        let poly = env.generalize(var_ty, &ir);

        assert!(poly.forall.is_empty());
        assert_eq!(poly.ty, var_ty);
    }

    #[test]
    fn free_vars_primitive() {
        let ir = Ir::default();
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

        let fn_ty = ir.fn_type(vec![var0_ty], var1_ty);

        let env = TypeEnv::default();
        let free = env.free_vars_type(fn_ty, &ir);

        assert_eq!(free.len(), 2);
        assert!(free.contains(&var0));
        assert!(free.contains(&var1));
    }

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

        let fn_ty = ir.fn_type(vec![var0_ty], var1_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);
        subst.insert(var1, str_ty);

        let result = subst.apply(fn_ty, &mut ir);

        match &ir.types.get(result).kind {
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
    fn apply_nested_record() {
        let mut ir = Ir::default();
        let int_ty = ir.int_type();
        let var0 = TypeVar(0);
        let var0_ty = ir.var_type(var0);
        let field_name = Symbol::synthetic();

        let record_ty = ir.types.alloc(Type {
            loc: crate::ast::Loc::generated(),
            kind: TypeKind::Record(RecordFields::from_fields(vec![(field_name, var0_ty)])),
        });

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(record_ty, &mut ir);

        match &ir.types.get(result).kind {
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
        let named_ty = ir.named_type(DefId::new(0));

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

        let mut s1 = Subst::default();
        s1.insert(var0, int_ty);

        let mut s2 = Subst::default();
        s2.insert(var1, var0_ty);

        let composed = s1.compose(&s2, &mut ir);

        let result = composed.apply(var1_ty, &mut ir);
        assert_eq!(
            ir.types.get(result).kind,
            TypeKind::Primitive(PrimitiveType::Int)
        );
    }
}
