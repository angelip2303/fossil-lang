//! Type utilities — environment, substitution, fresh-var generation and
//! pretty-printing — all in terms of the interned `Ty` identifier.
//!
//! With canonical interning, substitution becomes a pure recursive walk that
//! re-interns the rewritten `TyKind`. There is no per-IR arena and therefore
//! no `&mut Ir` plumbing in any helper here.

use std::collections::{HashMap, HashSet};

use crate::ast::Loc;
use crate::db::{Db, DefId};
use crate::error::FossilError;
use crate::ty::types::{Polytype, RecordFields, Ty, TyKind, TypeVar};

use super::TypeChecker;

#[derive(Clone, Debug, Default)]
pub struct TypeEnv {
    pub(crate) bindings: HashMap<DefId, Polytype>,
}

impl TypeEnv {
    pub fn insert(&mut self, def_id: DefId, poly: Polytype) {
        self.bindings.insert(def_id, poly);
    }

    pub fn lookup(&self, def_id: DefId) -> Option<&Polytype> {
        self.bindings.get(&def_id)
    }

    pub fn free_type_vars(&self, db: &dyn Db) -> HashSet<TypeVar> {
        self.bindings
            .values()
            .flat_map(|poly| free_vars_polytype(poly, db))
            .collect()
    }

    pub fn generalize(&self, ty: Ty, db: &dyn Db) -> Polytype {
        let env_vars = self.free_type_vars(db);
        let ty_vars = free_vars_type(ty, db);
        let mut forall: Vec<_> = ty_vars.difference(&env_vars).copied().collect();
        forall.sort_by_key(|v| v.0);
        Polytype { forall, ty }
    }
}

fn free_vars_polytype(poly: &Polytype, db: &dyn Db) -> HashSet<TypeVar> {
    let ty_vars = free_vars_type(poly.ty, db);
    let bound: HashSet<_> = poly.forall.iter().copied().collect();
    ty_vars.difference(&bound).copied().collect()
}

/// Free type variables of an interned `Ty`. Pure recursive walk; cheap
/// because each step is a Salsa interned-table lookup.
pub fn free_vars_type(ty: Ty, db: &dyn Db) -> HashSet<TypeVar> {
    match ty.kind(db) {
        TyKind::Var(var) => {
            let mut set = HashSet::new();
            set.insert(var);
            set
        }
        TyKind::Record(fields) => free_vars_fields(&fields, db),
        TyKind::Function(params, ret) => {
            let mut vars: HashSet<_> = params
                .iter()
                .flat_map(|p| free_vars_type(*p, db))
                .collect();
            vars.extend(free_vars_type(ret, db));
            vars
        }
        TyKind::Optional(inner) => free_vars_type(inner, db),
        TyKind::Primitive(_) | TyKind::Named(_) | TyKind::Unit | TyKind::Error => HashSet::new(),
    }
}

pub fn free_vars_fields(fields: &RecordFields, db: &dyn Db) -> HashSet<TypeVar> {
    fields
        .fields
        .iter()
        .flat_map(|(_, ty)| free_vars_type(*ty, db))
        .collect()
}

// ── Substitution ───────────────────────────────────────────────────

#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub(crate) map: HashMap<TypeVar, Ty>,
}

impl Subst {
    /// Apply this substitution to `ty`. Result is itself an interned `Ty`,
    /// canonically deduplicated, so equality holds even when the substitution
    /// is reapplied along a different path.
    pub fn apply(&self, ty: Ty, db: &dyn Db) -> Ty {
        self.apply_with_cache(ty, db, &mut HashMap::new())
    }

    fn apply_with_cache(&self, ty: Ty, db: &dyn Db, cache: &mut HashMap<Ty, Ty>) -> Ty {
        if let Some(&cached) = cache.get(&ty) {
            return cached;
        }

        let result = match ty.kind(db) {
            TyKind::Var(var) => match self.map.get(&var) {
                Some(&subst_ty) => self.apply_with_cache(subst_ty, db, cache),
                None => ty,
            },
            TyKind::Record(fields) => {
                let new_fields: Vec<_> = fields
                    .fields
                    .into_iter()
                    .map(|(name, t)| (name, self.apply_with_cache(t, db, cache)))
                    .collect();
                Ty::mk_record(db, new_fields)
            }
            TyKind::Function(params, ret) => {
                let new_params: Vec<_> = params
                    .into_iter()
                    .map(|p| self.apply_with_cache(p, db, cache))
                    .collect();
                let new_ret = self.apply_with_cache(ret, db, cache);
                Ty::mk_function(db, new_params, new_ret)
            }
            TyKind::Optional(inner) => {
                let new_inner = self.apply_with_cache(inner, db, cache);
                Ty::mk_optional(db, new_inner)
            }
            TyKind::Primitive(_) | TyKind::Named(_) | TyKind::Unit | TyKind::Error => ty,
        };

        cache.insert(ty, result);
        result
    }

    pub fn compose(&self, other: &Subst, db: &dyn Db) -> Subst {
        let mut composed = HashMap::new();

        for (&var, &ty) in &other.map {
            composed.insert(var, self.apply(ty, db));
        }

        for (&var, &ty) in &self.map {
            composed.entry(var).or_insert(ty);
        }

        Subst { map: composed }
    }

    pub fn insert(&mut self, var: TypeVar, ty: Ty) {
        self.map.insert(var, ty);
    }
}

// ── TypeChecker helpers ────────────────────────────────────────────

impl TypeChecker<'_> {
    pub fn fresh_type_var(&mut self, _loc: Loc) -> Ty {
        let var = self.tvg.fresh();
        Ty::mk_var(self.db, var)
    }

    pub fn fresh_type_var_generated(&mut self) -> Ty {
        let var = self.tvg.fresh();
        Ty::mk_var(self.db, var)
    }

    pub fn format_type(&self, ty: Ty) -> String {
        match ty.kind(self.db) {
            TyKind::Primitive(p) => format!("{:?}", p),
            TyKind::Var(v) => format!("'{}", v.0),
            TyKind::Unit => "()".to_string(),
            TyKind::Error => "<error>".to_string(),
            TyKind::Named(def_id) => {
                let name = def_id.name(self.db).text(self.db);
                if let Some(ns) = def_id.namespace(self.db) {
                    format!("{}.{}", ns.text(self.db), name)
                } else {
                    name.to_string()
                }
            }
            TyKind::Function(params, ret) => {
                let param_strs: Vec<_> = params.iter().map(|p| self.format_type(*p)).collect();
                format!("({}) -> {}", param_strs.join(", "), self.format_type(ret))
            }
            TyKind::Optional(inner) => format!("{}?", self.format_type(inner)),
            TyKind::Record(fields) => format!("{{{}}}", self.format_fields(&fields)),
        }
    }

    fn format_fields(&self, fields: &RecordFields) -> String {
        fields
            .fields
            .iter()
            .map(|(name, ty)| {
                let field_name = name.text(self.db);
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
    use crate::db::{DefId, DefKindTag, FossilDb, Symbol};
    use crate::ty::types::Ty;

    fn test_def_id(db: &FossilDb) -> DefId {
        DefId::new(db, None, Symbol::new(db, "test"), DefKindTag::Let)
    }

    #[test]
    fn env_insert_and_lookup() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let def_id = test_def_id(&db);
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
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);

        let env = TypeEnv::default();
        let poly = env.generalize(int_ty, &db);

        assert!(poly.forall.is_empty());
        assert_eq!(poly.ty, int_ty);
    }

    #[test]
    fn generalize_with_free_vars() {
        let db = FossilDb::default();
        let var0 = TypeVar(0);
        let var_ty = Ty::mk_var(&db, var0);

        let env = TypeEnv::default();
        let poly = env.generalize(var_ty, &db);

        assert_eq!(poly.forall, vec![var0]);
        assert_eq!(poly.ty, var_ty);
    }

    #[test]
    fn generalize_env_constrains() {
        let db = FossilDb::default();
        let var0 = TypeVar(0);
        let var_ty = Ty::mk_var(&db, var0);
        let def_id = test_def_id(&db);

        let mut env = TypeEnv::default();
        env.insert(def_id, Polytype::mono(var_ty));

        let poly = env.generalize(var_ty, &db);

        assert!(poly.forall.is_empty());
        assert_eq!(poly.ty, var_ty);
    }

    #[test]
    fn free_vars_primitive() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let free = free_vars_type(int_ty, &db);
        assert!(free.is_empty());
    }

    #[test]
    fn free_vars_type_var() {
        let db = FossilDb::default();
        let var0 = TypeVar(0);
        let var_ty = Ty::mk_var(&db, var0);
        let free = free_vars_type(var_ty, &db);
        assert_eq!(free.len(), 1);
        assert!(free.contains(&var0));
    }

    #[test]
    fn free_vars_optional() {
        let db = FossilDb::default();
        let var0 = TypeVar(0);
        let var_ty = Ty::mk_var(&db, var0);
        let opt_ty = Ty::mk_optional(&db, var_ty);
        let free = free_vars_type(opt_ty, &db);
        assert_eq!(free.len(), 1);
        assert!(free.contains(&var0));
    }

    #[test]
    fn free_vars_function() {
        let db = FossilDb::default();
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = Ty::mk_var(&db, var0);
        let var1_ty = Ty::mk_var(&db, var1);

        let fn_ty = Ty::mk_function(&db, vec![var0_ty], var1_ty);
        let free = free_vars_type(fn_ty, &db);
        assert_eq!(free.len(), 2);
        assert!(free.contains(&var0));
        assert!(free.contains(&var1));
    }

    #[test]
    fn apply_identity() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let subst = Subst::default();
        let result = subst.apply(int_ty, &db);
        assert_eq!(result, int_ty);
    }

    #[test]
    fn apply_var_to_concrete() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let var0 = TypeVar(0);
        let var_ty = Ty::mk_var(&db, var0);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(var_ty, &db);
        assert_eq!(result, int_ty);
    }

    #[test]
    fn apply_unbound_var() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var1_ty = Ty::mk_var(&db, var1);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(var1_ty, &db);
        assert_eq!(result, var1_ty);
    }

    #[test]
    fn apply_nested_function() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let str_ty = Ty::mk_string(&db);
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = Ty::mk_var(&db, var0);
        let var1_ty = Ty::mk_var(&db, var1);

        let fn_ty = Ty::mk_function(&db, vec![var0_ty], var1_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);
        subst.insert(var1, str_ty);

        let result = subst.apply(fn_ty, &db);
        let expected = Ty::mk_function(&db, vec![int_ty], str_ty);
        assert_eq!(result, expected);
    }

    #[test]
    fn apply_nested_record() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let var0 = TypeVar(0);
        let var0_ty = Ty::mk_var(&db, var0);
        let field_name = Symbol::new(&db, "_test");

        let record_ty = Ty::mk_record(&db, vec![(field_name, var0_ty)]);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(record_ty, &db);
        let expected = Ty::mk_record(&db, vec![(field_name, int_ty)]);
        assert_eq!(result, expected);
    }

    #[test]
    fn apply_preserves_named() {
        let db = FossilDb::default();
        let named_ty = Ty::mk_named(&db, test_def_id(&db));

        let var0 = TypeVar(0);
        let int_ty = Ty::mk_int(&db);
        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(named_ty, &db);
        assert_eq!(result, named_ty);
    }

    #[test]
    fn apply_nested_optional() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let var0 = TypeVar(0);
        let var0_ty = Ty::mk_var(&db, var0);

        let opt_ty = Ty::mk_optional(&db, var0_ty);

        let mut subst = Subst::default();
        subst.insert(var0, int_ty);

        let result = subst.apply(opt_ty, &db);
        assert_eq!(result, Ty::mk_optional(&db, int_ty));
    }

    #[test]
    fn compose_applies_to_values() {
        let db = FossilDb::default();
        let int_ty = Ty::mk_int(&db);
        let var0 = TypeVar(0);
        let var1 = TypeVar(1);
        let var0_ty = Ty::mk_var(&db, var0);
        let var1_ty = Ty::mk_var(&db, var1);

        let mut s1 = Subst::default();
        s1.insert(var0, int_ty);

        let mut s2 = Subst::default();
        s2.insert(var1, var0_ty);

        let composed = s1.compose(&s2, &db);

        let result = composed.apply(var1_ty, &db);
        assert_eq!(result, int_ty);
    }
}
