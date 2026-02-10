//! Type unification (simplified - no row polymorphism)
//!
//! This module implements unification for types with simple closed records.

use crate::ast::Loc;
use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ident, RecordFields, TypeId, TypeKind, TypeVar};

use super::{subst::Subst, TypeChecker};

impl TypeChecker {
    /// Resolve a Named type to its underlying type definition
    ///
    /// Given a DefId for a named type like `PersonShape`, finds the type definition
    /// statement and returns the TypeId of its underlying type (e.g., the record type).
    pub fn resolve_named_type(&mut self, def_id: DefId) -> Option<TypeId> {
        use crate::ir::StmtKind;

        let type_def = self.gcx.definitions.get(def_id);
        let type_name = type_def.name;

        // Search for the type definition in IR statements
        self.ir.root.clone().iter().find_map(|stmt_id| {
            let stmt = self.ir.stmts.get(*stmt_id);
            match &stmt.kind {
                StmtKind::Type { name, ty, .. } if *name == type_name => Some(*ty),
                _ => None,
            }
        })
    }

    /// Unify two types
    pub fn unify(&mut self, ty1_id: TypeId, ty2_id: TypeId, loc: Loc) -> Result<Subst, FossilError> {
        if ty1_id == ty2_id {
            return Ok(Subst::default());
        }

        let ty1 = self.ir.types.get(ty1_id);
        let ty2 = self.ir.types.get(ty2_id);

        match (&ty1.kind, &ty2.kind) {
            // Named type resolution - resolve to underlying type before unifying
            (TypeKind::Named(Ident::Resolved(def_id)), _) => {
                if let Some(resolved_ty) = self.resolve_named_type(*def_id) {
                    self.unify(resolved_ty, ty2_id, loc)
                } else {
                    // Can't resolve named type, fall through to error
                    let expected_str = self.format_type(ty1_id);
                    let actual_str = self.format_type(ty2_id);
                    Err(FossilError::type_mismatch(
                        format!(
                            "Cannot resolve named type `{}` to unify with `{}`",
                            expected_str, actual_str
                        ),
                        loc,
                    ))
                }
            }
            (_, TypeKind::Named(Ident::Resolved(def_id))) => {
                if let Some(resolved_ty) = self.resolve_named_type(*def_id) {
                    self.unify(ty1_id, resolved_ty, loc)
                } else {
                    let expected_str = self.format_type(ty1_id);
                    let actual_str = self.format_type(ty2_id);
                    Err(FossilError::type_mismatch(
                        format!(
                            "Cannot resolve named type `{}` to unify with `{}`",
                            actual_str, expected_str
                        ),
                        loc,
                    ))
                }
            }

            // Variable unification
            (TypeKind::Var(v1), TypeKind::Var(v2)) if v1 == v2 => Ok(Subst::default()),
            (TypeKind::Var(v), _) => self.bind_var(*v, ty2_id, loc),
            (_, TypeKind::Var(v)) => self.bind_var(*v, ty1_id, loc),

            // Unit types
            (TypeKind::Unit, TypeKind::Unit) => Ok(Subst::default()),

            // Primitive types
            (TypeKind::Primitive(p1), TypeKind::Primitive(p2)) if p1 == p2 => Ok(Subst::default()),

            // List types
            (TypeKind::List(elem1), TypeKind::List(elem2)) => {
                let elem1 = *elem1;
                let elem2 = *elem2;
                self.unify(elem1, elem2, loc)
            }

            // Optional types: T? ~ T? → unify inner
            (TypeKind::Optional(inner1), TypeKind::Optional(inner2)) => {
                let inner1 = *inner1;
                let inner2 = *inner2;
                self.unify(inner1, inner2, loc)
            }

            // Widening: T ~ T? → unify T with inner
            (_, TypeKind::Optional(inner2)) => {
                let inner2 = *inner2;
                self.unify(ty1_id, inner2, loc)
            }
            (TypeKind::Optional(inner1), _) => {
                let inner1 = *inner1;
                self.unify(inner1, ty2_id, loc)
            }

            // Applied types (generic types like List<T>, Entity<T>)
            (
                TypeKind::App {
                    ctor: c1,
                    args: args1,
                },
                TypeKind::App {
                    ctor: c2,
                    args: args2,
                },
            ) => {
                // Type constructors must match
                let ctors_match = match (c1, c2) {
                    (Ident::Resolved(id1), Ident::Resolved(id2)) => id1 == id2,
                    _ => false,
                };

                if !ctors_match {
                    let expected_str = self.format_type(ty1_id);
                    let actual_str = self.format_type(ty2_id);

                    return Err(FossilError::type_mismatch(
                        format!(
                            "Cannot unify `{}` with `{}` - different type constructors",
                            expected_str, actual_str
                        ),
                        loc,
                    ));
                }

                // Arity must match
                if args1.len() != args2.len() {
                    return Err(FossilError::arity_mismatch(args1.len(), args2.len(), loc));
                }

                // Unify type arguments positionally
                let args1_clone = args1.clone();
                let args2_clone = args2.clone();

                let mut subst = Subst::default();
                for (arg1, arg2) in args1_clone.iter().zip(args2_clone.iter()) {
                    let s = self.unify(*arg1, *arg2, loc)?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                Ok(subst)
            }

            // Records - simple structural unification (no row polymorphism)
            (TypeKind::Record(fields1), TypeKind::Record(fields2)) => {
                self.unify_records(fields1.clone(), fields2.clone(), loc)
            }

            // Functions
            (TypeKind::Function(params1, ret1), TypeKind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(FossilError::arity_mismatch(params1.len(), params2.len(), loc));
                }

                // Clone to avoid borrowing issues
                let params1_clone = params1.clone();
                let params2_clone = params2.clone();
                let ret1 = *ret1;
                let ret2 = *ret2;

                let mut subst = Subst::default();
                for (p1, p2) in params1_clone.iter().zip(params2_clone.iter()) {
                    // IMPORTANT: Apply current substitution to both params before unifying
                    // This ensures that constraints from earlier params propagate to later ones
                    let p1_applied = subst.apply(*p1, &mut self.ir);
                    let p2_applied = subst.apply(*p2, &mut self.ir);
                    let s = self.unify(p1_applied, p2_applied, loc)?;
                    subst = subst.compose(&s, &mut self.ir);
                }

                let r1 = subst.apply(ret1, &mut self.ir);
                let r2 = subst.apply(ret2, &mut self.ir);
                let s = self.unify(r1, r2, loc)?;

                Ok(subst.compose(&s, &mut self.ir))
            }

            // Type mismatch
            _ => {
                let expected_str = self.format_type(ty1_id);
                let actual_str = self.format_type(ty2_id);

                Err(FossilError::type_mismatch(
                    format!("Expected type `{}`, but found `{}`", expected_str, actual_str),
                    loc,
                ))
            }
        }
    }

    /// Bind a type variable to a type (with occurs check)
    pub fn bind_var(
        &mut self,
        var: TypeVar,
        ty_id: TypeId,
        loc: Loc,
    ) -> Result<Subst, FossilError> {
        let ty = self.ir.types.get(ty_id);

        // If binding to itself, no substitution needed
        if matches!(ty.kind, TypeKind::Var(v) if v == var) {
            return Ok(Subst::default());
        }

        // Occurs check: prevent infinite types
        if self.occurs_in(var, ty_id) {
            // Convert TypeVar to error::TypeVar
            let error_var = crate::error::TypeVar(var.0);
            return Err(FossilError::infinite_type(error_var, loc));
        }

        let mut subst = Subst::default();
        subst.insert(var, ty_id);
        Ok(subst)
    }

    /// Check if a type variable occurs in a type
    pub fn occurs_in(&self, var: TypeVar, ty_id: TypeId) -> bool {
        let ty = self.ir.types.get(ty_id);
        match &ty.kind {
            TypeKind::Var(v) => *v == var,
            TypeKind::App { args, .. } => args.iter().any(|arg| self.occurs_in(var, *arg)),
            TypeKind::Record(fields) => self.occurs_in_record(var, fields),
            TypeKind::Function(params, ret) => {
                params.iter().any(|p| self.occurs_in(var, *p)) || self.occurs_in(var, *ret)
            }
            TypeKind::List(elem) => self.occurs_in(var, *elem),
            TypeKind::Optional(inner) => self.occurs_in(var, *inner),
            TypeKind::Primitive(_)
            | TypeKind::Named(_)
            | TypeKind::Unit
            | TypeKind::Provider { .. } => false,
        }
    }

    /// Unify two records (simple structural unification - no row polymorphism)
    pub fn unify_records(
        &mut self,
        fields1: RecordFields,
        fields2: RecordFields,
        loc: Loc,
    ) -> Result<Subst, FossilError> {
        // Records must have the same number of fields
        if fields1.len() != fields2.len() {
            return Err(FossilError::record_size_mismatch(
                fields2.len(),
                fields1.len(),
                loc,
            ));
        }

        let mut subst = Subst::default();

        // For each field in fields1, find matching field in fields2
        for (name1, ty1) in &fields1.fields {
            if let Some(ty2) = fields2.lookup(*name1) {
                let ty1_applied = subst.apply(*ty1, &mut self.ir);
                let ty2_applied = subst.apply(ty2, &mut self.ir);
                let s = self.unify(ty1_applied, ty2_applied, loc)?;
                subst = subst.compose(&s, &mut self.ir);
            } else {
                let field_str = self.gcx.interner.resolve(*name1).to_string();
                return Err(FossilError::field_not_found(field_str, loc));
            }
        }

        Ok(subst)
    }

    /// Check if a type variable occurs in record fields
    pub fn occurs_in_record(&self, var: TypeVar, fields: &RecordFields) -> bool {
        fields.fields.iter().any(|(_, ty)| self.occurs_in(var, *ty))
    }
}
