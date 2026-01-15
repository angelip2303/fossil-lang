//! Type unification with row polymorphism
//!
//! This module implements unification for types, including support for
//! extensible records with row polymorphism.

use crate::ast::{Loc, thir};
use crate::context::Symbol;
use crate::error::{CompileError, CompileErrorKind, ErrorSuggestion};

use super::{TypeChecker, subst::Subst};

impl TypeChecker {
    /// Unify two types
    pub fn unify(
        &mut self,
        ty1_id: thir::TypeId,
        ty2_id: thir::TypeId,
        loc: Loc,
    ) -> Result<Subst, CompileError> {
        if ty1_id == ty2_id {
            return Ok(Subst::default());
        }

        let ty1 = self.target.types.get(ty1_id);
        let ty2 = self.target.types.get(ty2_id);

        match (&ty1.kind, &ty2.kind) {
            // Variable unification
            (thir::TypeKind::Var(v1), thir::TypeKind::Var(v2)) if v1 == v2 => Ok(Subst::default()),
            (thir::TypeKind::Var(v), _) => self.bind_var(*v, ty2_id, loc),
            (_, thir::TypeKind::Var(v)) => self.bind_var(*v, ty1_id, loc),

            // Primitive types
            (thir::TypeKind::Primitive(p1), thir::TypeKind::Primitive(p2)) if p1 == p2 => {
                Ok(Subst::default())
            }

            // Applied types (generic types like List<T>, Entity<T>)
            (
                thir::TypeKind::App { ctor: c1, args: args1 },
                thir::TypeKind::App { ctor: c2, args: args2 },
            ) => {
                // Type constructors must match
                if c1 != c2 {
                    let expected_str = self.format_type(ty1_id);
                    let actual_str = self.format_type(ty2_id);

                    return Err(CompileError::new(
                        CompileErrorKind::TypeMismatch {
                            expected: ty1_id,
                            actual: ty2_id,
                        },
                        loc,
                    )
                    .with_context(format!(
                        "Cannot unify `{}` with `{}` - different type constructors",
                        expected_str, actual_str
                    )));
                }

                // Arity must match
                if args1.len() != args2.len() {
                    return Err(CompileError::new(
                        CompileErrorKind::ArityMismatch {
                            expected: args1.len(),
                            actual: args2.len(),
                        },
                        loc,
                    )
                    .with_context(format!(
                        "Type constructor expects {} type arguments but got {}",
                        args1.len(),
                        args2.len()
                    )));
                }

                // Unify type arguments positionally
                let args1_clone = args1.clone();
                let args2_clone = args2.clone();

                let mut subst = Subst::default();
                for (arg1, arg2) in args1_clone.iter().zip(args2_clone.iter()) {
                    let s = self.unify(*arg1, *arg2, loc.clone())?;
                    subst = subst.compose(&s, &mut self.target);
                }

                Ok(subst)
            }

            // Records - use row polymorphism
            (thir::TypeKind::Record(row1), thir::TypeKind::Record(row2)) => {
                self.unify_rows(row1.clone(), row2.clone(), loc)
            }

            // Functions
            (thir::TypeKind::Function(params1, ret1), thir::TypeKind::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(CompileError::new(
                        CompileErrorKind::ArityMismatch {
                            expected: params1.len(),
                            actual: params2.len(),
                        },
                        loc,
                    )
                    .with_context(format!(
                        "Function takes {} parameters but is being used with {}",
                        params1.len(),
                        params2.len()
                    )));
                }

                // Clone to avoid borrowing issues
                let params1_clone = params1.clone();
                let params2_clone = params2.clone();
                let ret1 = *ret1;
                let ret2 = *ret2;

                let mut subst = Subst::default();
                for (p1, p2) in params1_clone.iter().zip(params2_clone.iter()) {
                    let s = self.unify(*p1, *p2, loc.clone())?;
                    subst = subst.compose(&s, &mut self.target);
                }

                let r1 = subst.apply(ret1, &mut self.target);
                let r2 = subst.apply(ret2, &mut self.target);
                let s = self.unify(r1, r2, loc)?;

                Ok(subst.compose(&s, &mut self.target))
            }

            // Type mismatch
            _ => {
                let expected_str = self.format_type(ty1_id);
                let actual_str = self.format_type(ty2_id);

                Err(CompileError::new(
                    CompileErrorKind::TypeMismatch {
                        expected: ty1_id,
                        actual: ty2_id,
                    },
                    loc,
                )
                .with_context(format!(
                    "Expected type `{}`, but found `{}`",
                    expected_str, actual_str
                ))
                .with_suggestion(ErrorSuggestion::Help(
                    "Types must match exactly. Consider adding explicit type annotations.".to_string()
                )))
            }
        }
    }

    /// Bind a type variable to a type (with occurs check)
    pub fn bind_var(
        &mut self,
        var: thir::TypeVar,
        ty_id: thir::TypeId,
        loc: Loc,
    ) -> Result<Subst, CompileError> {
        let ty = self.target.types.get(ty_id);

        // If binding to itself, no substitution needed
        if matches!(ty.kind, thir::TypeKind::Var(v) if v == var) {
            return Ok(Subst::default());
        }

        // Occurs check: prevent infinite types
        if self.occurs_in(var, ty_id) {
            // Convert thir::TypeVar to error::TypeVar
            let error_var = crate::error::TypeVar(var.0);
            return Err(CompileError::new(
                CompileErrorKind::InfiniteType(error_var),
                loc,
            )
            .with_context(format!(
                "Type variable {} occurs in the type being unified, creating an infinite type",
                error_var
            )));
        }

        let mut subst = Subst::default();
        subst.insert(var, ty_id);
        Ok(subst)
    }

    /// Check if a type variable occurs in a type
    pub fn occurs_in(&self, var: thir::TypeVar, ty_id: thir::TypeId) -> bool {
        let ty = self.target.types.get(ty_id);
        match &ty.kind {
            thir::TypeKind::Var(v) => *v == var,
            thir::TypeKind::App { args, .. } => {
                args.iter().any(|arg| self.occurs_in(var, *arg))
            }
            thir::TypeKind::Record(row) => self.occurs_in_row(var, row),
            thir::TypeKind::Function(params, ret) => {
                params.iter().any(|p| self.occurs_in(var, *p)) || self.occurs_in(var, *ret)
            }
            thir::TypeKind::Primitive(_) | thir::TypeKind::Named(_) => false,
        }
    }

    /// Unify two record rows (supports row polymorphism)
    pub fn unify_rows(
        &mut self,
        row1: thir::RecordRow,
        row2: thir::RecordRow,
        loc: Loc,
    ) -> Result<Subst, CompileError> {
        use thir::RecordRow::*;

        match (row1, row2) {
            // Both empty
            (Empty, Empty) => Ok(Subst::default()),

            // Same field at head
            (
                Extend {
                    field: f1,
                    ty: t1,
                    rest: r1,
                },
                Extend {
                    field: f2,
                    ty: t2,
                    rest: r2,
                },
            ) if f1 == f2 => {
                // Unify field types
                let s1 = self.unify(t1, t2, loc.clone())?;

                // Apply s1 to rest rows before unifying
                let r1_applied = self.apply_row_subst(&s1, *r1);
                let r2_applied = self.apply_row_subst(&s1, *r2);

                // Unify rest
                let s2 = self.unify_rows(r1_applied, r2_applied, loc)?;

                Ok(s1.compose(&s2, &mut self.target))
            }

            // Different fields at head - reorder
            (
                Extend {
                    field: f1,
                    ty: t1,
                    rest: r1,
                },
                row2 @ Extend { .. },
            ) => {
                // Extract f1 from row2
                if let Some((t2, rest2)) = self.extract_field(f1, row2) {
                    // Unify types of f1
                    let s1 = self.unify(t1, t2, loc.clone())?;

                    // Unify rest
                    let r1_applied = self.apply_row_subst(&s1, *r1);
                    let rest2_applied = self.apply_row_subst(&s1, rest2);
                    let s2 = self.unify_rows(r1_applied, rest2_applied, loc)?;

                    Ok(s1.compose(&s2, &mut self.target))
                } else {
                    Err(CompileError::new(
                        CompileErrorKind::RecordFieldMismatch,
                        loc,
                    )
                    .with_context("Record types have incompatible fields"))
                }
            }

            // Row variable unification
            (Var(v1), Var(v2)) if v1 == v2 => Ok(Subst::default()),
            (Var(v), row) | (row, Var(v)) => {
                // Occurs check on row
                if self.occurs_in_row(v, &row) {
                    return Err(CompileError::new(
                        CompileErrorKind::InfiniteType(crate::error::TypeVar(v.0)),
                        loc,
                    ));
                }

                // Bind row variable
                let row_ty = self.target.types.alloc(thir::Type {
                    loc: loc.clone(),
                    kind: thir::TypeKind::Record(row),
                });

                let mut subst = Subst::default();
                subst.insert(v, row_ty);
                Ok(subst)
            }

            // Mismatch
            _ => Err(CompileError::new(
                CompileErrorKind::RecordSizeMismatch,
                loc,
            )),
        }
    }

    /// Extract a field from a row, returning its type and the remaining row
    pub fn extract_field(
        &self,
        field: Symbol,
        row: thir::RecordRow,
    ) -> Option<(thir::TypeId, thir::RecordRow)> {
        match row {
            thir::RecordRow::Empty => None,
            thir::RecordRow::Extend {
                field: f,
                ty,
                rest,
            } => {
                if f == field {
                    Some((ty, *rest))
                } else {
                    // Recursively extract from rest
                    self.extract_field(field, *rest).map(|(ty_found, new_rest)| {
                        (
                            ty_found,
                            thir::RecordRow::Extend {
                                field: f,
                                ty,
                                rest: Box::new(new_rest),
                            },
                        )
                    })
                }
            }
            thir::RecordRow::Var(_) => None,
        }
    }

    /// Check if a type variable occurs in a record row
    pub fn occurs_in_row(&self, var: thir::TypeVar, row: &thir::RecordRow) -> bool {
        match row {
            thir::RecordRow::Empty => false,
            thir::RecordRow::Extend { ty, rest, .. } => {
                self.occurs_in(var, *ty) || self.occurs_in_row(var, rest)
            }
            thir::RecordRow::Var(v) => *v == var,
        }
    }

    /// Apply substitution to a record row
    pub fn apply_row_subst(&mut self, subst: &Subst, row: thir::RecordRow) -> thir::RecordRow {
        match row {
            thir::RecordRow::Empty => thir::RecordRow::Empty,
            thir::RecordRow::Extend { field, ty, rest } => {
                let ty = subst.apply(ty, &mut self.target);
                let rest = Box::new(self.apply_row_subst(subst, *rest));
                thir::RecordRow::Extend { field, ty, rest }
            }
            thir::RecordRow::Var(v) => {
                // Check if v is bound in subst
                if let Some(&bound_ty) = subst.map.get(&v) {
                    // Extract the row from the type
                    let bound = self.target.types.get(bound_ty);
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
