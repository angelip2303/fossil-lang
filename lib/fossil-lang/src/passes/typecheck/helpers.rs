//! Helper functions for type construction
//!
//! This module provides convenient helpers to reduce boilerplate
//! when creating types in the THIR arena.

use crate::ast::ast::PrimitiveType;
use crate::ast::{Loc, thir};

use super::TypeChecker;

impl TypeChecker {
    /// Create a fresh type variable with the given location
    pub fn fresh_type_var(&mut self, loc: Loc) -> thir::TypeId {
        let var = self.tvg.fresh();
        self.target.types.alloc(thir::Type {
            loc,
            kind: thir::TypeKind::Var(var),
        })
    }

    /// Create a fresh type variable with a generated (empty) location
    pub fn fresh_type_var_generated(&mut self) -> thir::TypeId {
        let var = self.tvg.fresh();
        self.target.types.alloc(thir::Type {
            loc: Loc::generated(),
            kind: thir::TypeKind::Var(var),
        })
    }

    /// Create a primitive type with the given location
    pub fn primitive_type(&mut self, prim: PrimitiveType, loc: Loc) -> thir::TypeId {
        self.target.types.alloc(thir::Type {
            loc,
            kind: thir::TypeKind::Primitive(prim),
        })
    }

    /// Create a list type with the given element type and location
    ///
    /// Creates a `List<T>` type using the App representation:
    /// `App { ctor: List, args: [elem_ty] }`
    pub fn list_type(&mut self, elem_ty: thir::TypeId, loc: Loc) -> thir::TypeId {
        let list_ctor = self.gcx.list_type_ctor
            .expect("SAFETY: List type constructor is registered in GlobalContext::new() as a builtin type. \
                     It is guaranteed to be present in all GlobalContext instances.");

        self.target.types.alloc(thir::Type {
            loc,
            kind: thir::TypeKind::App {
                ctor: list_ctor,
                args: vec![elem_ty],
            },
        })
    }

    /// Create a record type with the given row and location
    pub fn record_type(&mut self, row: thir::RecordRow, loc: Loc) -> thir::TypeId {
        self.target.types.alloc(thir::Type {
            loc,
            kind: thir::TypeKind::Record(row),
        })
    }

    /// Create a function type with the given parameters, return type, and location
    pub fn function_type(
        &mut self,
        params: Vec<thir::TypeId>,
        ret: thir::TypeId,
        loc: Loc,
    ) -> thir::TypeId {
        self.target.types.alloc(thir::Type {
            loc,
            kind: thir::TypeKind::Function(params, ret),
        })
    }

    /// Format a type in a human-readable way for error messages
    pub fn format_type(&self, ty_id: thir::TypeId) -> String {
        let ty = self.target.types.get(ty_id);
        match &ty.kind {
            thir::TypeKind::Primitive(p) => format!("{:?}", p),
            thir::TypeKind::Var(v) => format!("'{}", v.0),
            thir::TypeKind::Named(def_id) => {
                let def = self.gcx.definitions.get(*def_id);
                self.gcx.interner.resolve(def.name).to_string()
            }
            thir::TypeKind::App { ctor, args } => {
                let ctor_def = self.gcx.definitions.get(*ctor);
                let ctor_name = self.gcx.interner.resolve(ctor_def.name);
                if args.is_empty() {
                    ctor_name.to_string()
                } else {
                    let arg_strs: Vec<_> = args.iter()
                        .map(|arg| self.format_type(*arg))
                        .collect();
                    format!("{}<{}>", ctor_name, arg_strs.join(", "))
                }
            }
            thir::TypeKind::Function(params, ret) => {
                let param_strs: Vec<_> = params.iter()
                    .map(|p| self.format_type(*p))
                    .collect();
                format!("fn({}) -> {}", param_strs.join(", "), self.format_type(*ret))
            }
            thir::TypeKind::Record(row) => {
                format!("{{{}}}", self.format_row(row))
            }
            thir::TypeKind::FieldSelector { record_ty, field_ty, field } => {
                let field_name = self.gcx.interner.resolve(*field);
                format!(
                    "FieldSelector<{}, {} as {}>",
                    self.format_type(*record_ty),
                    field_name,
                    self.format_type(*field_ty)
                )
            }
        }
    }

    /// Format a record row for error messages
    fn format_row(&self, row: &thir::RecordRow) -> String {
        match row {
            thir::RecordRow::Empty => String::new(),
            thir::RecordRow::Var(v) => format!("...'r{}", v.0),
            thir::RecordRow::Extend { field, ty, rest } => {
                let field_name = self.gcx.interner.resolve(*field);
                let ty_str = self.format_type(*ty);
                let rest_str = self.format_row(rest);
                if rest_str.is_empty() {
                    format!("{}: {}", field_name, ty_str)
                } else {
                    format!("{}: {}, {}", field_name, ty_str, rest_str)
                }
            }
        }
    }
}
