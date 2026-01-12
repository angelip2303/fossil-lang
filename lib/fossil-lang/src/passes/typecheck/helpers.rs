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
            .expect("List type constructor should be registered in GlobalContext");

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
}
