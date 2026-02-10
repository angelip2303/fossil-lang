//! Helper functions for type checking
//!
//! Provides helpers for type variable generation and formatting.

use crate::ast::Loc;
use crate::context::DefId;
use crate::error::FossilError;
use crate::ir::{Ident, RecordFields, StmtKind, Type, TypeId, TypeKind};

use super::TypeChecker;

impl TypeChecker {
    /// Create a fresh type variable
    pub fn fresh_type_var(&mut self, loc: Loc) -> TypeId {
        let var = self.tvg.fresh();
        self.ir.types.alloc(Type {
            loc,
            kind: TypeKind::Var(var),
        })
    }

    /// Create a fresh type variable with generated location
    pub fn fresh_type_var_generated(&mut self) -> TypeId {
        self.ir.var_type(self.tvg.fresh())
    }

    /// Format a type in a human-readable way for error messages
    pub fn format_type(&self, ty_id: TypeId) -> String {
        let ty = self.ir.types.get(ty_id);
        match &ty.kind {
            TypeKind::Primitive(p) => format!("{:?}", p),
            TypeKind::Var(v) => format!("'{}", v.0),
            TypeKind::Unit => "()".to_string(),
            TypeKind::Named(ident) => match ident {
                Ident::Resolved(def_id) => {
                    let def = self.gcx.definitions.get(*def_id);
                    self.gcx.interner.resolve(def.name).to_string()
                }
                Ident::Unresolved(path) => format!("{:?}", path),
            },
            TypeKind::App { ctor, args } => {
                let ctor_name = match ctor {
                    Ident::Resolved(def_id) => {
                        let def = self.gcx.definitions.get(*def_id);
                        self.gcx.interner.resolve(def.name).to_string()
                    }
                    Ident::Unresolved(path) => format!("{:?}", path),
                };
                if args.is_empty() {
                    ctor_name
                } else {
                    let arg_strs: Vec<_> = args.iter().map(|arg| self.format_type(*arg)).collect();
                    format!("{}<{}>", ctor_name, arg_strs.join(", "))
                }
            }
            TypeKind::Function(params, ret) => {
                let param_strs: Vec<_> = params.iter().map(|p| self.format_type(*p)).collect();
                format!("fn({}) -> {}", param_strs.join(", "), self.format_type(*ret))
            }
            TypeKind::Record(fields) => {
                format!("{{{}}}", self.format_fields(fields))
            }
            TypeKind::List(elem) => format!("[{}]", self.format_type(*elem)),
            TypeKind::Optional(inner) => format!("{}?", self.format_type(*inner)),
            TypeKind::Provider { .. } => "<provider>".to_string(),
        }
    }

    /// Format record fields for error messages
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

    /// Extract the element type from a list type
    ///
    /// Returns the element type T if the given type is [T],
    /// or an error if it's not a list type.
    pub fn extract_list_element_type(
        &mut self,
        list_ty: TypeId,
        loc: Loc,
    ) -> Result<TypeId, FossilError> {
        let ty = self.ir.types.get(list_ty);

        match &ty.kind {
            TypeKind::List(elem_ty) => Ok(*elem_ty),

            TypeKind::Var(_) => {
                let elem_ty = self.fresh_type_var(loc);
                let expected_list = self.ir.list_type(elem_ty);
                self.unify(list_ty, expected_list, loc)?;
                Ok(elem_ty)
            }

            // Record/Named types are valid sources (lazy data)
            TypeKind::Record(_) | TypeKind::Named(_) => Ok(self.fresh_type_var(loc)),

            _ => {
                Err(FossilError::type_mismatch(
                    format!("expected [T] or Records, got {}", self.format_type(list_ty)),
                    loc,
                ))
            }
        }
    }

    /// Get the number of constructor parameters for a type
    pub fn get_type_ctor_param_count(&self, type_def_id: DefId) -> usize {
        let type_def = self.gcx.definitions.get(type_def_id);

        // Look for the type statement in the IR
        for &stmt_id in &self.ir.root {
            let stmt = self.ir.stmts.get(stmt_id);
            if let StmtKind::Type {
                name, ctor_params, ..
            } = &stmt.kind
            {
                if *name == type_def.name {
                    return ctor_params.len();
                }
            }
        }

        0
    }

    /// Check that constructor argument count matches type parameters
    pub fn check_ctor_arg_count(
        &self,
        type_def_id: DefId,
        actual_args: usize,
        loc: Loc,
    ) -> Result<(), FossilError> {
        let expected_params = self.get_type_ctor_param_count(type_def_id);

        if actual_args != expected_params {
            return Err(FossilError::arity_mismatch(expected_params, actual_args, loc));
        }

        Ok(())
    }
}
