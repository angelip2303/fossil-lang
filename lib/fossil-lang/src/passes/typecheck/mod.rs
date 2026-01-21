//! Type checking and inference module
//!
//! This module implements Hindley-Milner type inference (Algorithm W) with row polymorphism
//! to convert HIR (High-level IR with resolved names) to THIR (Typed HIR).
//!
//! ## Type System Features
//!
//! - **Hindley-Milner Type Inference**: Automatic type inference with let-polymorphism
//! - **Row Polymorphism**: Extensible records supporting partial field access
//! - **Principal Types**: The most general type is always inferred
//! - **Type Variables**: Fresh type variables generated during inference
//!
//! ## Type Checking Process
//!
//! 1. **Inference** (`infer`): Apply Algorithm W to infer types for expressions
//! 2. **Unification** (`unify`): Solve type constraints and check compatibility
//! 3. **Substitution** (`subst`): Apply solved constraints throughout the program
//! 4. **Generalization**: Convert monotypes to polytypes for let-bindings
//! 5. **Instantiation**: Create fresh instances of polytypes for use sites
//! 6. **Conversion** (`convert`): Transform HIR to THIR with type annotations
//!
//! ## Module Organization
//!
//! - `subst`: Type substitution (mapping from type variables to types)
//! - `env`: Type environment for tracking bindings with polytypes
//! - `unify`: Type unification including row polymorphism for records
//! - `infer`: Type inference (Algorithm W)
//! - `convert`: HIR to THIR conversion
//! - `helpers`: Helper functions for type construction
//!
//! ## Example Flow
//!
//! ```text
//! let id = fn (x) -> x       // User code
//!     ↓ Parse + Resolve
//! HIR: Let("id", Function)   // With resolved names
//!     ↓ Type Inference
//! THIR: Let("id", fn('a) -> 'a : ∀a. a → a)  // With inferred types
//! ```
//!
//! ## References
//!
//! - Algorithm W: Damas & Milner (1982) "Principal type-schemes for functional programs"
//! - Row Polymorphism: Leijen (2005) "Extensible records with scoped labels"

use std::collections::HashMap;

use crate::ast::{hir, thir};
use crate::context::DefId;
use crate::error::{CompileError, CompileErrors};
use crate::passes::{GlobalContext, HirProgram, ThirProgram};
use crate::passes::resolve::table::ResolutionTable;

// Sub-modules
pub mod subst;
pub mod env;
pub mod unify;
pub mod infer;
pub mod convert;
pub mod helpers;

// Re-exports
pub use subst::Subst;
pub use env::TypeEnv;

/// Type variable generator
///
/// Generates fresh type variables during type inference.
/// Each variable is assigned a unique incrementing ID.
#[derive(Default)]
pub struct TypeVarGen {
    counter: usize,
}

impl TypeVarGen {
    /// Generate a fresh type variable
    ///
    /// Returns a new type variable with a unique ID.
    pub fn fresh(&mut self) -> thir::TypeVar {
        let var = thir::TypeVar(self.counter);
        self.counter += 1;
        var
    }
}

/// Type checker implementing Algorithm W with row polymorphism
///
/// The type checker maintains several pieces of state:
/// - **source**: The input HIR being type-checked
/// - **target**: The output THIR being constructed
/// - **gcx**: Global context with definitions and interner
/// - **tvg**: Generator for fresh type variables
/// - **env**: Type environment mapping definitions to polytypes
/// - **expr_cache**: Memoization of HIR → THIR expression conversions
/// - **type_cache**: Memoization of HIR → THIR type conversions
/// - **global_subst**: Accumulated substitution from all inference operations
///
/// ## Usage
///
/// ```rust,ignore
/// let checker = TypeChecker::new(hir_program);
/// let thir_program = checker.check()?;
/// ```
pub struct TypeChecker {
    source: hir::Hir,
    target: thir::TypedHir,
    gcx: GlobalContext,
    tvg: TypeVarGen,
    env: TypeEnv,
    expr_cache: HashMap<hir::ExprId, thir::ExprId>,
    type_cache: HashMap<hir::TypeId, thir::TypeId>,
    /// Cache for inferred types: maps HIR ExprId to (Subst, TypeId)
    infer_cache: HashMap<hir::ExprId, thir::TypeId>,
    resolutions: ResolutionTable,
    /// Global substitution accumulated during type inference
    global_subst: Subst,
    /// Local substitution for the current compound expression being inferred.
    /// This is used to propagate constraints between subexpressions (e.g.,
    /// multiple field accesses on the same variable within a function call).
    /// It gets merged into global_subst when the expression inference completes.
    local_subst: Subst,
}

impl TypeChecker {
    /// Create a new type checker
    ///
    /// # Arguments
    /// * `hir_program` - The HIR program to type-check
    ///
    /// # Returns
    /// A new TypeChecker ready to perform type inference
    pub fn new(hir_program: HirProgram) -> Self {
        let mut checker = Self {
            source: hir_program.hir,
            target: thir::TypedHir::default(),
            gcx: hir_program.gcx,
            tvg: TypeVarGen::default(),
            env: TypeEnv::default(),
            expr_cache: HashMap::new(),
            type_cache: HashMap::new(),
            infer_cache: HashMap::new(),
            resolutions: hir_program.resolutions,
            global_subst: Subst::default(),
            local_subst: Subst::default(),
        };

        // Initialize environment with registered functions
        checker.init_builtin_functions();

        checker
    }

    /// Initialize the type environment with registered builtin functions
    ///
    /// This method populates the type environment with any functions that have been
    /// registered via GlobalContext::register_function(), ensuring they have their
    /// types available during type checking.
    fn init_builtin_functions(&mut self) {
        use crate::context::DefKind;

        // Iterate through all definitions to find registered functions
        for def_id in 0..self.gcx.definitions.len() {
            let def_id = crate::context::DefId::new(def_id as u32);
            let def = self.gcx.definitions.get(def_id);

            if let DefKind::Func(Some(func_impl)) = &def.kind {
                // Get the function's signature
                let mut next_var = || self.tvg.fresh();
                let polytype = func_impl.signature(&mut self.target, &mut next_var, &self.gcx);

                // Add to type environment
                self.env.insert(def_id, polytype);
            }
        }

        // Initialize record constructors
        self.init_record_constructors();
    }

    /// Initialize record constructor function signatures
    ///
    /// Creates type signatures for auto-generated record constructors
    /// based on the field types from the AST.
    fn init_record_constructors(&mut self) {
        // Get all constructor DefIds from resolutions
        let constructors: Vec<(DefId, DefId)> = self.resolutions
            .record_constructors
            .iter()
            .map(|(ctor_def_id, type_def_id)| (*ctor_def_id, *type_def_id))
            .collect();

        for (constructor_def_id, type_def_id) in constructors {
            // Get field names from resolutions
            let field_names = match self.resolutions.constructor_fields.get(&constructor_def_id) {
                Some(names) => names.clone(),
                None => continue, // No fields info, skip
            };

            // Find the type definition in source HIR and convert to THIR types
            if let Some(field_types) = self.get_record_field_types_from_hir(type_def_id, &field_names) {
                // Create function type: (T1, T2, ..., Tn) -> RecordType
                let return_type = self.target.types.alloc(thir::Type {
                    loc: crate::ast::Loc::generated(),
                    kind: thir::TypeKind::Named(type_def_id),
                });

                let fn_type = self.target.types.alloc(thir::Type {
                    loc: crate::ast::Loc::generated(),
                    kind: thir::TypeKind::Function(field_types.clone(), return_type),
                });

                // Create monomorphic polytype
                let polytype = thir::Polytype::mono(fn_type);

                // Add to type environment
                self.env.insert(constructor_def_id, polytype);
            }
        }
    }

    /// Get field types for a record type from the source HIR
    ///
    /// This looks up the record type definition in the source HIR and converts
    /// the field types to THIR types.
    fn get_record_field_types_from_hir(
        &mut self,
        type_def_id: DefId,
        _field_names: &[crate::context::Symbol],
    ) -> Option<Vec<thir::TypeId>> {
        use hir::StmtKind;

        // Get the symbol name for this type
        let type_def = self.gcx.definitions.get(type_def_id);
        let type_name = type_def.name;

        // Search for the type definition in source HIR statements
        for stmt_id in &self.source.root {
            let stmt = self.source.stmts.get(*stmt_id);

            if let StmtKind::Type { name, ty } = &stmt.kind
                && *name == type_name {
                    // Found the type definition, check if it's a record
                    let ty_data = self.source.types.get(*ty);

                    if let hir::TypeKind::Record(fields) = &ty_data.kind {
                        // Clone field type IDs to avoid borrow checker issues
                        let field_ty_ids: Vec<hir::TypeId> = fields.iter().map(|f| f.ty).collect();

                        // Extract field types and convert to THIR
                        let mut thir_field_types = Vec::new();

                        for field_ty_id in field_ty_ids {
                            // Use fold_type_to_thir which handles caching and conversion
                            if let Ok(thir_ty_id) = self.fold_type_to_thir(field_ty_id) {
                                thir_field_types.push(thir_ty_id);
                            } else {
                                // If conversion fails, skip this constructor
                                return None;
                            }
                        }

                        return Some(thir_field_types);
                    }
                }
        }

        None
    }

    /// Run type checking on the entire program
    ///
    /// This is the main entry point for type checking. It processes all statements
    /// in the program, performing type inference and building the THIR.
    ///
    /// # Returns
    /// - `Ok(ThirProgram)` - Successfully typed program
    /// - `Err(CompileError)` - Type error with location and context
    ///
    /// # Errors
    /// Returns an error if type checking fails (type mismatch, undefined variable, etc.)
    pub fn check(mut self) -> Result<ThirProgram, CompileErrors> {
        // Process root statements (block statements are checked when their block is checked)
        let root_ids = self.source.root.clone();
        let mut thir_root = Vec::new();
        let mut errors = CompileErrors::new();

        for stmt_id in root_ids {
            match self.check_stmt(stmt_id) {
                Ok(thir_stmt_id) => thir_root.push(thir_stmt_id),
                Err(e) => errors.push(e),
            }
        }

        // Return errors if any occurred
        if !errors.is_empty() {
            return Err(errors);
        }

        self.target.root = thir_root;

        // Instantiate record constructors now that we have complete type information
        self.instantiate_record_constructors();

        // Zonk all types: apply global substitution to resolve all type variables
        self.zonk_all_types();

        Ok(ThirProgram {
            thir: self.target,
            gcx: self.gcx,
        })
    }

    /// Zonk all types in the THIR
    ///
    /// This method applies the global substitution to all types in the THIR,
    /// ensuring that type variables are resolved to their final concrete types.
    /// This is essential for IDE features like hover to show resolved types.
    fn zonk_all_types(&mut self) {
        // Collect all expression IDs and their old types first to avoid borrowing issues
        let expr_types: Vec<(thir::ExprId, thir::TypeId)> = self.target.exprs
            .iter()
            .map(|(id, expr)| (id, expr.ty))
            .collect();

        // Apply substitution to each expression's type
        for (expr_id, old_ty) in expr_types {
            let new_ty = self.global_subst.apply(old_ty, &mut self.target);

            if new_ty != old_ty {
                // Update the expression with the resolved type
                self.target.exprs.get_mut(expr_id).ty = new_ty;
            }
        }

        // Also zonk types in type statements
        let stmt_types: Vec<(thir::StmtId, thir::TypeId)> = self.target.root
            .iter()
            .filter_map(|stmt_id| {
                let stmt = self.target.stmts.get(*stmt_id);
                if let thir::StmtKind::Type { ty, .. } = &stmt.kind {
                    Some((*stmt_id, *ty))
                } else {
                    None
                }
            })
            .collect();

        for (stmt_id, old_ty) in stmt_types {
            let new_ty = self.global_subst.apply(old_ty, &mut self.target);

            if new_ty != old_ty {
                let stmt = self.target.stmts.get_mut(stmt_id);
                if let thir::StmtKind::Type { ty, .. } = &mut stmt.kind {
                    *ty = new_ty;
                }
            }
        }

        // Zonk types in function parameters
        // Collect function expressions and their type info
        let func_info: Vec<(thir::ExprId, thir::TypeId, Vec<thir::TypeId>, thir::TypeId, crate::ast::Loc)> = self.target.exprs
            .iter()
            .filter_map(|(id, expr)| {
                let ty = self.target.types.get(expr.ty);
                if let thir::TypeKind::Function(params, ret) = &ty.kind {
                    Some((id, expr.ty, params.clone(), *ret, ty.loc.clone()))
                } else {
                    None
                }
            })
            .collect();

        for (expr_id, _old_ty_id, old_params, old_ret, loc) in func_info {
            let new_params: Vec<_> = old_params
                .iter()
                .map(|p| self.global_subst.apply(*p, &mut self.target))
                .collect();
            let new_ret = self.global_subst.apply(old_ret, &mut self.target);

            // Check if anything changed
            if new_params != old_params || new_ret != old_ret {
                let new_ty_id = self.target.types.alloc(thir::Type {
                    loc,
                    kind: thir::TypeKind::Function(new_params, new_ret),
                });
                self.target.exprs.get_mut(expr_id).ty = new_ty_id;
            }
        }
    }

    /// Instantiate record constructor functions after type checking
    ///
    /// This method is called after type checking completes to create runtime
    /// implementations for record constructors. It needs to run after type checking
    /// because it requires the THIR type information.
    fn instantiate_record_constructors(&mut self) {
        use crate::runtime::constructor::RecordConstructorFunction;

        // Get all constructor DefIds from resolutions
        let constructors: Vec<(DefId, DefId)> = self.resolutions
            .record_constructors
            .iter()
            .map(|(ctor_def_id, type_def_id)| (*ctor_def_id, *type_def_id))
            .collect();

        for (constructor_def_id, type_def_id) in constructors {
            // Get field names from resolutions
            let field_names = match self.resolutions.constructor_fields.get(&constructor_def_id) {
                Some(names) => names.clone(),
                None => continue, // No fields info, skip
            };

            // Find the type definition in THIR to get field types
            // We need to look through type definitions to find the record type
            let field_types = self.extract_record_field_types(type_def_id);

            if let Some(field_types) = field_types {
                // Create the constructor function implementation
                let constructor_impl = RecordConstructorFunction::boxed(
                    type_def_id,
                    field_names,
                    field_types,
                );

                // Update the definition with the implementation
                self.gcx.definitions.update_function(constructor_def_id, Some(constructor_impl));
            }
        }
    }

    /// Extract field types from a record type definition
    ///
    /// Given a type DefId, looks up the type in THIR and extracts the field types
    /// if it's a record type.
    ///
    /// # Returns
    /// `Some(field_types)` if the type is a record, `None` otherwise
    fn extract_record_field_types(&self, type_def_id: DefId) -> Option<Vec<thir::TypeId>> {
        use thir::TypeKind;

        // Get the symbol name for this type
        let type_def = self.gcx.definitions.get(type_def_id);
        let type_name = type_def.name;

        // Search for the type definition in THIR statements
        for stmt_id in &self.target.root {
            let stmt = self.target.stmts.get(*stmt_id);

            if let thir::StmtKind::Type { name, ty } = &stmt.kind
                && *name == type_name {
                    // Found the type definition, check if it's a record
                    let ty_data = self.target.types.get(*ty);

                    if let TypeKind::Record(row) = &ty_data.kind {
                        // Extract field types from the record row
                        let field_types = self.extract_field_types_from_row(row);
                        return Some(field_types);
                    }
                }
        }

        None
    }

    /// Recursively extract field types from a record row
    fn extract_field_types_from_row(&self, row: &thir::RecordRow) -> Vec<thir::TypeId> {
        let mut field_types = Vec::new();

        let mut current_row = row;
        loop {
            match current_row {
                thir::RecordRow::Empty | thir::RecordRow::Var(_) => break,
                thir::RecordRow::Extend { ty, rest, .. } => {
                    field_types.push(*ty);
                    current_row = rest;
                }
            }
        }

        field_types
    }

    /// Type-check a single statement
    ///
    /// Handles let-bindings, type definitions, imports, and expression statements.
    /// For let-bindings, infers the type, generalizes it, and adds it to the environment.
    fn check_stmt(&mut self, stmt_id: hir::StmtId) -> Result<thir::StmtId, CompileError> {
        // Clone what we need before any mutable operations
        let stmt = self.source.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let loc = stmt.loc.clone();

        let thir_kind = match stmt_kind {
            hir::StmtKind::Import { module, items, alias } => {
                // Imports don't have types, just pass through
                thir::StmtKind::Import {
                    module: module.clone(),
                    items: items.clone(),
                    alias,
                }
            }

            hir::StmtKind::Let { name, def_id, ty: type_annotation, value } => {
                // Infer type of the value
                let (mut subst, inferred_ty) = self.infer(value)?;
                let inferred_ty = subst.apply(inferred_ty, &mut self.target);

                // If there's a type annotation, unify it with the inferred type
                let final_ty = if let Some(annotation_hir_ty) = type_annotation {
                    // Convert annotation to THIR type
                    let annotation_thir_ty = self.fold_type_to_thir(annotation_hir_ty)?;

                    // Unify annotation with inferred type (use the let statement's loc for error)
                    let s = self.unify(annotation_thir_ty, inferred_ty, loc.clone())?;
                    subst = subst.compose(&s, &mut self.target);

                    // Use the annotation type as the final type
                    subst.apply(annotation_thir_ty, &mut self.target)
                } else {
                    inferred_ty
                };

                // Accumulate substitution into global
                self.global_subst = self.global_subst.compose(&subst, &mut self.target);

                // Generalize the type
                let poly = self.env.generalize(final_ty, &self.target);

                // Add the binding to the type environment
                self.env.insert(def_id, poly);

                // Convert the expression to THIR with its inferred type
                let thir_value = self.fold_expr_to_thir(value, final_ty)?;

                thir::StmtKind::Let {
                    name,
                    value: thir_value,
                }
            }

            hir::StmtKind::Type { name, ty } => {
                // Convert the type definition to THIR
                let thir_ty = self.fold_type_to_thir(ty)?;
                thir::StmtKind::Type { name, ty: thir_ty }
            }

            hir::StmtKind::Expr(expr) => {
                // Infer type of the expression
                let (subst, ty) = self.infer(expr)?;
                // Accumulate substitution into global
                self.global_subst = self.global_subst.compose(&subst, &mut self.target);
                let thir_expr = self.fold_expr_to_thir(expr, ty)?;
                thir::StmtKind::Expr(thir_expr)
            }
        };

        Ok(self.target.stmts.alloc(thir::Stmt {
            loc,
            kind: thir_kind,
        }))
    }
}
