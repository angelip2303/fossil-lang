//! Type checking and inference module
//!
//! This module implements Hindley-Milner type inference (Algorithm W) with row polymorphism
//! to perform type checking on the IR (Intermediate Representation).
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
//!
//! ## Module Organization
//!
//! - `subst`: Type substitution (mapping from type variables to types)
//! - `env`: Type environment for tracking bindings with polytypes
//! - `unify`: Type unification including row polymorphism for records
//! - `infer`: Type inference (Algorithm W)
//! - `helpers`: Helper functions for type construction

use std::collections::HashMap;

use crate::context::{DefId, Symbol};
use crate::error::{CompileError, CompileErrorKind, CompileErrors};
use crate::ir::{
    ExprId, Ident, Ir, Polytype, StmtId, StmtKind, Type, TypeId, TypeKind, TypeRef, TypeVar,
};
use crate::passes::{GlobalContext, IrProgram};

// Sub-modules
pub mod env;
pub mod helpers;
pub mod infer;
pub mod subst;
pub mod unify;

// Re-exports
pub use env::TypeEnv;
pub use subst::Subst;

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
    pub fn fresh(&mut self) -> TypeVar {
        let var = TypeVar(self.counter);
        self.counter += 1;
        var
    }
}

/// Type checker implementing Algorithm W with row polymorphism
///
/// The type checker maintains several pieces of state:
/// - **ir**: The IR being type-checked (modified in-place)
/// - **gcx**: Global context with definitions and interner
/// - **tvg**: Generator for fresh type variables
/// - **env**: Type environment mapping definitions to polytypes
/// - **global_subst**: Accumulated substitution from all inference operations
pub struct TypeChecker {
    pub(crate) ir: Ir,
    pub(crate) gcx: GlobalContext,
    pub(crate) tvg: TypeVarGen,
    pub(crate) env: TypeEnv,
    /// Cache for inferred types: maps ExprId to TypeId
    infer_cache: HashMap<ExprId, TypeId>,
    /// Global substitution accumulated during type inference
    global_subst: Subst,
    /// Local substitution for the current compound expression being inferred.
    local_subst: Subst,
    /// The type of `self` when inside an impl block method.
    self_type: Option<TypeId>,
    /// Cached trait method types: (trait_def_id, method_name) -> TypeId
    trait_method_types: HashMap<(DefId, Symbol), TypeId>,
    /// Deferred trait constraints
    trait_constraints: Vec<(TypeId, DefId, crate::ast::Loc)>,
}

impl TypeChecker {
    /// Create a new type checker from an IR
    pub fn new(ir: Ir, gcx: GlobalContext) -> Self {
        let mut checker = Self {
            ir,
            gcx,
            tvg: TypeVarGen::default(),
            env: TypeEnv::default(),
            infer_cache: HashMap::new(),
            global_subst: Subst::default(),
            local_subst: Subst::default(),
            self_type: None,
            trait_method_types: HashMap::new(),
            trait_constraints: Vec::new(),
        };

        // Initialize environment with registered functions
        checker.init_builtin_functions();

        checker
    }

    /// Initialize the type environment with registered builtin functions
    fn init_builtin_functions(&mut self) {
        use crate::context::DefKind;

        // Iterate through all definitions to find registered functions
        for def_id in 0..self.gcx.definitions.len() {
            let def_id = DefId::new(def_id as u32);
            let def = self.gcx.definitions.get(def_id);

            if let DefKind::Func(Some(func_impl)) = &def.kind {
                // Get the function's signature
                let mut next_var = || self.tvg.fresh();
                let polytype = func_impl.signature(&mut self.ir, &mut next_var, &self.gcx);

                // Add to type environment
                self.env.insert(def_id, polytype);
            }
        }

        // Initialize record constructors
        self.init_record_constructors();
    }

    /// Initialize record constructor function signatures
    ///
    /// For each type definition that is a record type, we generate a constructor
    /// function signature: `(field1: T1, field2: T2, ...) -> RecordType`
    fn init_record_constructors(&mut self) {
        use crate::context::DefKind;

        // Collect all Type definitions from root statements
        let type_stmts: Vec<(StmtId, crate::context::Symbol, TypeId)> = self
            .ir
            .root
            .iter()
            .filter_map(|&stmt_id| {
                let stmt = self.ir.stmts.get(stmt_id);
                if let StmtKind::Type { name, ty } = &stmt.kind {
                    Some((stmt_id, *name, *ty))
                } else {
                    None
                }
            })
            .collect();

        for (_stmt_id, type_name, type_id) in type_stmts {
            let ty = self.ir.types.get(type_id);
            let loc = ty.loc.clone();

            // Only process record types
            if let TypeKind::Record(row) = &ty.kind {
                // Check if there's a constructor registered for this type
                let ctor_def_id = self
                    .gcx
                    .definitions
                    .iter()
                    .find(|def| def.name == type_name && matches!(def.kind, DefKind::Func(None)))
                    .map(|def| def.id());

                if let Some(ctor_def_id) = ctor_def_id {
                    // Get the type's DefId
                    let type_def_id = self
                        .gcx
                        .definitions
                        .iter()
                        .find(|def| def.name == type_name && matches!(def.kind, DefKind::Type))
                        .map(|def| def.id());

                    if let Some(type_def_id) = type_def_id {
                        // Collect field types from the record row
                        let mut param_types = Vec::new();
                        let mut current_row = row.clone();
                        loop {
                            match current_row {
                                crate::ir::RecordRow::Empty => break,
                                crate::ir::RecordRow::Var(_) => break,
                                crate::ir::RecordRow::Extend { ty, rest, .. } => {
                                    param_types.push(ty);
                                    current_row = *rest;
                                }
                            }
                        }

                        // Create the return type: Named(type_def_id)
                        let return_ty = self.ir.types.alloc(Type {
                            loc: loc.clone(),
                            kind: TypeKind::Named(Ident::Resolved(type_def_id)),
                        });

                        // Create function type: (T1, T2, ...) -> RecordType
                        let fn_ty = self.ir.types.alloc(Type {
                            loc: loc.clone(),
                            kind: TypeKind::Function(param_types, return_ty),
                        });

                        // Add to type environment as monomorphic
                        self.env.insert(ctor_def_id, Polytype::mono(fn_ty));
                    }
                }
            }
        }
    }

    /// Run type checking on the entire program
    ///
    /// Returns the typed IR program or errors.
    pub fn check(mut self) -> Result<IrProgram, CompileErrors> {
        let root_ids = self.ir.root.clone();
        let mut errors = CompileErrors::new();

        for stmt_id in root_ids {
            if let Err(e) = self.check_stmt(stmt_id) {
                errors.push(e);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        // Verify deferred trait constraints
        let constraint_errors = self.verify_trait_constraints();
        if !constraint_errors.is_empty() {
            let mut errors = CompileErrors::new();
            for err in constraint_errors {
                errors.push(err);
            }
            return Err(errors);
        }

        Ok(IrProgram {
            ir: self.ir,
            gcx: self.gcx,
            env: self.env,
        })
    }

    /// Verify all deferred trait constraints
    fn verify_trait_constraints(&self) -> Vec<CompileError> {
        let mut errors = Vec::new();

        for &(ty_id, trait_def_id, ref loc) in &self.trait_constraints {
            if let Some(err) = self.check_trait_impl(ty_id, trait_def_id, loc.clone()) {
                errors.push(err);
            }
        }

        errors
    }

    /// Check if a resolved type implements a given trait
    fn check_trait_impl(
        &self,
        ty_id: TypeId,
        trait_def_id: DefId,
        loc: crate::ast::Loc,
    ) -> Option<CompileError> {
        let trait_name = self
            .gcx
            .interner
            .resolve(self.gcx.definitions.get(trait_def_id).name)
            .to_string();

        let ty = self.ir.types.get(ty_id);
        match &ty.kind {
            // Primitives have builtin trait impls
            TypeKind::Primitive(_) => None,

            // Named types - check trait_impls registry
            TypeKind::Named(Ident::Resolved(def_id)) => {
                if self.gcx.trait_impls.contains_key(&(trait_def_id, *def_id)) {
                    return None;
                }
                let type_name = self
                    .gcx
                    .interner
                    .resolve(self.gcx.definitions.get(*def_id).name)
                    .to_string();

                Some(
                    CompileError::new(
                        CompileErrorKind::Runtime(format!(
                            "Type '{}' does not implement '{}'",
                            type_name, trait_name
                        )),
                        loc,
                    )
                    .with_context(format!(
                        "Add an implementation: impl {} for {} {{ ... }}",
                        trait_name, type_name
                    )),
                )
            }

            // Unresolved type variables
            TypeKind::Var(_) => Some(
                CompileError::new(
                    CompileErrorKind::Runtime(format!(
                        "Cannot verify '{}' for unresolved type",
                        trait_name
                    )),
                    loc,
                )
                .with_context("Add an explicit type annotation to help type inference."),
            ),

            _ => Some(
                CompileError::new(
                    CompileErrorKind::Runtime(format!(
                        "This type does not implement '{}'",
                        trait_name
                    )),
                    loc,
                )
                .with_context(format!("Add 'impl {}' for this type.", trait_name)),
            ),
        }
    }

    /// Type-check a single statement
    fn check_stmt(&mut self, stmt_id: StmtId) -> Result<(), CompileError> {
        let stmt = self.ir.stmts.get(stmt_id);
        let stmt_kind = stmt.kind.clone();
        let loc = stmt.loc.clone();

        match stmt_kind {
            StmtKind::Let {
                name: _,
                def_id,
                ty: type_annotation,
                value,
            } => {
                // Infer type of the value
                let (mut subst, inferred_ty) = self.infer(value)?;
                let inferred_ty = subst.apply(inferred_ty, &mut self.ir);

                // If there's a type annotation, unify with it
                let final_ty = if let Some(annotation_ty) = type_annotation {
                    let s = self.unify(annotation_ty, inferred_ty, loc.clone())?;
                    subst = subst.compose(&s, &mut self.ir);
                    subst.apply(annotation_ty, &mut self.ir)
                } else {
                    inferred_ty
                };

                // Accumulate substitution
                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                // Generalize and bind to environment
                if let Some(def_id) = def_id {
                    let poly = self.env.generalize(final_ty, &self.ir);
                    self.env.insert(def_id, poly);
                }

                // Update expression's type
                self.ir.exprs.get_mut(value).ty = TypeRef::Known(final_ty);
            }

            StmtKind::Const {
                name: _,
                def_id,
                value,
            } => {
                // Infer type of the value
                let (subst, inferred_ty) = self.infer(value)?;

                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                // Const bindings: generalize the type
                if let Some(def_id) = def_id {
                    let poly = self.env.generalize(inferred_ty, &self.ir);
                    self.env.insert(def_id, poly);
                }

                self.ir.exprs.get_mut(value).ty = TypeRef::Known(inferred_ty);
            }

            StmtKind::Type { name: _, ty: _ } => {
                // Type definitions are already in the IR, nothing to do
            }

            StmtKind::Trait {
                name: _,
                def_id,
                methods,
            } => {
                // Store trait method types for validation
                if let Some(trait_def_id) = def_id {
                    for method in &methods {
                        self.trait_method_types
                            .insert((trait_def_id, method.name), method.ty);
                    }
                }
            }

            StmtKind::Impl {
                trait_name,
                type_name,
                methods,
            } => {
                // Get the trait and type DefIds
                let trait_def_id = match trait_name {
                    Ident::Resolved(id) => id,
                    _ => return Ok(()), // Unresolved, skip
                };
                let type_def_id = match type_name {
                    Ident::Resolved(id) => id,
                    _ => return Ok(()),
                };

                // Set self_type for method type checking
                let self_ty = self.ir.types.alloc(Type {
                    loc: loc.clone(),
                    kind: TypeKind::Named(Ident::Resolved(type_def_id)),
                });
                self.self_type = Some(self_ty);

                // Type check methods
                for (method_name, method_expr) in &methods {
                    let (subst, ty) = self.infer(*method_expr)?;
                    self.global_subst = self.global_subst.compose(&subst, &mut self.ir);

                    // Validate against trait method signature if available
                    if let Some(&expected_ty) =
                        self.trait_method_types.get(&(trait_def_id, *method_name))
                    {
                        if let Err(e) = self.unify(ty, expected_ty, loc.clone()) {
                            self.self_type = None;
                            return Err(e);
                        }
                    }

                    self.ir.exprs.get_mut(*method_expr).ty = TypeRef::Known(ty);
                }

                self.self_type = None;
            }

            StmtKind::Expr(expr) => {
                let (subst, ty) = self.infer(expr)?;
                self.global_subst = self.global_subst.compose(&subst, &mut self.ir);
                self.ir.exprs.get_mut(expr).ty = TypeRef::Known(ty);
            }
        }

        Ok(())
    }

    /// Instantiate a polytype with fresh type variables
    pub fn instantiate(&mut self, poly: &Polytype) -> TypeId {
        if poly.forall.is_empty() {
            // Re-add any constraints
            for constraint in &poly.constraints {
                self.trait_constraints.push((
                    constraint.ty,
                    constraint.trait_def_id,
                    constraint.loc.clone(),
                ));
            }
            return poly.ty;
        }

        // Create substitution mapping quantified vars to fresh vars
        let mut subst = Subst::default();
        for &old_var in &poly.forall {
            let fresh_ty = self.fresh_type_var_generated();
            subst.insert(old_var, fresh_ty);
        }

        // Apply substitution to constraints
        for constraint in &poly.constraints {
            let instantiated_ty = subst.apply(constraint.ty, &mut self.ir);
            self.trait_constraints.push((
                instantiated_ty,
                constraint.trait_def_id,
                constraint.loc.clone(),
            ));
        }

        subst.apply(poly.ty, &mut self.ir)
    }
}
