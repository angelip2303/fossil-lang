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
use crate::error::{CompileError, CompileErrors};
use crate::passes::{GlobalContext, HirProgram, ThirProgram};

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
                let polytype = func_impl.signature(&mut self.target, &mut next_var);

                // Add to type environment
                self.env.insert(def_id, polytype);
            }
        }
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

        Ok(ThirProgram {
            thir: self.target,
            gcx: self.gcx,
        })
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
            hir::StmtKind::Import { module, alias } => {
                // Imports don't have types, just pass through
                thir::StmtKind::Import {
                    module: module.clone(),
                    alias,
                }
            }

            hir::StmtKind::Let { name, def_id, value } => {
                // Infer type of the value
                let (subst, ty) = self.infer(value)?;
                let ty = subst.apply(ty, &mut self.target);

                // Generalize the type
                let poly = self.env.generalize(ty, &self.target);

                // Add the binding to the type environment
                self.env.insert(def_id, poly);

                // Convert the expression to THIR with its inferred type
                let thir_value = self.fold_expr_to_thir(value, ty)?;

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
                let (_, ty) = self.infer(expr)?;
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
