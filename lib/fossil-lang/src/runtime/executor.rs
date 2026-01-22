//! THIR Program Executor
//!
//! This module provides execution of complete THIR programs.
//! It processes top-level statements and returns the final results.

use crate::ast::thir::StmtKind;
use crate::error::RuntimeError;
use crate::passes::ThirProgram;
use crate::runtime::evaluator::ThirEvaluator;
use crate::runtime::value::{Environment, Value};

/// Executor for complete THIR programs
///
/// Takes a compiled THIR program and executes it from top to bottom,
/// processing statements and collecting results.
pub struct ThirExecutor;

impl ThirExecutor {
    /// Execute a THIR program and return the results
    ///
    /// # Arguments
    ///
    /// * `program` - The compiled THIR program to execute
    ///
    /// # Returns
    ///
    /// A vector of values, one for each top-level statement/expression
    ///
    /// # Example
    ///
    /// ```ignore
    /// use fossil_lang::compiler::{Compiler, CompilerInput};
    ///
    /// let compiler = Compiler::new();
    /// let input = CompilerInput::String {
    ///     src: source.to_string(),
    ///     name: "example".to_string()
    /// };
    /// let thir = compiler.compile(input)?;
    /// let results = ThirExecutor::execute(thir)?;
    /// ```
    pub fn execute(program: ThirProgram) -> Result<Vec<Value>, RuntimeError> {
        let ThirProgram { thir, gcx } = program;

        // Create environment for top-level bindings
        let mut env = Environment::new();

        // Create evaluator
        let mut evaluator = ThirEvaluator::new(&thir, &gcx, env.clone());

        // Process each top-level statement
        let mut results = Vec::new();

        for &stmt_id in &thir.root {
            let stmt = thir.stmts.get(stmt_id);
            match &stmt.kind {
                StmtKind::Let { name, value } => {
                    // Evaluate the value
                    let val = evaluator.eval(*value)?;

                    // Bind it in the environment
                    env.bind(*name, val.clone());

                    // Update evaluator's environment
                    evaluator = ThirEvaluator::new(&thir, &gcx, env.clone());

                    // Add to results
                    results.push(val);
                }

                StmtKind::Expr(expr_id) => {
                    // Evaluate the expression
                    let val = evaluator.eval(*expr_id)?;
                    results.push(val);
                }

                StmtKind::Import { .. } => {
                    // Imports are handled at compile time, nothing to do at runtime
                }

                StmtKind::Type { .. } => {
                    // Type declarations are compile time only, nothing to do at runtime
                }
            }
        }

        Ok(results)
    }
}
