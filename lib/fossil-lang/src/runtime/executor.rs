//! IR Program Executor
//!
//! This module provides execution of complete IR programs.
//! It processes top-level statements and returns the final results.

use crate::error::RuntimeError;
use crate::ir::StmtKind;
use crate::passes::IrProgram;
use crate::runtime::evaluator::IrEvaluator;
use crate::runtime::value::{Environment, Value};

/// Executor for complete IR programs
///
/// Takes a compiled IR program and executes it from top to bottom,
/// processing statements and collecting results.
pub struct IrExecutor;

impl IrExecutor {
    /// Execute an IR program and return the results
    ///
    /// # Arguments
    ///
    /// * `program` - The compiled IR program to execute
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
    /// let ir = compiler.compile(input)?;
    /// let results = IrExecutor::execute(ir)?;
    /// ```
    pub fn execute(program: IrProgram) -> Result<Vec<Value>, RuntimeError> {
        let IrProgram { ir, gcx, .. } = program;

        // Create environment for top-level bindings
        let mut env = Environment::new();

        // Create evaluator
        let mut evaluator = IrEvaluator::new(&ir, &gcx, env.clone());

        // Process each top-level statement
        let mut results = Vec::new();

        for &stmt_id in &ir.root {
            let stmt = ir.stmts.get(stmt_id);
            match &stmt.kind {
                StmtKind::Let { name, value, .. } => {
                    // Evaluate the value
                    let val = evaluator.eval(*value)?;

                    // Bind it in the environment
                    env.bind(*name, val.clone());

                    // Update evaluator's environment
                    evaluator = IrEvaluator::new(&ir, &gcx, env.clone());

                    // Add to results
                    results.push(val);
                }

                StmtKind::Const { name, value, .. } => {
                    // Evaluate the value
                    let val = evaluator.eval(*value)?;

                    // Bind it in the environment (same as let at runtime)
                    env.bind(*name, val.clone());

                    // Update evaluator's environment
                    evaluator = IrEvaluator::new(&ir, &gcx, env.clone());

                    // Add to results
                    results.push(val);
                }

                StmtKind::Expr(expr_id) => {
                    // Evaluate the expression
                    let val = evaluator.eval(*expr_id)?;
                    results.push(val);
                }

                StmtKind::Type { .. } => {
                    // Type declarations are compile time only, nothing to do at runtime
                }

                StmtKind::Trait { .. } => {
                    // Trait declarations are compile time only, nothing to do at runtime
                }

                StmtKind::Impl { .. } => {
                    // Impl declarations are compile time only, nothing to do at runtime
                }
            }
        }

        Ok(results)
    }
}
