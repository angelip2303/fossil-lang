//! IR Program Executor
//!
//! This module provides execution of complete IR programs.
//! It processes top-level statements and returns the final results.

use crate::error::CompileError;
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
    pub fn execute(program: IrProgram) -> Result<Vec<Value>, CompileError> {
        let IrProgram { ir, gcx, .. } = program;

        // Create evaluator with empty environment
        let mut evaluator = IrEvaluator::new(&ir, &gcx, Environment::new());

        // Process each top-level statement
        let mut results = Vec::new();

        for &stmt_id in &ir.root {
            let stmt = ir.stmts.get(stmt_id);
            match &stmt.kind {
                // Let and Const are identical at runtime
                StmtKind::Let { name, value, .. } | StmtKind::Const { name, value, .. } => {
                    let val = evaluator.eval(*value)?;
                    // Bind directly in evaluator's environment - no clone needed
                    evaluator.bind(*name, val.clone());
                    results.push(val);
                }

                StmtKind::Expr(expr_id) => {
                    let val = evaluator.eval(*expr_id)?;
                    results.push(val);
                }

                StmtKind::Type { .. } => {
                    // Type declarations are compile-time only
                }
            }
        }

        Ok(results)
    }
}
