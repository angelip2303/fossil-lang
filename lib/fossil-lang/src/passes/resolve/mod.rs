//! Name resolution module
//!
//! This module handles name resolution, building symbol tables and resolving all
//! identifiers to their definitions.
//!
//! ## Resolution Process
//!
//! 1. **Declaration Collection** (`collect_declarations`): First pass collects all
//!    top-level declarations (functions, types) into the scope.
//! 2. **Statement Resolution** (`resolve_stmt`): Second pass resolves names in each
//!    statement, creating bindings and resolving references.
//! 3. **Expression/Type Resolution**: Recursively resolves names in expressions and types.
//!
//! ## Module Organization
//!
//! - `scope`: Scope stack for tracking nested bindings
//! - `table`: Resolution table mapping AST nodes to definitions
//! - `resolver`: Main name resolver implementation

pub mod scope;
pub mod table;
mod resolver;

// Re-exports
pub use table::{ResolvedAst, ResolutionTable};
pub use resolver::NameResolver;
