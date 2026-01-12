use std::sync::Arc;

use crate::ast::ast::{Literal, Ast, TypeId};
use crate::context::Interner;
use crate::error::ProviderError;
use crate::traits::function::FunctionImpl;

/// Output from a type provider (F# style)
///
/// Providers generate both a type AND an optional module specification.
/// When a module is generated, it contains functions that operate on the type.
///
/// Example: `csv<"people.csv">` generates:
/// - Type: `{ id: int, name: string, age: int }`
/// - Module: `People` with `People::load()` function
pub struct ProviderOutput {
    /// The generated AST type
    pub generated_type: TypeId,

    /// Optional module specification
    /// When Some, a module with this name will be created containing functions
    pub module_spec: Option<ModuleSpec>,
}

/// Specification for a generated module
///
/// Describes the functions and submodules that should be created
/// for a provider-generated module.
pub struct ModuleSpec {
    /// Functions to register in the generated module
    pub functions: Vec<FunctionDef>,

    /// Nested submodules (for future extensibility)
    pub submodules: Vec<(String, ModuleSpec)>,
}

/// Definition of a function to be registered in a generated module
pub struct FunctionDef {
    /// Function name (e.g., "load", "save")
    pub name: String,

    /// Rust implementation of the function
    pub implementation: Arc<dyn FunctionImpl>,
}

/// The TypeProvider trait generates an AST type and optional module at compile-time
///
/// Providers have access to the AST type arena to allocate types,
/// and the interner for symbol management.
///
/// The generated AST type flows through the normal pipeline:
/// AST → Lower → HIR → TypeCheck → THIR
///
/// # F# Style Type Providers
///
/// Like F#, providers can generate both types and modules. For example,
/// a CSV provider generates a record type based on the CSV schema,
/// and a module containing a `load()` function to load the CSV at runtime.
///
/// Example:
/// ```ignore
/// type People = csv<"people.csv">
/// let data = People::load()  // Generated module function
/// ```
pub trait TypeProviderImpl: Send + Sync {
    /// Generate an AST type and optional module from compile-time arguments
    ///
    /// # Arguments
    /// * `args` - Literal arguments passed to the provider (e.g., file path)
    /// * `ast` - Mutable access to the AST arena for type allocation
    /// * `interner` - Mutable access to the symbol interner
    ///
    /// # Returns
    /// A `ProviderOutput` containing:
    /// - The generated type
    /// - An optional module specification with functions
    fn provide(
        &self,
        args: &[Literal],
        ast: &mut Ast,
        interner: &mut Interner,
    ) -> Result<ProviderOutput, ProviderError>;
}
