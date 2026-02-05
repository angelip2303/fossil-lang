//! Global compiler context
//!
//! Contains the GlobalContext struct which is the central state store for the compiler.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::context::{
    DefId, DefKind, Definitions, Interner, Kind, TypeConstructorInfo, TypeMetadata,
};

use crate::traits::function::FunctionImpl;
use crate::traits::provider::TypeProviderImpl;

/// Global context for the compiler
#[derive(Clone)]
pub struct GlobalContext {
    /// String interner for symbol management
    pub interner: Interner,
    /// Definition storage
    pub definitions: Definitions,
    /// Registry of type constructors (generic types like List)
    pub type_constructors: HashMap<DefId, TypeConstructorInfo>,
    /// Type metadata (attributes, field info) - keyed by DefId
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    /// Set of variadic function DefIds
    pub variadic_functions: HashSet<DefId>,
}

impl GlobalContext {
    /// Register a type provider
    ///
    /// This allows external crates (e.g. fossil-providers) to register
    /// their providers.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_providers::CsvProvider;
    ///
    /// let mut gcx = GlobalContext::default();
    /// gcx.register_provider("csv", CsvProvider);
    /// ```
    pub fn register_provider(&mut self, name: &str, provider: impl TypeProviderImpl + 'static) {
        let symbol = self.interner.intern(name);
        let provider = Arc::new(provider);
        let def_kind = DefKind::Provider(provider);
        self.definitions.insert(None, symbol, def_kind);
    }

    /// Register a type constructor
    ///
    /// This allows external crates (like fossil-stdlib) to register
    /// generic types like Entity, Option, Result, etc.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the type constructor (e.g., "Entity", "Option")
    /// * `arity` - The number of type parameters (e.g., 1 for Entity<T>, 2 for Map<K,V>)
    ///
    /// # Returns
    ///
    /// The DefId of the registered type constructor, which can be used when
    /// constructing `TypeKind::App` instances.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    ///
    /// let mut gcx = GlobalContext::default();
    /// let option_def_id = gcx.register_type_constructor("Option", 1);
    /// // Now Option can be used as Option<T> in the type system
    /// ```
    pub fn register_type_constructor(&mut self, name: &str, arity: usize) -> DefId {
        let sym = self.interner.intern(name);

        // Check if already registered
        if let Some(def) = self.definitions.get_by_symbol(sym) {
            return def.id();
        }

        // Register as a type definition
        let def_id = self.definitions.insert(None, sym, DefKind::Type);

        // Store type constructor metadata
        self.type_constructors.insert(
            def_id,
            TypeConstructorInfo {
                def_id,
                arity,
                name: sym,
                param_kinds: vec![Kind::Type; arity],
            },
        );

        def_id
    }

    /// Register a function within a module
    ///
    /// This allows external crates (like fossil-stdlib) to register
    /// their functions without creating a circular dependency.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_stdlib::EntityWrapFunction;
    /// use std::sync::Arc;
    ///
    /// let mut gcx = GlobalContext::default();
    /// gcx.register_function("Entity", "wrap", Arc::new(EntityWrapFunction));
    /// ```
    pub fn register_function(
        &mut self,
        module_name: &str,
        function_name: &str,
        function: impl FunctionImpl + 'static,
    ) {
        let module_symbol = self.interner.intern(module_name);
        let module_id = self
            .definitions
            .get_by_symbol(module_symbol)
            .map(|def| def.id())
            .unwrap_or_else(|| {
                self.definitions.insert(
                    None,
                    module_symbol,
                    DefKind::Mod {
                        file_path: None,
                        is_inline: true,
                    },
                )
            });

        // Register function within module
        let function_symbol = self.interner.intern(function_name);
        self.definitions.insert(
            Some(module_id),
            function_symbol,
            DefKind::Func(Arc::new(function)),
        );
    }

    /// Register a top-level function (not within a module)
    ///
    /// Use this for builtin functions that should be called directly
    /// without module qualification (e.g., `to(...)` instead of `Mod::func(...)`).
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_stdlib::ToFunction;
    ///
    /// let mut gcx = GlobalContext::default();
    /// gcx.register_toplevel_function("to", ToFunction);
    /// // Now callable as: to(source, Type, fn(x) -> ...)
    /// ```
    pub fn register_toplevel_function(
        &mut self,
        function_name: &str,
        function: impl FunctionImpl + 'static,
    ) {
        let function_symbol = self.interner.intern(function_name);
        self.definitions.insert(
            None,
            function_symbol,
            DefKind::Func(Arc::new(function)),
        );
    }

    /// Register a variadic top-level function
    ///
    /// Variadic functions accept any number of additional arguments beyond
    /// their declared signature. The type checker will only verify the
    /// declared parameters; extra arguments are passed to the runtime.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    ///
    /// let mut gcx = GlobalContext::default();
    /// gcx.register_variadic_toplevel_function("print", PrintFunction);
    /// // Now callable as: print(arg1, arg2, arg3, ...)
    /// ```
    pub fn register_variadic_toplevel_function(
        &mut self,
        function_name: &str,
        function: impl FunctionImpl + 'static,
    ) {
        let function_symbol = self.interner.intern(function_name);
        let def_id = self.definitions.insert(
            None,
            function_symbol,
            DefKind::Func(Arc::new(function)),
        );
        self.variadic_functions.insert(def_id);
    }

    pub fn is_variadic(&self, def_id: DefId) -> bool {
        self.variadic_functions.contains(&def_id)
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            interner: Interner::default(),
            definitions: Definitions::default(),
            type_metadata: HashMap::new(),
            type_constructors: HashMap::new(),
            variadic_functions: HashSet::new(),
        }
    }
}
