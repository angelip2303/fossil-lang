//! Global compiler context
//!
//! Contains the GlobalContext struct which is the central state store for the compiler.
//! It is organized into three cohesive domains:
//! - Core: interner + definitions (infrastructure base)
//! - Type System: constructors and metadata of types
//! - Trait System: implementations of traits

use std::collections::HashMap;
use std::sync::Arc;

use crate::context::{
    AttributeRegistry, AttributeSchema, DefId, DefKind, Definitions, Interner, Kind, Symbol,
    TypeConstructorInfo, TypeMetadata,
};
use crate::traits::function::FunctionImpl;

/// Information about a trait implementation
#[derive(Clone, Debug)]
pub struct TraitImplInfo {
    /// Method name -> DefId of the implementation function
    pub methods: HashMap<Symbol, DefId>,
}

/// Cached DefIds for builtin traits
#[derive(Clone, Debug, Default)]
pub struct BuiltinTraits {
    pub to_string: Option<DefId>,
}

/// Global context for the compiler
///
/// Organized in 3 domains:
/// - Core: interner + definitions (infrastructure)
/// - Types: constructors and metadata of types
/// - Traits: implementations of traits
#[derive(Clone)]
pub struct GlobalContext {
    // === Core (infrastructure) ===
    /// String interner for symbol management
    pub interner: Interner,
    /// Definition storage
    pub definitions: Definitions,
    /// Pre-interned wildcard symbol `_` for use in type signatures
    wildcard_symbol: Symbol,

    // === Type System ===
    /// Registry of type constructors (generic types like List, Entity, Option)
    pub type_constructors: HashMap<DefId, TypeConstructorInfo>,
    /// Type metadata (attributes, field info) - keyed by DefId
    pub type_metadata: HashMap<DefId, Arc<TypeMetadata>>,
    /// Pending type metadata keyed by type name Symbol (before DefId assignment)
    /// This is populated from AST attributes and transferred to type_metadata during resolution
    pub pending_type_metadata: HashMap<Symbol, TypeMetadata>,
    /// Cached DefId for the List type constructor (builtin)
    pub list_type_ctor: Option<DefId>,

    // === Trait System ===
    /// Registry of trait implementations: (trait_def_id, type_def_id) -> impl info
    pub trait_impls: HashMap<(DefId, DefId), TraitImplInfo>,
    /// Builtin trait DefIds
    pub builtin_traits: BuiltinTraits,

    // === Attributes ===
    /// Registry of attribute schemas for compile-time validation
    pub attribute_registry: AttributeRegistry,
}

impl GlobalContext {
    pub fn new() -> Self {
        let mut interner = Interner::default();
        // Pre-intern the wildcard symbol for use in type signatures
        let wildcard_symbol = interner.intern("_");

        let mut gcx = Self {
            interner,
            definitions: Definitions::default(),
            wildcard_symbol,
            type_metadata: HashMap::new(),
            pending_type_metadata: HashMap::new(),
            type_constructors: HashMap::new(),
            list_type_ctor: None,
            attribute_registry: AttributeRegistry::new(),
            trait_impls: HashMap::new(),
            builtin_traits: BuiltinTraits::default(),
        };

        // Register builtin type constructors
        gcx.register_builtin_type_constructors();

        // Register builtin traits
        gcx.register_builtin_traits();

        gcx
    }

    /// Get the pre-interned wildcard symbol `_`
    ///
    /// This symbol is used in type signatures to represent "any field"
    /// in FieldSelector types.
    pub fn wildcard_symbol(&self) -> Symbol {
        self.wildcard_symbol
    }

    /// Register an attribute schema
    ///
    /// This allows external crates (like fossil-stdlib) to register
    /// their attribute schemas for compile-time validation.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_lang::context::{AttributeSchema, AttributeTarget, ArgSpec, ArgType};
    ///
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_attribute(
    ///     AttributeSchema::new("rdf", AttributeTarget::Field)
    ///         .arg("uri", ArgSpec::required(ArgType::String))
    ///         .arg("prefix", ArgSpec::optional(ArgType::String))
    /// );
    /// ```
    pub fn register_attribute(&mut self, schema: AttributeSchema) {
        let name_symbol = self.interner.intern(schema.name);
        self.attribute_registry.register(schema, name_symbol);
    }

    /// Register builtin traits (e.g., ToString)
    fn register_builtin_traits(&mut self) {
        // Register ToString trait: trait ToString { to_string: (self) -> string }
        let to_string_sym = self.interner.intern("to_string");
        let to_string_trait_sym = self.interner.intern("ToString");
        let to_string_def_id = self.definitions.insert(
            None,
            to_string_trait_sym,
            DefKind::Trait {
                methods: vec![to_string_sym],
            },
        );
        self.builtin_traits.to_string = Some(to_string_def_id);

        // Register builtin impl ToString for primitives (int, bool, string)
        // These are inherently stringable at runtime via value_to_string
        let int_sym = self.interner.intern("int");
        let int_def_id = self.definitions.insert(None, int_sym, DefKind::Type);
        let mut int_methods = HashMap::new();
        int_methods.insert(to_string_sym, int_def_id);
        self.trait_impls.insert(
            (to_string_def_id, int_def_id),
            TraitImplInfo {
                methods: int_methods,
            },
        );

        let bool_sym = self.interner.intern("bool");
        let bool_def_id = self.definitions.insert(None, bool_sym, DefKind::Type);
        let mut bool_methods = HashMap::new();
        bool_methods.insert(to_string_sym, bool_def_id);
        self.trait_impls.insert(
            (to_string_def_id, bool_def_id),
            TraitImplInfo {
                methods: bool_methods,
            },
        );

        let string_sym = self.interner.intern("string");
        let string_def_id = self.definitions.insert(None, string_sym, DefKind::Type);
        let mut string_methods = HashMap::new();
        string_methods.insert(to_string_sym, string_def_id);
        self.trait_impls.insert(
            (to_string_def_id, string_def_id),
            TraitImplInfo {
                methods: string_methods,
            },
        );
    }

    /// Register builtin type constructors
    ///
    /// Currently only registers `List` as a builtin generic type.
    fn register_builtin_type_constructors(&mut self) {
        // Register List<T> as a builtin type constructor
        let list_sym = self.interner.intern("List");
        let list_def_id = self.definitions.insert(None, list_sym, DefKind::Type);
        self.type_constructors.insert(
            list_def_id,
            TypeConstructorInfo {
                def_id: list_def_id,
                arity: 1,
                name: list_sym,
                param_kinds: vec![Kind::Type],
            },
        );
        self.list_type_ctor = Some(list_def_id);
    }

    /// Register a type provider
    ///
    /// This allows external crates (like fossil-providers) to register
    /// their providers without creating a circular dependency.
    ///
    /// Example:
    /// ```ignore
    /// use fossil_lang::passes::GlobalContext;
    /// use fossil_providers::CsvProvider;
    /// use std::sync::Arc;
    ///
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_provider("csv", Arc::new(CsvProvider));
    /// ```
    pub fn register_provider(
        &mut self,
        name: &str,
        provider: impl crate::traits::provider::TypeProviderImpl + 'static,
    ) {
        let symbol = self.interner.intern(name);
        self.definitions
            .insert(None, symbol, DefKind::Provider(Arc::new(provider)));
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
    /// let mut gcx = GlobalContext::new();
    /// let entity_def_id = gcx.register_type_constructor("Entity", 1);
    /// // Now Entity can be used as Entity<T> in the type system
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

    /// Register a function
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
    /// let mut gcx = GlobalContext::new();
    /// gcx.register_function("Entity", "wrap", Arc::new(EntityWrapFunction));
    /// ```
    pub fn register_function(
        &mut self,
        module_name: &str,
        function_name: &str,
        function: impl FunctionImpl + 'static,
    ) {
        // Get or create module
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
            DefKind::Func(Some(Arc::new(function))),
        );
    }
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self::new()
    }
}
