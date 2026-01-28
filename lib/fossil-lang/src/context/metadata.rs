//! Type metadata infrastructure
//!
//! This module provides infrastructure for capturing and storing compile-time
//! metadata about types, particularly attributes on record fields. This metadata
//! can then be accessed at runtime by builtin functions.
//!
//! # Architecture
//!
//! ```text
//! AST (RecordField.attrs)
//!   → Resolution (extract_type_metadata)
//!   → GlobalContext.type_metadata
//!   → Runtime (FunctionImpl accesses via RuntimeContext)
//! ```

use std::collections::HashMap;

use crate::ast::ast::Literal;
use crate::context::{DefId, Interner, Symbol};

/// Type metadata extracted from AST during name resolution
///
/// Contains metadata about a type definition, including:
/// - Type-level attributes (e.g., `#[rdf(type = "...", id = "...")]`)
/// - Field-level attributes (e.g., `#[rdf(uri = "...")]` on record fields)
///
/// This information is captured at compile-time and made available to
/// runtime functions via RuntimeContext.
///
/// # Example
/// ```fossil
/// #[rdf(type = "http://schema.org/Person", id = "http://example.org/${id}")]
/// type Person = {
///     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
///     name: string,
/// }
/// ```
///
/// Results in TypeMetadata with:
/// - def_id: DefId of Person
/// - type_attributes: [Attribute("rdf", { type: "http://...", id: "http://..." })]
/// - field_metadata: { "name" -> [Attribute("rdf", { uri: "http://..." })] }
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    /// DefId of the type this metadata belongs to
    pub def_id: DefId,

    /// Type-level attributes (e.g., #[rdf(type = "...", id = "...")])
    pub type_attributes: Vec<AttributeData>,

    /// Metadata for each field in the type (if it's a record)
    pub field_metadata: HashMap<Symbol, FieldMetadata>,
}

impl TypeMetadata {
    /// Create new TypeMetadata for a type
    pub fn new(def_id: DefId) -> Self {
        Self {
            def_id,
            type_attributes: Vec::new(),
            field_metadata: HashMap::new(),
        }
    }

    /// Get a type-level attribute by name
    pub fn get_type_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.type_attributes.iter().find(|attr| attr.name == name)
    }

    /// Get metadata for a specific field
    pub fn get_field(&self, field: Symbol) -> Option<&FieldMetadata> {
        self.field_metadata.get(&field)
    }

    /// Check if this type has any field metadata
    pub fn is_empty(&self) -> bool {
        self.field_metadata.is_empty()
    }
}

/// Metadata for a single field in a record type
#[derive(Debug, Clone)]
pub struct FieldMetadata {
    /// Attributes attached to this field
    pub attributes: Vec<AttributeData>,
}

impl FieldMetadata {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    /// Find an attribute by name
    pub fn get_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.attributes.iter().find(|attr| attr.name == name)
    }
}

impl Default for FieldMetadata {
    fn default() -> Self {
        Self::new()
    }
}

/// Data for a single attribute
///
/// Represents an attribute like `#[rdf(uri = "http://example.com", required = true)]`
/// with the attribute name and its named arguments.
#[derive(Debug, Clone)]
pub struct AttributeData {
    /// Name of the attribute (e.g., "rdf", "serde")
    pub name: Symbol,

    /// Named arguments to the attribute (key -> value)
    pub args: HashMap<Symbol, Literal>,
}

impl AttributeData {
    /// Get a named argument value
    pub fn get(&self, key: Symbol) -> Option<&Literal> {
        self.args.get(&key)
    }

    /// Get a string argument value by key Symbol
    ///
    /// Returns the string value if the argument exists and is a string literal.
    pub fn get_string<'a>(&self, key: Symbol, interner: &'a Interner) -> Option<&'a str> {
        match self.args.get(&key) {
            Some(Literal::String(sym)) => Some(interner.resolve(*sym)),
            _ => None,
        }
    }

    /// Get an integer argument value by key Symbol
    ///
    /// Returns the integer value if the argument exists and is an integer literal.
    pub fn get_int(&self, key: Symbol) -> Option<i64> {
        match self.args.get(&key) {
            Some(Literal::Integer(n)) => Some(*n),
            _ => None,
        }
    }

    /// Get a boolean argument value by key Symbol
    ///
    /// Returns the boolean value if the argument exists and is a boolean literal.
    pub fn get_bool(&self, key: Symbol) -> Option<bool> {
        match self.args.get(&key) {
            Some(Literal::Boolean(b)) => Some(*b),
            _ => None,
        }
    }
}

/// Extract type metadata from AST and store in pending_type_metadata
///
/// This function iterates through all Type statements in the AST,
/// extracts field attributes from record types, and stores them
/// keyed by the type name Symbol. During IR resolution, this metadata
/// will be transferred to type_metadata keyed by DefId.
pub fn extract_pending_type_metadata(
    ast: &crate::ast::ast::Ast,
    gcx: &mut crate::passes::GlobalContext,
) {
    use crate::ast::ast::{StmtKind, TypeKind};

    for &stmt_id in &ast.root {
        let stmt = ast.stmts.get(stmt_id);

        if let StmtKind::Type { name, ty, attrs } = &stmt.kind {
            let ty_node = ast.types.get(*ty);

            // Create metadata with type-level attributes
            let mut metadata = TypeMetadata::new(DefId::new(0));

            // Store type-level attributes (e.g., #[rdf(type = "...", id = "...")])
            for attr in attrs {
                let args = attr
                    .args
                    .iter()
                    .map(|arg| (arg.key, arg.value.clone()))
                    .collect();

                metadata.type_attributes.push(AttributeData {
                    name: attr.name,
                    args,
                });
            }

            // Extract field-level attributes from record types
            if let TypeKind::Record(fields) = &ty_node.kind {
                for field in fields {
                    if !field.attrs.is_empty() {
                        let mut field_meta = FieldMetadata::new();

                        for attr in &field.attrs {
                            // Convert Vec<AttributeArg> to HashMap<Symbol, Literal>
                            let args = attr
                                .args
                                .iter()
                                .map(|arg| (arg.key, arg.value.clone()))
                                .collect();

                            field_meta.attributes.push(AttributeData {
                                name: attr.name,
                                args,
                            });
                        }

                        metadata.field_metadata.insert(field.name, field_meta);
                    }
                }
            }

            // Store metadata if it has any content (type-level or field-level)
            if !metadata.is_empty() || !metadata.type_attributes.is_empty() {
                gcx.pending_type_metadata.insert(*name, metadata);
            }
        }
    }
}

/// A typed wrapper for attribute access with convenient string-based lookups
///
/// This provides a more ergonomic API for accessing attribute arguments
/// without needing to manually look up symbols.
///
/// # Example
///
/// ```rust,ignore
/// let typed = TypedAttribute::new(attr_data, interner);
/// let uri = typed.string("uri");    // Returns Option<&str>
/// let count = typed.int("count");   // Returns Option<i64>
/// let flag = typed.bool("enabled"); // Returns Option<bool>
/// ```
pub struct TypedAttribute<'a> {
    attr: &'a AttributeData,
    interner: &'a Interner,
}

impl<'a> TypedAttribute<'a> {
    /// Create a new TypedAttribute wrapper
    pub fn new(attr: &'a AttributeData, interner: &'a Interner) -> Self {
        Self { attr, interner }
    }

    /// Get the attribute name as a string
    pub fn name(&self) -> &str {
        self.interner.resolve(self.attr.name)
    }

    /// Get a string argument by name
    ///
    /// Looks up the key in the interner and returns the string value if found.
    pub fn string(&self, key: &str) -> Option<&str> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_string(key_sym, self.interner)
    }

    /// Get an integer argument by name
    ///
    /// Looks up the key in the interner and returns the integer value if found.
    pub fn int(&self, key: &str) -> Option<i64> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_int(key_sym)
    }

    /// Get a boolean argument by name
    ///
    /// Looks up the key in the interner and returns the boolean value if found.
    pub fn bool(&self, key: &str) -> Option<bool> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_bool(key_sym)
    }

    /// Check if an argument exists
    pub fn has(&self, key: &str) -> bool {
        self.interner
            .lookup(key)
            .map(|sym| self.attr.args.contains_key(&sym))
            .unwrap_or(false)
    }
}
