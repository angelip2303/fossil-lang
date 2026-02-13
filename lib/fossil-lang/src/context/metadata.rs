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

use crate::ast::{Attribute, AttributeArg, Literal, StmtKind, TypeKind};
use crate::context::{Interner, Symbol};

/// Type metadata (type-level + field-level attributes) extracted from AST
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    pub type_attributes: Vec<AttributeData>,
    pub field_metadata: HashMap<Symbol, FieldMetadata>,
}

impl TypeMetadata {
    pub fn new() -> Self {
        Self {
            type_attributes: Vec::new(),
            field_metadata: HashMap::new(),
        }
    }

    pub fn get_type_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.type_attributes.iter().find(|attr| attr.name == name)
    }

    pub fn get_field(&self, field: Symbol) -> Option<&FieldMetadata> {
        self.field_metadata.get(&field)
    }

    pub fn is_empty(&self) -> bool {
        self.field_metadata.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct FieldMetadata {
    pub attributes: Vec<AttributeData>,
}

impl FieldMetadata {
    pub fn new() -> Self {
        Self {
            attributes: Vec::new(),
        }
    }

    pub fn get_attribute(&self, name: Symbol) -> Option<&AttributeData> {
        self.attributes.iter().find(|attr| attr.name == name)
    }
}

impl Default for FieldMetadata {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct AttributeData {
    pub name: Symbol,
    pub args: HashMap<Symbol, Literal>,
    pub positional: Vec<Literal>,
}

impl AttributeData {
    pub fn get(&self, key: Symbol) -> Option<&Literal> {
        self.args.get(&key)
    }

    pub fn get_string<'a>(&self, key: Symbol, interner: &'a Interner) -> Option<&'a str> {
        match self.args.get(&key) {
            Some(Literal::String(sym)) => Some(interner.resolve(*sym)),
            _ => None,
        }
    }

    pub fn get_int(&self, key: Symbol) -> Option<i64> {
        match self.args.get(&key) {
            Some(Literal::Integer(n)) => Some(*n),
            _ => None,
        }
    }

    pub fn get_bool(&self, key: Symbol) -> Option<bool> {
        match self.args.get(&key) {
            Some(Literal::Boolean(b)) => Some(*b),
            _ => None,
        }
    }

    pub fn first_positional_string<'a>(&self, interner: &'a Interner) -> Option<&'a str> {
        match self.positional.first() {
            Some(Literal::String(sym)) => Some(interner.resolve(*sym)),
            _ => None,
        }
    }

    fn from_attribute(attr: &Attribute) -> Self {
        let mut args = HashMap::new();
        let mut positional = Vec::new();
        for arg in &attr.args {
            match arg {
                AttributeArg::Named { key, value } => {
                    args.insert(*key, value.clone());
                }
                AttributeArg::Positional(value) => {
                    positional.push(value.clone());
                }
            }
        }
        AttributeData {
            name: attr.name,
            args,
            positional,
        }
    }
}

pub fn extract_type_metadata(ast: &crate::ast::Ast) -> HashMap<Symbol, TypeMetadata> {
    let mut result = HashMap::new();

    for &stmt_id in &ast.root {
        let stmt = ast.stmts.get(stmt_id);

        if let StmtKind::Type {
            name, ty, attrs, ..
        } = &stmt.kind
        {
            let ty_node = ast.types.get(*ty);

            let mut metadata = TypeMetadata::new();

            for attr in attrs {
                metadata
                    .type_attributes
                    .push(AttributeData::from_attribute(attr));
            }

            if let TypeKind::Record(fields) = &ty_node.kind {
                for field in fields {
                    if !field.attrs.is_empty() {
                        let mut field_meta = FieldMetadata::new();
                        for attr in &field.attrs {
                            field_meta
                                .attributes
                                .push(AttributeData::from_attribute(attr));
                        }
                        metadata.field_metadata.insert(field.name, field_meta);
                    }
                }
            }

            if !metadata.is_empty() || !metadata.type_attributes.is_empty() {
                result.insert(*name, metadata);
            }
        }
    }

    result
}

pub struct TypedAttribute<'a> {
    attr: &'a AttributeData,
    interner: &'a Interner,
}

impl<'a> TypedAttribute<'a> {
    pub fn new(attr: &'a AttributeData, interner: &'a Interner) -> Self {
        Self { attr, interner }
    }

    pub fn name(&self) -> &str {
        self.interner.resolve(self.attr.name)
    }

    pub fn string(&self, key: &str) -> Option<&str> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_string(key_sym, self.interner)
    }

    pub fn int(&self, key: &str) -> Option<i64> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_int(key_sym)
    }

    pub fn bool(&self, key: &str) -> Option<bool> {
        let key_sym = self.interner.lookup(key)?;
        self.attr.get_bool(key_sym)
    }

    pub fn has(&self, key: &str) -> bool {
        self.interner
            .lookup(key)
            .map(|sym| self.attr.args.contains_key(&sym))
            .unwrap_or(false)
    }
}
