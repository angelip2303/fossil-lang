//! Procedural macros for Fossil
//!
//! This crate provides derive macros for declarative attribute extraction.
//!
//! # Example
//!
//! ```rust,ignore
//! use fossil_macros::FromAttrs;
//!
//! #[derive(FromAttrs)]
//! struct RdfAttrs {
//!     /// Extracted from #[rdf(type = "...")]
//!     #[attr("rdf.type")]
//!     rdf_type: Option<String>,
//!
//!     /// Extracted from #[rdf(id = "...")]
//!     #[attr("rdf.id")]
//!     id_template: Option<String>,
//! }
//!
//! // Field-level attributes
//! #[derive(FromAttrs)]
//! struct RdfFieldAttrs {
//!     /// Extracted from #[rdf(uri = "...")] on a field
//!     #[attr("rdf.uri")]
//!     uri: Option<String>,
//! }
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Type, Lit, Meta};

/// Derive macro for extracting attributes from TypeMetadata
///
/// Generates an implementation that extracts named attributes into struct fields.
///
/// # Field Attributes
///
/// - `#[attr("namespace.key")]` - Extract from type-level attribute
/// - `#[attr("namespace.key", field)]` - Extract from field-level attribute
///
/// # Supported Types
///
/// - `Option<String>` - Optional string attribute
/// - `Option<i64>` - Optional integer attribute
/// - `Option<bool>` - Optional boolean attribute
/// - `String` - Required string attribute (panics if missing)
/// - `i64` - Required integer attribute
/// - `bool` - Required boolean attribute
#[proc_macro_derive(FromAttrs, attributes(attr))]
pub fn derive_from_attrs(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => panic!("FromAttrs only supports structs with named fields"),
        },
        _ => panic!("FromAttrs only supports structs"),
    };

    let mut type_level_extractors = Vec::new();
    let mut type_level_field_names = Vec::new();
    let mut field_level_extractors = Vec::new();
    let mut field_level_field_names = Vec::new();

    for field in fields {
        let field_name = field.ident.as_ref().unwrap();

        // Find #[attr("...")] attribute
        let attr_args = field.attrs.iter()
            .find_map(|attr| {
                if attr.path().is_ident("attr") {
                    let meta = &attr.meta;
                    if let Meta::List(list) = meta {
                        let tokens = list.tokens.clone();
                        let parsed: Result<AttrArgs, _> = syn::parse2(tokens);
                        parsed.ok()
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

        let Some(attr_args) = attr_args else {
            panic!("Field '{}' must have #[attr(\"namespace.key\")] attribute", field_name);
        };

        let (namespace, key) = parse_attr_path(&attr_args.path);
        let is_field_level = attr_args.is_field_level;
        let field_ty = &field.ty;

        let extractor = generate_field_extractor(
            field_name,
            &namespace,
            &key,
            field_ty,
            is_field_level,
        );

        if is_field_level {
            field_level_extractors.push(extractor);
            field_level_field_names.push(field_name.clone());
        } else {
            type_level_extractors.push(extractor);
            type_level_field_names.push(field_name.clone());
        }
    }

    // Determine which methods to generate based on what fields exist
    let has_type_level = !type_level_extractors.is_empty();
    let has_field_level = !field_level_extractors.is_empty();

    let type_level_method = if has_type_level {
        quote! {
            /// Extract type-level attributes from TypeMetadata
            ///
            /// Use this for attributes like `#[rdf(type = "...", id = "...")]` on the type.
            pub fn from_type_metadata(
                metadata: &fossil_lang::context::TypeMetadata,
                interner: &fossil_lang::context::Interner,
            ) -> Self {
                #(#type_level_extractors)*

                Self {
                    #(#type_level_field_names),*
                }
            }
        }
    } else {
        quote! {}
    };

    let field_level_method = if has_field_level {
        quote! {
            /// Extract field-level attributes from FieldMetadata
            ///
            /// Use this for attributes like `#[rdf(uri = "...")]` on record fields.
            pub fn from_field_metadata(
                metadata: &fossil_lang::context::FieldMetadata,
                interner: &fossil_lang::context::Interner,
            ) -> Self {
                #(#field_level_extractors)*

                Self {
                    #(#field_level_field_names),*
                }
            }
        }
    } else {
        quote! {}
    };

    let expanded = quote! {
        impl #name {
            #type_level_method
            #field_level_method
        }
    };

    TokenStream::from(expanded)
}

/// Arguments to the #[attr(...)] attribute
struct AttrArgs {
    path: String,
    is_field_level: bool,
}

impl syn::parse::Parse for AttrArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Parse the string literal path
        let path_lit: Lit = input.parse()?;
        let path = match path_lit {
            Lit::Str(s) => s.value(),
            _ => return Err(input.error("expected string literal")),
        };

        // Check for optional ", field" marker
        let is_field_level = if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;
            let ident: syn::Ident = input.parse()?;
            ident == "field"
        } else {
            false
        };

        Ok(AttrArgs { path, is_field_level })
    }
}

/// Parse "namespace.key" into (namespace, key)
fn parse_attr_path(path: &str) -> (String, String) {
    let parts: Vec<&str> = path.splitn(2, '.').collect();
    match parts.as_slice() {
        [ns, key] => (ns.to_string(), key.to_string()),
        _ => panic!("Invalid attr path '{}', expected 'namespace.key'", path),
    }
}

/// Generate code to extract a single field
fn generate_field_extractor(
    field_name: &syn::Ident,
    namespace: &str,
    key: &str,
    field_ty: &Type,
    is_field_level: bool,
) -> proc_macro2::TokenStream {
    let namespace_str = namespace;
    let key_str = key;

    // Determine if the field is Option<T> or T
    let (is_optional, inner_type) = unwrap_option_type(field_ty);

    // Generate the extraction code based on the inner type
    let extraction = match get_type_name(&inner_type).as_str() {
        "String" => quote! {
            typed_attr.string(#key_str).map(|s| s.to_string())
        },
        "i64" => quote! {
            typed_attr.int(#key_str)
        },
        "bool" => quote! {
            typed_attr.bool(#key_str)
        },
        other => panic!("Unsupported attribute type: {}", other),
    };

    let getter = if is_field_level {
        quote! { metadata.get_attribute(ns) }
    } else {
        quote! { metadata.get_type_attribute(ns) }
    };

    if is_optional {
        quote! {
            let #field_name = {
                let ns_sym = interner.lookup(#namespace_str);
                ns_sym.and_then(|ns| {
                    #getter.and_then(|attr_data| {
                        let typed_attr = fossil_lang::context::TypedAttribute::new(attr_data, interner);
                        #extraction
                    })
                })
            };
        }
    } else {
        quote! {
            let #field_name = {
                let ns_sym = interner.lookup(#namespace_str)
                    .expect(concat!("Missing attribute namespace: ", #namespace_str));
                let attr_data = #getter
                    .expect(concat!("Missing attribute: ", #namespace_str));
                let typed_attr = fossil_lang::context::TypedAttribute::new(attr_data, interner);
                #extraction.expect(concat!("Missing attribute key: ", #key_str))
            };
        }
    }
}

/// Check if a type is Option<T> and return (is_option, inner_type)
fn unwrap_option_type(ty: &Type) -> (bool, Type) {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        return (true, inner.clone());
                    }
                }
            }
        }
    }
    (false, ty.clone())
}

/// Get the simple type name from a Type
fn get_type_name(ty: &Type) -> String {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            return segment.ident.to_string();
        }
    }
    panic!("Cannot determine type name for attribute field")
}
