//! SHACL Type Provider
//!
//! This provider reads SHACL (Shapes Constraint Language) schema files and generates
//! Fossil record types with field-level URI attributes for RDF serialization.
//!
//! # Usage
//!
//! ```fossil
//! type Person = shacl!("schema.ttl", shape: "PersonShape")
//! ```
//!
//! # Generated Output
//!
//! Given a SHACL shape like:
//! ```turtle
//! @prefix sh: <http://www.w3.org/ns/shacl#> .
//! @prefix ex: <http://example.org/> .
//! @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
//!
//! ex:PersonShape a sh:NodeShape ;
//!     sh:property [
//!         sh:path ex:name ;
//!         sh:datatype xsd:string ;
//!         sh:minCount 1 ;
//!         sh:maxCount 1
//!     ] ;
//!     sh:property [
//!         sh:path ex:age ;
//!         sh:datatype xsd:integer ;
//!         sh:minCount 0 ;
//!         sh:maxCount 1
//!     ] .
//! ```
//!
//! The provider generates:
//! ```fossil
//! type Person = {
//!     #[rdf(uri = "http://example.org/name")]
//!     name: string,
//!     #[rdf(uri = "http://example.org/age")]
//!     age: Option<int>
//! }
//! ```

use std::fs;

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{
    Ast, Attribute, AttributeArg, Literal, Path, PrimitiveType, ProviderArgument, RecordField,
    Type as AstType, TypeKind as AstTypeKind,
};
use fossil_lang::context::Interner;
use fossil_lang::error::{CompileErrorKind, ProviderError};
use fossil_lang::traits::provider::{ProviderOutput, ProviderParamInfo, TypeProviderImpl};

use shacl_ast::ast::Schema as ShaclSchema;
use shacl_ast::component::Component;
use shacl_ast::property_shape::PropertyShape;
use shacl_ast::shape::Shape;
use shacl_rdf::ShaclParser;
use srdf::{RDFFormat, ReaderMode, SRDFGraph};

use crate::shapes::{ShapeField, extract_local_name, xsd_to_fossil_type};
use crate::utils::*;

/// SHACL Type Provider
///
/// Reads SHACL schema files (Turtle format) and generates Fossil record types with URI attributes.
pub struct ShaclProvider;

impl TypeProviderImpl for ShaclProvider {
    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![
            ProviderParamInfo {
                name: "path",
                required: true,
                default: None,
            },
            ProviderParamInfo {
                name: "shape",
                required: true,
                default: None,
            },
        ]
    }

    fn provide(
        &self,
        args: &[ProviderArgument],
        ast: &mut Ast,
        interner: &mut Interner,
        _type_name: &str,
    ) -> Result<ProviderOutput, ProviderError> {
        // Parse arguments
        let (path_str, shape_name) = parse_shacl_args(args, interner)?;

        // Validate file
        validate_extension(&path_str, &["ttl", "shacl", "turtle"], interner)?;
        validate_local_file(&path_str, interner)?;

        // Read and parse SHACL file
        let shacl_content = fs::read_to_string(&path_str).map_err(|e| {
            ProviderError::new(
                CompileErrorKind::ProviderError(
                    interner.intern(&format!("Failed to read SHACL file: {}", e)),
                ),
                Loc::generated(),
            )
        })?;

        let schema = parse_shacl_schema(&shacl_content, &path_str, interner)?;

        // Find the specified shape
        let shape_fields = extract_shacl_fields(&schema, &shape_name, interner)?;

        // Convert SHACL fields to AST record fields
        let fields = shacl_fields_to_ast_fields(shape_fields, ast, interner);

        // Create AST record type
        let record_ty = ast.types.alloc(AstType {
            loc: Loc::generated(),
            kind: AstTypeKind::Record(fields),
        });

        Ok(ProviderOutput {
            generated_type: record_ty,
            module_spec: None,
        })
    }
}

/// Parse SHACL provider arguments
fn parse_shacl_args(
    args: &[ProviderArgument],
    interner: &mut Interner,
) -> Result<(String, String), ProviderError> {
    let path_sym = interner.intern("path");
    let shape_sym = interner.intern("shape");

    let mut path = None;
    let mut shape = None;

    // Named arguments
    for arg in args {
        if let ProviderArgument::Named { name, value } = arg {
            if *name == path_sym {
                if let Literal::String(s) = value {
                    path = Some(interner.resolve(*s).to_string());
                }
            } else if *name == shape_sym
                && let Literal::String(s) = value
            {
                shape = Some(interner.resolve(*s).to_string());
            }
        }
    }

    // Positional arguments fallback (path, shape)
    let mut positional_idx = 0;
    for arg in args {
        if let ProviderArgument::Positional(Literal::String(s)) = arg {
            match positional_idx {
                0 if path.is_none() => path = Some(interner.resolve(*s).to_string()),
                1 if shape.is_none() => shape = Some(interner.resolve(*s).to_string()),
                _ => {}
            }
            positional_idx += 1;
        }
    }

    let path = path.ok_or_else(|| {
        ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("SHACL provider requires 'path' argument"),
            ),
            Loc::generated(),
        )
    })?;

    let shape = shape.ok_or_else(|| {
        ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("SHACL provider requires 'shape' argument"),
            ),
            Loc::generated(),
        )
    })?;

    Ok((path, shape))
}

/// Parse SHACL schema content from Turtle format
fn parse_shacl_schema(
    content: &str,
    path: &str,
    interner: &mut Interner,
) -> Result<ShaclSchema<SRDFGraph>, ProviderError> {
    // Parse RDF graph from Turtle
    let rdf_graph = SRDFGraph::from_str(content, &RDFFormat::Turtle, None, &ReaderMode::Lax)
        .map_err(|e| {
            ProviderError::new(
                CompileErrorKind::ProviderError(
                    interner.intern(&format!("Failed to parse Turtle from {}: {}", path, e)),
                ),
                Loc::generated(),
            )
        })?;

    // Parse SHACL schema from RDF graph
    ShaclParser::new(rdf_graph).parse().map_err(|e| {
        ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern(&format!("Failed to parse SHACL schema: {}", e)),
            ),
            Loc::generated(),
        )
    })
}

/// Extract fields from a named SHACL NodeShape
fn extract_shacl_fields(
    schema: &ShaclSchema<SRDFGraph>,
    shape_name: &str,
    interner: &mut Interner,
) -> Result<Vec<ShapeField>, ProviderError> {
    // Iterate through all shapes in the schema
    for (shape_ref, shape) in schema.iter() {
        // Check if this shape matches the requested name
        let shape_iri = shape_ref.to_string();
        let local_name = extract_local_name(&shape_iri);

        if local_name == shape_name || shape_iri.ends_with(shape_name) {
            // Only process NodeShapes (not PropertyShapes at top level)
            if let Shape::NodeShape(node_shape) = shape {
                let mut fields = Vec::new();

                // Get the property shape references from the node shape
                for prop_ref in node_shape.property_shapes() {
                    // Look up the property shape in the schema
                    if let Some(prop_shape) = schema.get_shape(prop_ref) {
                        if let Shape::PropertyShape(ps) = prop_shape {
                            if let Some(field) = extract_property_field(ps) {
                                fields.push(field);
                            }
                        }
                    }
                }

                return Ok(fields);
            }
        }
    }

    Err(ProviderError::new(
        CompileErrorKind::ProviderError(
            interner.intern(&format!("Shape '{}' not found in SHACL schema", shape_name)),
        ),
        Loc::generated(),
    ))
}

/// Extract a field from a SHACL PropertyShape
fn extract_property_field(prop_shape: &PropertyShape<SRDFGraph>) -> Option<ShapeField> {
    // Get sh:path - the predicate IRI
    let path = prop_shape.path();
    let predicate_uri = path.to_string();
    let name = extract_local_name(&predicate_uri);

    // Get sh:datatype from components and convert to Fossil type
    let mut fossil_type = PrimitiveType::String;
    let mut min_count: isize = 0;
    let mut max_count: Option<isize> = None;

    for component in prop_shape.components() {
        match component {
            Component::Datatype(iri_ref) => {
                if let Ok(iri) = iri_ref.get_iri() {
                    fossil_type = xsd_to_fossil_type(&iri.to_string());
                }
            }
            Component::MinCount(n) => {
                min_count = *n;
            }
            Component::MaxCount(n) => {
                max_count = Some(*n);
            }
            _ => {}
        }
    }

    // Determine if field is optional or a list:
    // - Required single: minCount >= 1 AND maxCount == 1
    // - Optional single: minCount == 0 AND maxCount == 1
    // - List: maxCount > 1 OR maxCount is unlimited (None)
    let is_required_single = min_count >= 1 && max_count == Some(1);
    let optional = min_count == 0 && max_count == Some(1);
    let is_list = !is_required_single && !optional;

    Some(ShapeField {
        name,
        predicate_uri,
        fossil_type,
        optional,
        is_list,
    })
}

/// Convert SHACL fields to AST record fields
fn shacl_fields_to_ast_fields(
    shape_fields: Vec<ShapeField>,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Vec<RecordField> {
    shape_fields
        .into_iter()
        .map(|field| {
            let field_name = interner.intern(&field.name);

            // Create the base primitive type
            let base_ty = ast.types.alloc(AstType {
                loc: Loc::generated(),
                kind: AstTypeKind::Primitive(field.fossil_type),
            });

            // Apply cardinality wrapping:
            // - is_list: wrap in List<T>
            // - optional: wrap in Option<T>
            // - neither: use base type directly
            let ty = if field.is_list {
                // List<T>
                ast.types.alloc(AstType {
                    loc: Loc::generated(),
                    kind: AstTypeKind::List(base_ty),
                })
            } else if field.optional {
                // Option<T> using TypeKind::App
                let option_sym = interner.intern("Option");
                ast.types.alloc(AstType {
                    loc: Loc::generated(),
                    kind: AstTypeKind::App {
                        ctor: Path::Simple(option_sym),
                        args: vec![base_ty],
                    },
                })
            } else {
                // Required field: T
                base_ty
            };

            // Build attributes - #[rdf(uri = "...")] for RDF serialization
            let rdf_attr_name = interner.intern("rdf");
            let uri_key = interner.intern("uri");
            let attrs = vec![Attribute {
                name: rdf_attr_name,
                args: vec![AttributeArg {
                    key: uri_key,
                    value: Literal::String(interner.intern(&field.predicate_uri)),
                }],
                loc: Loc::generated(),
            }];

            RecordField {
                name: field_name,
                ty,
                attrs,
            }
        })
        .collect()
}
