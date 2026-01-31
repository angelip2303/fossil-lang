//! ShEx Type Provider
//!
//! This provider reads ShEx (Shape Expressions) schema files and generates
//! Fossil record types with field-level URI attributes for RDF serialization.
//!
//! # Usage
//!
//! ```fossil
//! type Person = shex!("schema.shex", shape: "PersonShape")
//! ```
//!
//! # Generated Output
//!
//! Given a ShEx shape like:
//! ```shex
//! PREFIX foaf: <http://xmlns.com/foaf/0.1/>
//!
//! PersonShape {
//!     foaf:name xsd:string ;
//!     foaf:age xsd:integer ?
//! }
//! ```
//!
//! The provider generates:
//! ```fossil
//! type Person = {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/age")]
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
use fossil_lang::error::{ProviderError, ProviderErrorKind};
use fossil_lang::traits::provider::{ProviderOutput, ProviderParamInfo, TypeProviderImpl};

use iri_s::IriS;
use polars::prelude::PlPath;
use shex_ast::ast::{Schema, ShapeExpr, ShapeExprLabel, TripleExpr};
use shex_ast::compact::ShExParser;

use crate::shapes::{ShapeField, extract_local_name, xsd_to_fossil_type};
use crate::utils::{extract_string_path, provider_err, validate_extension, validate_path};

/// ShEx Type Provider
///
/// Reads ShEx schema files and generates Fossil record types with URI attributes.
pub struct ShexProvider;

impl TypeProviderImpl for ShexProvider {
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
        loc: Loc,
    ) -> Result<ProviderOutput, ProviderError> {
        let (path, shape_name) = parse_shex_args(args, interner, loc)?;
        validate_extension(path.as_ref(), &["shex"])
            .map_err(|kind| provider_err(kind, loc))?;
        validate_path(path.as_ref())
            .map_err(|kind| provider_err(kind, loc))?;

        let path_str = path.to_str().to_string();
        let shex_content = fs::read_to_string(&path_str).map_err(|e| {
            provider_err(
                ProviderErrorKind::ReadError {
                    path: path_str.clone(),
                    cause: e.to_string(),
                },
                loc,
            )
        })?;

        let schema = parse_shex_schema(&shex_content, loc)?;

        // Find the specified shape
        let shape_fields = extract_shape_fields(&schema, &shape_name, loc)?;

        // Convert ShEx fields to AST record fields
        let fields = shex_fields_to_ast_fields(shape_fields, ast, interner, loc);

        // Create AST record type with the provider invocation location
        let record_ty = ast.types.alloc(AstType {
            loc,
            kind: AstTypeKind::Record(fields),
        });

        Ok(ProviderOutput {
            generated_type: record_ty,
            module_spec: None,
        })
    }
}

/// Parse ShEx provider arguments
fn parse_shex_args(
    args: &[ProviderArgument],
    interner: &Interner,
    loc: Loc,
) -> Result<(PlPath, String), ProviderError> {
    let mut iter = args.iter().take(2);

    // First argument: path (positional)
    let Some(path_arg) = iter.next() else {
        return Err(provider_err(
            ProviderErrorKind::MissingArgument {
                name: "path",
                provider: "shex",
            },
            loc,
        ));
    };

    let ProviderArgument::Positional(lit) = path_arg else {
        return Err(provider_err(
            ProviderErrorKind::MissingArgument {
                name: "path",
                provider: "shex",
            },
            loc,
        ));
    };

    let path = extract_string_path(lit, interner)
        .map_err(|kind| provider_err(kind, loc))?;

    // Second argument: shape (named)
    let Some(shape_arg) = iter.next() else {
        return Err(provider_err(
            ProviderErrorKind::MissingArgument {
                name: "shape",
                provider: "shex",
            },
            loc,
        ));
    };

    let shape = match shape_arg {
        ProviderArgument::Named { name, value } if interner.resolve(*name) == "shape" => {
            if let Literal::String(s) = value {
                interner.resolve(*s).to_string()
            } else {
                return Err(provider_err(
                    ProviderErrorKind::InvalidArgumentType {
                        name: "shape",
                        expected: "a string",
                    },
                    loc,
                ));
            }
        }
        _ => {
            return Err(provider_err(
                ProviderErrorKind::MissingArgument {
                    name: "shape",
                    provider: "shex",
                },
                loc,
            ))
        }
    };

    Ok((path, shape))
}

/// Parse ShEx schema content
fn parse_shex_schema(content: &str, loc: Loc) -> Result<Schema, ProviderError> {
    let source_iri = IriS::new_unchecked("file:///schema.shex");
    ShExParser::parse(content, None, &source_iri).map_err(|e| {
        provider_err(
            ProviderErrorKind::ParseError {
                format: "ShEx",
                cause: e.to_string(),
            },
            loc,
        )
    })
}

// Using ShapeField from crate::shapes

/// Extract fields from a named shape
fn extract_shape_fields(
    schema: &Schema,
    shape_name: &str,
    loc: Loc,
) -> Result<Vec<ShapeField>, ProviderError> {
    let shapes = schema
        .shapes()
        .ok_or_else(|| provider_err(ProviderErrorKind::NoShapesDefined, loc))?;

    for shape_decl in shapes {
        if shape_label_matches(shape_decl.id(), shape_name) {
            let mut fields = Vec::new();
            extract_fields_from_shape_expr(&shape_decl.shape_expr, &mut fields, schema);
            return Ok(fields);
        }
    }

    Err(provider_err(
        ProviderErrorKind::ShapeNotFound {
            name: shape_name.to_string(),
        },
        loc,
    ))
}

/// Check if a shape label matches the given name
fn shape_label_matches(label: &ShapeExprLabel, name: &str) -> bool {
    match label {
        ShapeExprLabel::IriRef { value } => {
            let iri_str = value.to_string();
            extract_local_name(&iri_str) == name || iri_str.ends_with(name)
        }
        ShapeExprLabel::BNode { value } => value.to_string() == name,
        ShapeExprLabel::Start => name == "start" || name == "Start",
    }
}

/// Recursively extract fields from a shape expression
fn extract_fields_from_shape_expr(
    shape_expr: &ShapeExpr,
    fields: &mut Vec<ShapeField>,
    schema: &Schema,
) {
    match shape_expr {
        // Shape is a tuple variant with a Shape struct inside
        ShapeExpr::Shape(shape) => {
            if let Some(triple_expr_wrapper) = &shape.expression {
                extract_fields_from_triple_expr(&triple_expr_wrapper.te, fields, schema);
            }
        }
        ShapeExpr::ShapeAnd { shape_exprs } => {
            for se_wrapper in shape_exprs {
                extract_fields_from_shape_expr(&se_wrapper.se, fields, schema);
            }
        }
        ShapeExpr::ShapeOr { shape_exprs } => {
            // For OR, take fields from first branch
            if let Some(first) = shape_exprs.first() {
                extract_fields_from_shape_expr(&first.se, fields, schema);
            }
        }
        ShapeExpr::Ref(reference) => {
            // Follow reference
            if let Some(shapes) = schema.shapes() {
                for shape_decl in shapes {
                    if shape_decl.id() == reference {
                        extract_fields_from_shape_expr(&shape_decl.shape_expr, fields, schema);
                        break;
                    }
                }
            }
        }
        _ => {}
    }
}

/// Extract fields from triple expression
#[allow(clippy::only_used_in_recursion)]
fn extract_fields_from_triple_expr(
    triple_expr: &TripleExpr,
    fields: &mut Vec<ShapeField>,
    schema: &Schema,
) {
    match triple_expr {
        TripleExpr::TripleConstraint {
            predicate,
            value_expr,
            min,
            max,
            ..
        } => {
            // Get predicate URI and derive field name
            let predicate_uri = predicate.to_string();
            let field_name = extract_local_name(&predicate_uri);

            // Determine Fossil type from value expression
            let fossil_type = value_expr
                .as_ref()
                .map(|ve| shex_value_to_fossil_type(ve.as_ref()))
                .unwrap_or(PrimitiveType::String);

            // Cardinality mapping:
            // | ShEx      | min | max | Fossil Type  |
            // |-----------|-----|-----|--------------|
            // | (default) | 1   | 1   | T            |
            // | ?         | 0   | 1   | Option<T>    |
            // | *         | 0   | -1  | [T] (List)   |
            // | +         | 1   | -1  | [T] (List)   |
            // Note: In ShEx, -1 means unbounded, default cardinality is {1,1}
            let min_val = min.unwrap_or(1);
            let max_val = max.unwrap_or(1);

            // is_list: max == -1 (unbounded) OR max > 1
            let is_list = max_val == -1 || max_val > 1;

            // optional: only when min=0 AND max=1 (the ? cardinality)
            let optional = min_val == 0 && max_val == 1;

            fields.push(ShapeField {
                name: field_name,
                predicate_uri,
                fossil_type,
                optional,
                is_list,
            });
        }
        TripleExpr::EachOf { expressions, .. } | TripleExpr::OneOf { expressions, .. } => {
            for expr_wrapper in expressions {
                extract_fields_from_triple_expr(&expr_wrapper.te, fields, schema);
            }
        }
        TripleExpr::TripleExprRef(_) => {
            // Skip references to triple expressions for now
        }
    }
}

// extract_local_name is now imported from crate::shapes

/// Map ShEx value expression to Fossil type
fn shex_value_to_fossil_type(shape_expr: &ShapeExpr) -> PrimitiveType {
    match shape_expr {
        ShapeExpr::NodeConstraint(nc) => {
            if let Some(dt) = nc.datatype() {
                let iri_str = dt.to_string();
                xsd_to_fossil_type(&iri_str)
            } else {
                PrimitiveType::String
            }
        }
        _ => PrimitiveType::String,
    }
}

// xsd_to_fossil_type is now imported from crate::shapes

/// Convert ShEx fields to AST record fields
///
/// Uses the provider invocation location for all generated types so that
/// errors can be traced back to the source.
fn shex_fields_to_ast_fields(
    shape_fields: Vec<ShapeField>,
    ast: &mut Ast,
    interner: &mut Interner,
    loc: Loc,
) -> Vec<RecordField> {
    shape_fields
        .into_iter()
        .map(|field| {
            let field_name = interner.intern(&field.name);

            // Create the base primitive type
            let base_ty = ast.types.alloc(AstType {
                loc,
                kind: AstTypeKind::Primitive(field.fossil_type),
            });

            // Apply cardinality wrapping:
            // - is_list: wrap in List<T>
            // - optional: wrap in Option<T>
            // - neither: use base type directly
            let ty = if field.is_list {
                // List<T>
                ast.types.alloc(AstType {
                    loc,
                    kind: AstTypeKind::List(base_ty),
                })
            } else if field.optional {
                // Option<T> using TypeKind::App
                let option_sym = interner.intern("Option");
                ast.types.alloc(AstType {
                    loc,
                    kind: AstTypeKind::App {
                        ctor: Path::Simple(option_sym),
                        args: vec![base_ty],
                    },
                })
            } else {
                // Required field: T
                base_ty
            };

            // Build attributes - only #[rdf(uri = "...")] now
            // Option<T> expresses optionality in the type, no need for #[optional] attribute
            let rdf_attr_name = interner.intern("rdf");
            let uri_key = interner.intern("uri");
            let attrs = vec![Attribute {
                name: rdf_attr_name,
                args: vec![AttributeArg {
                    key: uri_key,
                    value: Literal::String(interner.intern(&field.predicate_uri)),
                }],
                loc,
            }];

            RecordField {
                name: field_name,
                ty,
                attrs,
            }
        })
        .collect()
}
