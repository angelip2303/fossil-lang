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
use fossil_lang::error::{FossilError, FossilWarning, FossilWarnings};
use fossil_lang::traits::provider::{
    ProviderOutput, ProviderParamInfo, TypeProviderImpl, resolve_args,
};

use iri_s::IriS;
use polars::prelude::PlPath;
use shex_ast::ast::{Schema, ShapeExpr, ShapeExprLabel, TripleExpr};
use shex_ast::compact::ShExParser;

use crate::shapes::{ShapeField, extract_local_name, xsd_to_fossil_type};
use crate::utils::{extract_string_path, validate_extension, validate_path};

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

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
    ) -> Result<ProviderOutput, FossilError> {
        let (path, shape_name) = parse_shex_args(args, &self.param_info(), interner, loc)?;
        validate_extension(path.as_ref(), &["shex"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let path_str = path.to_str().to_string();
        let shex_content = fs::read_to_string(&path_str)
            .map_err(|e| FossilError::read_error(path_str.clone(), e.to_string(), loc))?;

        let schema = parse_shex_schema(&shex_content, loc)?;

        let extraction = extract_shape_fields(&schema, &shape_name, ast, interner, loc)?;

        let fields = shex_fields_to_record_fields(extraction.fields, interner, loc);

        let record_ty = ast.types.alloc(AstType {
            loc,
            kind: AstTypeKind::Record(fields),
        });

        Ok(ProviderOutput::new(record_ty).with_warnings(extraction.warnings))
    }
}

/// Parse ShEx provider arguments using resolve_args
fn parse_shex_args(
    args: &[ProviderArgument],
    param_info: &[ProviderParamInfo],
    interner: &Interner,
    loc: Loc,
) -> Result<(PlPath, String), FossilError> {
    let resolved = resolve_args(args, param_info, interner, "shex", loc)?;

    // Path is first positional argument (index 0)
    let path_lit = resolved.get_positional(0).expect("path is required");
    let path = extract_string_path(path_lit, interner, loc)?;

    // Shape is second argument - can be positional (index 1) or named
    let shape = if let Some(shape_sym) = interner.lookup("shape") {
        resolved.get_named_string(shape_sym, interner)
    } else {
        None
    };

    // If not found as named, try positional index 1
    let shape = shape.or_else(|| resolved.get_positional_string(1, interner));
    let shape = shape.expect("shape is required");

    Ok((path, shape))
}

/// Parse ShEx schema content
fn parse_shex_schema(content: &str, loc: Loc) -> Result<Schema, FossilError> {
    let source_iri = IriS::new_unchecked("file:///schema.shex");
    ShExParser::parse(content, None, &source_iri)
        .map_err(|e| FossilError::parse_error("ShEx", e.to_string(), loc))
}

struct ShapeExtractionResult {
    fields: Vec<ShapeField>,
    warnings: FossilWarnings,
}

fn extract_shape_fields(
    schema: &Schema,
    shape_name: &str,
    ast: &mut Ast,
    interner: &mut Interner,
    loc: Loc,
) -> Result<ShapeExtractionResult, FossilError> {
    let shapes = schema
        .shapes()
        .ok_or_else(|| FossilError::no_shapes_defined(loc))?;

    for shape_decl in shapes {
        if shape_label_matches(shape_decl.id(), shape_name) {
            let mut fields = Vec::new();
            let mut warnings = FossilWarnings::new();
            extract_fields_from_shape_expr(
                &shape_decl.shape_expr,
                &mut fields,
                &mut warnings,
                schema,
                shape_name,
                ast,
                interner,
                loc,
            );
            return Ok(ShapeExtractionResult { fields, warnings });
        }
    }

    Err(FossilError::shape_not_found(shape_name, loc))
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
    warnings: &mut FossilWarnings,
    schema: &Schema,
    shape_name: &str,
    ast: &mut Ast,
    interner: &mut Interner,
    loc: Loc,
) {
    match shape_expr {
        ShapeExpr::Shape(shape) => {
            if let Some(triple_expr_wrapper) = &shape.expression {
                extract_fields_from_triple_expr(
                    &triple_expr_wrapper.te,
                    fields,
                    warnings,
                    schema,
                    shape_name,
                    ast,
                    interner,
                    loc,
                );
            }
        }
        ShapeExpr::ShapeAnd { shape_exprs } => {
            for se_wrapper in shape_exprs {
                extract_fields_from_shape_expr(
                    &se_wrapper.se,
                    fields,
                    warnings,
                    schema,
                    shape_name,
                    ast,
                    interner,
                    loc,
                );
            }
        }
        ShapeExpr::ShapeOr { shape_exprs } => {
            // For OR, take fields from first branch
            if let Some(first) = shape_exprs.first() {
                extract_fields_from_shape_expr(
                    &first.se, fields, warnings, schema, shape_name, ast, interner, loc,
                );
            }
        }
        ShapeExpr::Ref(reference) => {
            // Follow reference
            if let Some(shapes) = schema.shapes() {
                for shape_decl in shapes {
                    if shape_decl.id() == reference {
                        extract_fields_from_shape_expr(
                            &shape_decl.shape_expr,
                            fields,
                            warnings,
                            schema,
                            shape_name,
                            ast,
                            interner,
                            loc,
                        );
                        break;
                    }
                }
            }
        }
        _ => {}
    }
}

#[allow(clippy::too_many_arguments)]
fn extract_fields_from_triple_expr(
    triple_expr: &TripleExpr,
    fields: &mut Vec<ShapeField>,
    warnings: &mut FossilWarnings,
    schema: &Schema,
    shape_name: &str,
    ast: &mut Ast,
    interner: &mut Interner,
    loc: Loc,
) {
    match triple_expr {
        TripleExpr::TripleConstraint {
            predicate,
            value_expr,
            min,
            max,
            ..
        } => {
            let predicate_uri = predicate.to_string();

            // Skip rdf:type constraints
            if predicate_uri == RDF_TYPE {
                warnings.push(FossilWarning::shex_rdf_type_ignored(shape_name, loc));
                return;
            }

            let field_name = extract_local_name(&predicate_uri);

            // Determine base primitive type from value expression
            let primitive_type = value_expr
                .as_ref()
                .map(|ve| shex_value_to_fossil_type(ve.as_ref()))
                .unwrap_or(PrimitiveType::String);

            // Create base type
            let base_ty = ast.types.alloc(AstType {
                loc,
                kind: AstTypeKind::Primitive(primitive_type),
            });

            // Apply cardinality wrapper to get final type
            // | ShEx      | min | max | Fossil Type  |
            // |-----------|-----|-----|--------------|
            // | (default) | 1   | 1   | T            |
            // | ?         | 0   | 1   | Option<T>    |
            // | *         | 0   | -1  | [T]          |
            // | +         | 1   | -1  | [T]          |
            let min_val = min.unwrap_or(1);
            let max_val = max.unwrap_or(1);

            let ty = if max_val == -1 || max_val > 1 {
                // List: [T]
                ast.types.alloc(AstType {
                    loc,
                    kind: AstTypeKind::List(base_ty),
                })
            } else if min_val == 0 && max_val == 1 {
                // Optional: Option<T>
                let option_sym = interner.intern("Option");
                ast.types.alloc(AstType {
                    loc,
                    kind: AstTypeKind::App {
                        ctor: Path::Simple(option_sym),
                        args: vec![base_ty],
                    },
                })
            } else {
                // Required: T
                base_ty
            };

            fields.push(ShapeField {
                name: field_name,
                predicate_uri,
                ty,
            });
        }

        TripleExpr::EachOf { expressions, .. } | TripleExpr::OneOf { expressions, .. } => {
            for expr_wrapper in expressions {
                extract_fields_from_triple_expr(
                    &expr_wrapper.te,
                    fields,
                    warnings,
                    schema,
                    shape_name,
                    ast,
                    interner,
                    loc,
                );
            }
        }

        TripleExpr::TripleExprRef(_) => {
            unimplemented!("Expressions referencing other shapes are not supported yet")
        }
    }
}

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

/// Convert ShapeFields to RecordFields (just adds attributes, type is already built)
fn shex_fields_to_record_fields(
    shape_fields: Vec<ShapeField>,
    interner: &mut Interner,
    loc: Loc,
) -> Vec<RecordField> {
    shape_fields
        .into_iter()
        .map(|field| {
            let field_name = interner.intern(&field.name);

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
                ty: field.ty,
                attrs,
            }
        })
        .collect()
}
