use std::fs;

use fossil_lang::ast::Loc;
use fossil_lang::ast::{Attribute, AttributeArg, Literal, PrimitiveType};
use fossil_lang::context::Interner;
use fossil_lang::error::{FossilError, FossilWarning, FossilWarnings};
use fossil_lang::traits::provider::{
    FieldSpec, FieldType, ProviderArgs, ProviderContext, ProviderOutput, ProviderParamInfo,
    ProviderSchema, TypeProviderImpl,
};

use iri_s::IriS;
use shex_ast::ast::{Schema, ShapeExpr, ShapeExprLabel, StringFacet, TripleExpr, XsFacet};
use shex_ast::compact::ShExParser;

use crate::shapes::{ShapeField, ValidateValue, extract_local_name, xsd_to_fossil_type};
use crate::utils::{resolve_path, validate_extension, validate_path};

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
        args: &ProviderArgs,
        ctx: &mut ProviderContext,
        _type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError> {
        let path_str = args.require_string("path", "shex", loc)?;
        let path = resolve_path(path_str);
        validate_extension(path.as_ref(), &["shex"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let shape_name = args.require_string("shape", "shex", loc)?;

        let path_str = path.to_str().to_string();
        let shex_content = fs::read_to_string(&path_str)
            .map_err(|e| FossilError::read_error(path_str.clone(), e.to_string(), loc))?;

        let schema = parse_shex_schema(&shex_content, loc)?;
        let base_attrs = extract_base_attribute(&schema, ctx.interner, loc);
        let extraction = extract_shape_fields(&schema, shape_name, loc)?;
        let fields = shex_fields_to_field_specs(extraction.fields, ctx.interner, loc);

        Ok(ProviderOutput::new(ProviderSchema { fields })
            .with_warnings(extraction.warnings)
            .with_type_attributes(base_attrs))
    }
}

fn parse_shex_schema(content: &str, loc: Loc) -> Result<Schema, FossilError> {
    let source_iri = IriS::new_unchecked("file:///schema.shex");
    ShExParser::parse(content, None, &source_iri)
        .map_err(|e| FossilError::parse_error("ShEx", e.to_string(), loc))
}

fn extract_base_attribute(schema: &Schema, interner: &mut Interner, loc: Loc) -> Vec<Attribute> {
    let Some(base_iri) = schema.base() else {
        return Vec::new();
    };
    let rdf_sym = interner.intern("rdf");
    let base_key = interner.intern("base");
    vec![Attribute {
        name: rdf_sym,
        args: vec![AttributeArg::Named {
            key: base_key,
            value: Literal::String(interner.intern(&base_iri.to_string())),
        }],
        loc,
    }]
}

struct ShapeExtractionResult {
    fields: Vec<ShapeField>,
    warnings: FossilWarnings,
}

fn extract_shape_fields(
    schema: &Schema,
    shape_name: &str,
    loc: Loc,
) -> Result<ShapeExtractionResult, FossilError> {
    let shapes = schema
        .shapes()
        .ok_or_else(|| FossilError::data_error("schema has no shapes defined", loc))?;

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
                loc,
            );
            return Ok(ShapeExtractionResult { fields, warnings });
        }
    }

    Err(FossilError::undefined("shape", shape_name, loc))
}

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

fn extract_fields_from_shape_expr(
    shape_expr: &ShapeExpr,
    fields: &mut Vec<ShapeField>,
    warnings: &mut FossilWarnings,
    schema: &Schema,
    shape_name: &str,
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
                    loc,
                );
            }
        }
        ShapeExpr::ShapeAnd { shape_exprs } => {
            for se_wrapper in shape_exprs {
                extract_fields_from_shape_expr(
                    &se_wrapper.se, fields, warnings, schema, shape_name, loc,
                );
            }
        }
        ShapeExpr::ShapeOr { shape_exprs } => {
            if let Some(first) = shape_exprs.first() {
                extract_fields_from_shape_expr(
                    &first.se, fields, warnings, schema, shape_name, loc,
                );
            }
        }
        ShapeExpr::Ref(reference) => {
            if let Some(shapes) = schema.shapes() {
                for shape_decl in shapes {
                    if shape_decl.id() == reference {
                        extract_fields_from_shape_expr(
                            &shape_decl.shape_expr, fields, warnings, schema, shape_name, loc,
                        );
                        break;
                    }
                }
            }
        }
        _ => {}
    }
}

fn extract_fields_from_triple_expr(
    triple_expr: &TripleExpr,
    fields: &mut Vec<ShapeField>,
    warnings: &mut FossilWarnings,
    schema: &Schema,
    shape_name: &str,
    loc: Loc,
) {
    match triple_expr {
        TripleExpr::TripleConstraint {
            predicate,
            value_expr,
            min,
            ..
        } => {
            let predicate_uri = predicate.to_string();

            if predicate_uri == RDF_TYPE {
                warnings.push(FossilWarning::generic(
                    format!("shape '{}': rdf:type constraint ignored (use #[rdf(type)] instead)", shape_name),
                    loc,
                ));
                return;
            }

            let field_name = extract_local_name(&predicate_uri);

            let info = value_expr
                .as_ref()
                .map(|ve| extract_value_expr_info(ve.as_ref()))
                .unwrap_or(ValueExprInfo {
                    primitive_type: PrimitiveType::String,
                    validate_args: Vec::new(),
                });

            let base = FieldType::Primitive(info.primitive_type);
            let min_val = min.unwrap_or(1);
            let ty = if min_val == 0 {
                FieldType::Optional(Box::new(base))
            } else {
                base
            };

            fields.push(ShapeField {
                name: field_name,
                predicate_uri,
                ty,
                validate_args: info.validate_args,
            });
        }

        TripleExpr::EachOf { expressions, .. } | TripleExpr::OneOf { expressions, .. } => {
            for expr_wrapper in expressions {
                extract_fields_from_triple_expr(
                    &expr_wrapper.te, fields, warnings, schema, shape_name, loc,
                );
            }
        }

        _ => {}
    }
}

struct ValueExprInfo {
    primitive_type: PrimitiveType,
    validate_args: Vec<(String, ValidateValue)>,
}

fn extract_value_expr_info(shape_expr: &ShapeExpr) -> ValueExprInfo {
    match shape_expr {
        ShapeExpr::NodeConstraint(nc) => {
            let primitive_type = if let Some(dt) = nc.datatype() {
                xsd_to_fossil_type(&dt.to_string())
            } else {
                PrimitiveType::String
            };

            let mut validate_args = Vec::new();
            if let Some(facets) = nc.xs_facet() {
                for facet in facets {
                    if let XsFacet::StringFacet(sf) = facet {
                        match sf {
                            StringFacet::MinLength(n) => {
                                validate_args
                                    .push(("min_length".into(), ValidateValue::Int(n as i64)));
                            }
                            StringFacet::MaxLength(n) => {
                                validate_args
                                    .push(("max_length".into(), ValidateValue::Int(n as i64)));
                            }
                            StringFacet::Pattern(pat) => {
                                validate_args
                                    .push(("pattern".into(), ValidateValue::Str(pat.str.clone())));
                            }
                            StringFacet::Length(_) => {}
                        }
                    }
                }
            }

            ValueExprInfo {
                primitive_type,
                validate_args,
            }
        }
        _ => ValueExprInfo {
            primitive_type: PrimitiveType::String,
            validate_args: Vec::new(),
        },
    }
}

fn shex_fields_to_field_specs(
    shape_fields: Vec<ShapeField>,
    interner: &mut Interner,
    loc: Loc,
) -> Vec<FieldSpec> {
    shape_fields
        .into_iter()
        .map(|field| {
            let field_name = interner.intern(&field.name);

            let rdf_attr_name = interner.intern("rdf");
            let uri_key = interner.intern("uri");
            let mut attrs = vec![Attribute {
                name: rdf_attr_name,
                args: vec![AttributeArg::Named {
                    key: uri_key,
                    value: Literal::String(interner.intern(&field.predicate_uri)),
                }],
                loc,
            }];

            if !field.validate_args.is_empty() {
                let validate_name = interner.intern("validate");
                let validate_args = field
                    .validate_args
                    .iter()
                    .map(|(key, value)| {
                        let key_sym = interner.intern(key);
                        let literal = match value {
                            ValidateValue::Int(n) => Literal::Integer(*n),
                            ValidateValue::Str(s) => Literal::String(interner.intern(s)),
                        };
                        AttributeArg::Named {
                            key: key_sym,
                            value: literal,
                        }
                    })
                    .collect();

                attrs.push(Attribute {
                    name: validate_name,
                    args: validate_args,
                    loc,
                });
            }

            FieldSpec {
                name: field_name,
                ty: field.ty,
                attrs,
            }
        })
        .collect()
}
