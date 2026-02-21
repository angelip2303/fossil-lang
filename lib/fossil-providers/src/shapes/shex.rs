use fossil_lang::ast::Loc;
use fossil_lang::ast::{Attribute, AttributeArg, Literal, PrimitiveType};
use fossil_lang::context::Interner;
use fossil_lang::error::{FossilError, FossilWarning, FossilWarnings};
use fossil_lang::traits::provider::{
    FieldSpec, FieldType, FileReader, ProviderArgs, ProviderContext, ProviderInfo, ProviderKind,
    ProviderOutput, ProviderParamInfo, ProviderSchema, TypeProviderImpl,
};

use iri_s::IriS;
use shex_ast::ast::{ObjectValue, Schema, ShapeExpr, ShapeExprLabel, StringFacet, TripleExpr, ValueSetValue, XsFacet};
use shex_ast::compact::ShExParser;

use crate::shapes::{ShapeField, ValidateValue, extract_local_name, xsd_to_fossil_type};
use crate::utils::{resolve_path, validate_extension, validate_path};

const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

pub struct ShexProvider;

impl TypeProviderImpl for ShexProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo { extensions: &["shex"], kind: ProviderKind::Schema }
    }

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

    fn type_identity(&self, args: &ProviderArgs, reader: &dyn FileReader) -> Option<String> {
        let path_str = args.get_string("path")?;
        let shape_name = args.get_string("shape")?;
        let path = resolve_path(path_str);
        let content = reader.read_to_string(path.to_str()).ok()?;
        let schema = parse_shex_schema(&content, Loc::generated()).ok()?;
        let shapes = schema.shapes()?;
        for shape_decl in shapes {
            if shape_label_matches(shape_decl.id(), shape_name) {
                return match shape_decl.id() {
                    ShapeExprLabel::IriRef { value } => Some(value.to_string()),
                    ShapeExprLabel::BNode { value } => Some(value.to_string()),
                    ShapeExprLabel::Start => Some("Start".to_string()),
                };
            }
        }
        None
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
        let shex_content = ctx.file_reader.read_to_string(&path_str)
            .map_err(|e| FossilError::read_error(path_str.clone(), e, loc))?;

        let schema = parse_shex_schema(&shex_content, loc)?;
        let mut type_attrs = extract_base_attribute(&schema, ctx.interner, loc);
        let extraction = extract_shape_fields(&schema, shape_name, loc)?;
        let fields = shex_fields_to_field_specs(extraction.fields, ctx.interner, loc);

        if let Some(rdf_type_iri) = &extraction.rdf_type {
            type_attrs.push(rdf_attribute(ctx.interner, "type", rdf_type_iri, loc));
        }

        Ok(ProviderOutput::new(ProviderSchema { fields })
            .with_warnings(extraction.warnings)
            .with_type_attributes(type_attrs))
    }
}

fn parse_shex_schema(content: &str, loc: Loc) -> Result<Schema, FossilError> {
    let source_iri = IriS::new_unchecked("file:///schema.shex");
    ShExParser::parse(content, None, &source_iri)
        .map_err(|e| FossilError::parse_error("ShEx", e.to_string(), loc))
}

/// Build an `#[rdf(key = "value")]` attribute.
fn rdf_attribute(interner: &mut Interner, key: &str, value: &str, loc: Loc) -> Attribute {
    let rdf_sym = interner.intern("rdf");
    let key_sym = interner.intern(key);
    let value_sym = interner.intern(value);
    Attribute {
        name: rdf_sym,
        args: vec![AttributeArg::Named {
            key: key_sym,
            value: Literal::String(value_sym),
        }],
        loc,
    }
}

fn extract_base_attribute(schema: &Schema, interner: &mut Interner, loc: Loc) -> Vec<Attribute> {
    let Some(base_iri) = schema.base() else {
        return Vec::new();
    };
    vec![rdf_attribute(interner, "base", &base_iri.to_string(), loc)]
}

struct ShapeExtractionResult {
    fields: Vec<ShapeField>,
    rdf_type: Option<String>,
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
            let mut rdf_type = None;
            let mut warnings = FossilWarnings::new();
            extract_fields_from_shape_expr(
                &shape_decl.shape_expr,
                &mut fields,
                &mut rdf_type,
                &mut warnings,
                schema,
                shape_name,
                loc,
            );
            return Ok(ShapeExtractionResult { fields, rdf_type, warnings });
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
    rdf_type: &mut Option<String>,
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
                    rdf_type,
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
                    &se_wrapper.se, fields, rdf_type, warnings, schema, shape_name, loc,
                );
            }
        }
        ShapeExpr::ShapeOr { shape_exprs } => {
            if let Some(first) = shape_exprs.first() {
                extract_fields_from_shape_expr(
                    &first.se, fields, rdf_type, warnings, schema, shape_name, loc,
                );
            }
        }
        ShapeExpr::Ref(reference) => {
            if let Some(shapes) = schema.shapes() {
                for shape_decl in shapes {
                    if shape_decl.id() == reference {
                        extract_fields_from_shape_expr(
                            &shape_decl.shape_expr, fields, rdf_type, warnings, schema, shape_name, loc,
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
    rdf_type: &mut Option<String>,
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
                *rdf_type = value_expr
                    .as_ref()
                    .and_then(|ve| extract_iri_from_shape_expr(ve.as_ref()));
                if rdf_type.is_none() {
                    warnings.push(FossilWarning::generic(
                        format!("shape '{}': rdf:type constraint found but could not extract type IRI", shape_name),
                        loc,
                    ));
                }
                return;
            }

            let field_name = extract_local_name(&predicate_uri);

            let info = value_expr
                .as_ref()
                .map(|ve| extract_value_expr_info(ve.as_ref()))
                .unwrap_or(ValueExprInfo {
                    ty: ValueExprType::Primitive(PrimitiveType::String),
                    validate_args: Vec::new(),
                });

            let base = match info.ty {
                ValueExprType::Primitive(p) => FieldType::Primitive(p),
                ValueExprType::ShapeRef(name) => FieldType::Named(name),
            };
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
                    &expr_wrapper.te, fields, rdf_type, warnings, schema, shape_name, loc,
                );
            }
        }

        _ => {}
    }
}

/// Extract an IRI from a ShapeExpr value expression.
///
/// Supports both syntactic forms of rdf:type constraints in ShEx:
/// - `a [ex:Vehicle]` → `NodeConstraint` with a value set containing the IRI
/// - `a ex:Vehicle`   → `Ref` pointing to a shape label IRI
fn extract_iri_from_shape_expr(shape_expr: &ShapeExpr) -> Option<String> {
    match shape_expr {
        ShapeExpr::NodeConstraint(nc) => {
            nc.values()?
                .iter()
                .find_map(|vsv| match vsv {
                    ValueSetValue::ObjectValue(ObjectValue::IriRef(iri)) => {
                        Some(iri.to_string())
                    }
                    _ => None,
                })
        }
        ShapeExpr::Ref(ShapeExprLabel::IriRef { value }) => Some(value.to_string()),
        _ => None,
    }
}

enum ValueExprType {
    Primitive(PrimitiveType),
    ShapeRef(String),
}

struct ValueExprInfo {
    ty: ValueExprType,
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
                ty: ValueExprType::Primitive(primitive_type),
                validate_args,
            }
        }
        ShapeExpr::Ref(label) => {
            let name = match label {
                ShapeExprLabel::IriRef { value } => value.to_string(),
                ShapeExprLabel::BNode { value } => value.to_string(),
                ShapeExprLabel::Start => "Start".to_string(),
            };
            ValueExprInfo {
                ty: ValueExprType::ShapeRef(name),
                validate_args: Vec::new(),
            }
        }
        _ => ValueExprInfo {
            ty: ValueExprType::Primitive(PrimitiveType::String),
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

            let mut attrs = vec![rdf_attribute(interner, "uri", &field.predicate_uri, loc)];

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
