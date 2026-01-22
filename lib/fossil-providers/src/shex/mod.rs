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
//!     #[optional]
//!     age: int
//! }
//! ```

use std::fs;
use std::sync::Arc;

use fossil_lang::ast::Loc;
use fossil_lang::ast::ast::{
    Ast, Attribute, AttributeArg, Literal, PrimitiveType, ProviderArgument, RecordField,
    Type as AstType, TypeKind as AstTypeKind,
};
use fossil_lang::ast::thir::{
    Polytype, Type as ThirType, TypeKind as ThirTypeKind, TypeVar, TypedHir,
};
use fossil_lang::context::Interner;
use fossil_lang::error::{CompileErrorKind, ProviderError, RuntimeError};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{
    FunctionDef, ModuleSpec, ProviderOutput, ProviderParamInfo, TypeProviderImpl,
};

use iri_s::IriS;
use shex_ast::ast::{Schema, ShapeExpr, ShapeExprLabel, TripleExpr};
use shex_ast::compact::ShExParser;

use crate::utils::*;

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
    ) -> Result<ProviderOutput, ProviderError> {
        // Parse arguments
        let (path_str, shape_name) = parse_shex_args(args, interner)?;

        // Validate file
        validate_extension(&path_str, &["shex"], interner)?;
        validate_local_file(&path_str, interner)?;

        // Read and parse ShEx file
        let shex_content = fs::read_to_string(&path_str).map_err(|e| {
            ProviderError::new(
                CompileErrorKind::ProviderError(
                    interner.intern(&format!("Failed to read ShEx file: {}", e)),
                ),
                Loc::generated(),
            )
        })?;

        let schema = parse_shex_schema(&shex_content, interner)?;

        // Find the specified shape
        let shape_fields = extract_shape_fields(&schema, &shape_name, interner)?;

        // Convert ShEx fields to AST record fields
        let fields = shex_fields_to_ast_fields(shape_fields, ast, interner);

        // Create AST record type
        let record_ty = ast.types.alloc(AstType {
            loc: Loc::generated(),
            kind: AstTypeKind::Record(fields),
        });

        // Generate module with validate function
        let module_spec = ModuleSpec {
            functions: vec![FunctionDef {
                name: "validate".to_string(),
                implementation: Arc::new(ShexValidateFunction {
                    schema_path: path_str,
                    shape_name,
                }),
            }],
            submodules: vec![],
        };

        Ok(ProviderOutput {
            generated_type: record_ty,
            module_spec: Some(module_spec),
        })
    }
}

/// Parse ShEx provider arguments
fn parse_shex_args(
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
                interner.intern("ShEx provider requires 'path' argument"),
            ),
            Loc::generated(),
        )
    })?;

    let shape = shape.ok_or_else(|| {
        ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern("ShEx provider requires 'shape' argument"),
            ),
            Loc::generated(),
        )
    })?;

    Ok((path, shape))
}

/// Parse ShEx schema content
fn parse_shex_schema(content: &str, interner: &mut Interner) -> Result<Schema, ProviderError> {
    // Create a source IRI for parsing
    let source_iri = IriS::new_unchecked("file:///schema.shex");

    // Parse the ShEx schema
    ShExParser::parse(content, None, &source_iri).map_err(|e| {
        ProviderError::new(
            CompileErrorKind::ProviderError(
                interner.intern(&format!("Failed to parse ShEx schema: {}", e)),
            ),
            Loc::generated(),
        )
    })
}

/// Field information extracted from ShEx
#[derive(Debug)]
struct ShexField {
    /// Field name (derived from predicate local name)
    name: String,
    /// Full predicate IRI
    predicate_uri: String,
    /// Fossil primitive type
    fossil_type: PrimitiveType,
    /// Whether the field is optional (cardinality ?)
    optional: bool,
    /// Whether the field is a list (cardinality * or +)
    is_list: bool,
}

/// Extract fields from a named shape
fn extract_shape_fields(
    schema: &Schema,
    shape_name: &str,
    interner: &mut Interner,
) -> Result<Vec<ShexField>, ProviderError> {
    // Find shape by name in the schema
    let shapes = schema.shapes().ok_or_else(|| {
        ProviderError::new(
            CompileErrorKind::ProviderError(interner.intern("Schema has no shapes defined")),
            Loc::generated(),
        )
    })?;

    // Look for the shape with matching name
    for shape_decl in shapes {
        if shape_label_matches(shape_decl.id(), shape_name) {
            let mut fields = Vec::new();
            // The shape_expr field contains the actual shape expression
            extract_fields_from_shape_expr(&shape_decl.shape_expr, &mut fields, schema);
            return Ok(fields);
        }
    }

    Err(ProviderError::new(
        CompileErrorKind::ProviderError(
            interner.intern(&format!("Shape '{}' not found in schema", shape_name)),
        ),
        Loc::generated(),
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
    fields: &mut Vec<ShexField>,
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
fn extract_fields_from_triple_expr(
    triple_expr: &TripleExpr,
    fields: &mut Vec<ShexField>,
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

            // Cardinality - min/max are i32
            let min_val = min.unwrap_or(1);
            let max_val = *max; // max is already Option<i32>

            let optional = min_val == 0;
            let is_list = max_val.map(|m| m > 1).unwrap_or(true); // unbounded = list

            fields.push(ShexField {
                name: field_name,
                predicate_uri,
                fossil_type,
                optional: optional && !is_list,
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

/// Extract local name from IRI (last segment after # or /)
fn extract_local_name(iri: &str) -> String {
    // Remove angle brackets if present
    let iri = iri.trim_start_matches('<').trim_end_matches('>');

    // Try fragment first
    if let Some(pos) = iri.rfind('#') {
        return iri[pos + 1..].to_string();
    }

    // Then try last path segment
    if let Some(pos) = iri.rfind('/') {
        return iri[pos + 1..].to_string();
    }

    iri.to_string()
}

/// Map ShEx value expression to Fossil type
fn shex_value_to_fossil_type(shape_expr: &ShapeExpr) -> PrimitiveType {
    match shape_expr {
        ShapeExpr::NodeConstraint(nc) => {
            if let Some(dt) = nc.datatype() {
                let iri_str = dt.to_string();
                xsd_str_to_fossil_type(&iri_str)
            } else {
                PrimitiveType::String
            }
        }
        _ => PrimitiveType::String,
    }
}

/// Map XSD datatype string to Fossil primitive type
fn xsd_str_to_fossil_type(iri_str: &str) -> PrimitiveType {
    // Common XSD types
    if iri_str.contains("string") {
        PrimitiveType::String
    } else if iri_str.contains("integer")
        || iri_str.contains("int")
        || iri_str.contains("long")
        || iri_str.contains("short")
    {
        PrimitiveType::Int
    } else if iri_str.contains("float") || iri_str.contains("double") || iri_str.contains("decimal")
    {
        PrimitiveType::Float
    } else if iri_str.contains("boolean") {
        PrimitiveType::Bool
    } else {
        // Default to string for unknown types (including IRIs)
        PrimitiveType::String
    }
}

/// Convert ShEx fields to AST record fields
fn shex_fields_to_ast_fields(
    shex_fields: Vec<ShexField>,
    ast: &mut Ast,
    interner: &mut Interner,
) -> Vec<RecordField> {
    shex_fields
        .into_iter()
        .map(|field| {
            let field_name = interner.intern(&field.name);

            // Create the base type
            let base_ty = ast.types.alloc(AstType {
                loc: Loc::generated(),
                kind: AstTypeKind::Primitive(field.fossil_type),
            });

            // Wrap in List if needed
            let ty = if field.is_list {
                ast.types.alloc(AstType {
                    loc: Loc::generated(),
                    kind: AstTypeKind::List(base_ty),
                })
            } else {
                base_ty
            };

            // Build attributes
            let mut attrs = Vec::new();

            // Add #[rdf(uri = "...")] attribute
            let rdf_attr_name = interner.intern("rdf");
            let uri_key = interner.intern("uri");
            attrs.push(Attribute {
                name: rdf_attr_name,
                args: vec![AttributeArg {
                    key: uri_key,
                    value: Literal::String(interner.intern(&field.predicate_uri)),
                }],
            });

            // Add #[optional] attribute if needed
            if field.optional {
                let optional_attr_name = interner.intern("optional");
                attrs.push(Attribute {
                    name: optional_attr_name,
                    args: vec![],
                });
            }

            RecordField {
                name: field_name,
                ty,
                attrs,
            }
        })
        .collect()
}

/// Validate function implementation that uses rudof for ShEx validation
pub struct ShexValidateFunction {
    schema_path: String,
    shape_name: String,
}

impl FunctionImpl for ShexValidateFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // Signature: forall T. (Entity<T>) -> bool
        let t_var = next_type_var();

        // Create type variable for input
        let input_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Var(t_var),
        });

        // Return type is bool
        let bool_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Primitive(PrimitiveType::Bool),
        });

        // Create function type: (T) -> bool
        let fn_ty = thir.types.alloc(ThirType {
            loc: Loc::generated(),
            kind: ThirTypeKind::Function(vec![input_ty], bool_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        if args.is_empty() {
            return Err(CompileError::new(
                CompileErrorKind::Runtime("validate requires an argument".into()),
                Loc::generated(),
            ));
        }

        // For now, always return true (placeholder implementation)
        // Full validation would require:
        // 1. Converting the Value to RDF triples
        // 2. Loading the ShEx schema with rudof
        // 3. Running the validator
        //
        // This is left as a placeholder because full RDF conversion
        // depends on the Entity representation in the stdlib

        // TODO: Implement full ShEx validation using shex_validation crate

        Ok(Value::Bool(true))
    }
}
