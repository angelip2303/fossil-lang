pub mod metadata;
pub mod serializer;

pub use metadata::RdfMetadata;
pub use serializer::{OutputFormat, RdfBatchWriter};

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

/// Rdf::serialize function implementation
///
/// Signature: (Records, string) -> Unit
///
/// Serializes a RecordsPlan to an RDF file using metadata from type-level
/// and field-level attributes.
///
/// # Type-Level Attributes
///
/// ```fossil
/// #[rdf(type = "http://schema.org/Person", id = "http://example.org/${id}")]
/// type Person = shex!("person.shex", shape: "PersonShape")
/// ```
///
/// - `type`: Generates rdf:type triples for each entity
/// - `id`: Template for subject URIs (blank nodes if omitted)
///
/// # Field-Level Attributes
///
/// ```fossil
/// type Person = {
///     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
///     name: string,
/// }
/// ```
pub struct RdfSerializeFunction;

impl FunctionImpl for RdfSerializeFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T, String) -> Unit
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        let filename_ty = ir.string_type();
        let output_ty = ir.unit_type();
        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty, filename_ty], output_ty))
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let mut args_iter = args.into_iter();

        let records_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "Rdf::serialize requires records and filename".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        let filename = args_iter
            .next()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime(
                        "Rdf::serialize filename must be a string literal".to_string(),
                    ),
                    Loc::generated(),
                )
            })?;

        // Extract RecordsPlan
        let plan = match records_value {
            Value::Records(plan) => plan,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Rdf::serialize expects records".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Get RDF metadata from type-level and field-level attributes
        let rdf_metadata = plan
            .type_def_id
            .and_then(|def_id| {
                ctx.gcx
                    .type_metadata
                    .get(&def_id)
                    .and_then(|tm| RdfMetadata::from_type_metadata(tm, &ctx.gcx.interner))
            })
            .ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime(
                        "Rdf::serialize requires a typed record with #[rdf(...)] attributes.\n\
                         Add type-level attributes like:\n\
                         #[rdf(type = \"http://schema.org/Person\", id = \"http://example.org/${id}\")]\n\
                         type Person = ..."
                            .to_string(),
                    ),
                    Loc::generated(),
                )
            })?;

        serialize_streaming(&plan, &rdf_metadata, &filename, ctx)
    }
}

/// RDF serialization using chunked execution with Oxigraph
///
/// Uses ChunkedExecutor to process data in fixed-size batches, ensuring constant
/// memory usage regardless of dataset size. Writes native RDF using Oxigraph.
///
/// DataFrame schema for batches:
/// - `_subject`: Generated subject URI from id_template (or blank node `_:bN`)
/// - `_type`: The rdf:type URI (if specified)
/// - Predicate columns: Renamed to their RDF URIs
fn serialize_streaming(
    plan: &fossil_lang::runtime::value::RecordsPlan,
    rdf_metadata: &RdfMetadata,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::runtime::chunked_executor::{ChunkedExecutor, estimate_batch_size_from_plan};
    use fossil_lang::runtime::value::Transform;
    use std::cell::RefCell;

    let interner = ctx.gcx.interner.clone();

    // Clone the plan so we can modify its transforms
    let mut plan = plan.clone();

    // If there are subject_columns that need to be preserved from the source,
    // we need to inject them into any existing Select transform.
    // This ensures columns referenced in ${...} templates are available even if
    // they weren't part of the mapped type's fields.
    if !rdf_metadata.subject_columns.is_empty() {
        for transform in &mut plan.transforms {
            if let Transform::Select(exprs) = transform {
                for col_name in &rdf_metadata.subject_columns {
                    // Only add if not already present
                    let already_present = exprs.iter().any(|e| {
                        // Check if this expression is already selecting this column
                        matches!(e, Expr::Column(name) if name.to_string() == *col_name)
                            || matches!(e, Expr::Alias(_, alias) if alias.to_string() == *col_name)
                    });
                    if !already_present {
                        exprs.push(col(col_name));
                    }
                }
            }
        }
    }

    let mut selection: Vec<Expr> = Vec::new();

    // 1. Generate _subject column from id_template (required)
    let id_template = rdf_metadata.id_template.as_ref().ok_or_else(|| {
        fossil_lang::error::CompileError::new(
            fossil_lang::error::CompileErrorKind::Runtime(
                "Rdf::serialize requires #[rdf(id = \"...\")] attribute.\n\
                 The id template defines how subject URIs are generated.\n\
                 Example: #[rdf(id = \"http://example.org/${id}\")]\n\
                 Use ${column_name} for interpolation."
                    .to_string(),
            ),
            Loc::generated(),
        )
    })?;
    let subject_expr = parse_id_template_to_expr(id_template);
    selection.push(subject_expr.alias("_subject"));

    // 2. Add _type column if rdf_type is specified
    if let Some(ref rdf_type) = rdf_metadata.rdf_type {
        selection.push(lit(rdf_type.as_str()).alias("_type"));
    }

    // 3. Add predicate columns renamed to their URIs
    for (field_sym, predicate_uri) in &rdf_metadata.predicates {
        let field_name = interner.resolve(*field_sym);
        selection.push(col(field_name).alias(predicate_uri));
    }

    // Infer output format from file extension
    let format = OutputFormat::from_path(destination);

    // Create the RDF batch writer
    let writer = RdfBatchWriter::new(destination, format, rdf_metadata).map_err(|e| {
        fossil_lang::error::CompileError::new(
            fossil_lang::error::CompileErrorKind::Runtime(format!(
                "Failed to create RDF writer: {}",
                e
            )),
            Loc::generated(),
        )
    })?;

    // Use RefCell to allow mutable access from the closure
    let writer = RefCell::new(writer);

    // Estimate batch size from plan schema
    let batch_size = estimate_batch_size_from_plan(&plan);

    // Execute plan with selection - ChunkedExecutor is the ONLY place materialization happens
    let executor = ChunkedExecutor::new(batch_size);
    executor
        .execute_plan_with_select_batched(&plan, selection, |batch| {
            writer.borrow_mut().write_batch(&batch)
        })
        .map_err(|e| {
            fossil_lang::error::CompileError::new(
                fossil_lang::error::CompileErrorKind::Runtime(format!(
                    "Failed to write RDF: {}",
                    e
                )),
                Loc::generated(),
            )
        })?;

    // Finish writing and close the file
    writer.into_inner().finish().map_err(|e| {
        fossil_lang::error::CompileError::new(
            fossil_lang::error::CompileErrorKind::Runtime(format!(
                "Failed to finalize RDF file: {}",
                e
            )),
            Loc::generated(),
        )
    })?;

    Ok(Value::Unit)
}

/// Parse an id_template like "http://example.org/${id}/${name}" into a Polars concat_str expression
///
/// The template supports `${column_name}` interpolation which references columns in the data.
///
/// # Example
/// ```text
/// "http://example.org/${id}" -> concat_str([lit("http://example.org/"), col("id")])
/// "urn:${a}:${b}"            -> concat_str([lit("urn:"), col("a"), lit(":"), col("b")])
/// ```
fn parse_id_template_to_expr(template: &str) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current_pos = 0;
    let bytes = template.as_bytes();

    while current_pos < template.len() {
        // Find next ${
        if let Some(start) = template[current_pos..].find("${") {
            let start_abs = current_pos + start;

            // Add literal part before ${
            if start_abs > current_pos {
                parts.push(lit(&template[current_pos..start_abs]));
            }

            // Find matching }
            let expr_start = start_abs + 2;
            let mut brace_depth = 1;
            let mut expr_end = expr_start;

            while expr_end < template.len() && brace_depth > 0 {
                match bytes[expr_end] {
                    b'{' => brace_depth += 1,
                    b'}' => brace_depth -= 1,
                    _ => {}
                }
                if brace_depth > 0 {
                    expr_end += 1;
                }
            }

            if brace_depth == 0 {
                // Extract column name and add as col() expression
                let column_name = &template[expr_start..expr_end];
                parts.push(col(column_name).cast(DataType::String));
                current_pos = expr_end + 1;
            } else {
                // Unmatched brace - treat rest as literal
                parts.push(lit(&template[current_pos..]));
                break;
            }
        } else {
            // No more interpolations, add remaining text
            if current_pos < template.len() {
                parts.push(lit(&template[current_pos..]));
            }
            break;
        }
    }

    // Combine all parts with concat_str
    if parts.is_empty() {
        lit(template)
    } else if parts.len() == 1 {
        parts.pop().unwrap()
    } else {
        concat_str(parts, "", true)
    }
}
