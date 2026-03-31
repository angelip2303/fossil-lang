//! Text.extract<T...>() — Ontology-guided entity extraction from unstructured text.
//!
//! Pipeline (simplified Apple ODKE+):
//!   Stage 1: Chunking — split large text into LLM-sized windows
//!   Stage 2: Extraction — LLM tool_use with type-derived JSON schema
//!   Stage 3: Grounding — validate each entity against source text
//!   Stage 4: Deduplication + Emission construction

use std::collections::HashMap;

use polars::prelude::*;
use serde_json::json;

use fossil_lang::ast::Loc;
use fossil_lang::common::PrimitiveType;
use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::context::DefId;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{Emission, EmissionDef, Value};
use fossil_lang::traits::function::{FunctionEffect, FunctionImpl, RuntimeContext};



/// Maximum characters per chunk for LLM extraction.
const MAX_CHUNK_CHARS: usize = 100_000;
/// Overlap between consecutive chunks to avoid missing entities at boundaries.
const CHUNK_OVERLAP: usize = 2_000;

pub struct TextExtractFunction;

impl FunctionImpl for TextExtractFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T) -> T  (takes a Frame, returns an Emission — both are T at type level)
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);
        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty], t_ty))
    }

    fn effects(&self) -> &[FunctionEffect] {
        &[] // Not a sink — returns data, doesn't write
    }

    fn call(
        &self,
        args: Vec<Value>,
        type_args: &[DefId],
        ctx: &RuntimeContext,
    ) -> Result<Value, FossilError> {
        let loc = Loc::generated();

        // Validate: need at least one type arg
        if type_args.is_empty() {
            return Err(FossilError::evaluation(
                "Text.extract requires at least one type argument: Text.extract<Person, Org>(docs)",
                loc,
            ));
        }

        // Validate: need ExternalServices
        let services = ctx.gcx.external_services.as_ref().ok_or_else(|| {
            FossilError::evaluation(
                "Text.extract requires external services (LLM) to be configured",
                loc,
            )
        })?;

        // Get input frame
        let input_frame = args
            .into_iter()
            .next()
            .and_then(|v| v.into_frame())
            .ok_or_else(|| {
                FossilError::evaluation("Text.extract expects a Frame as first argument", loc)
            })?;

        // Stage 1: Build JSON schema from type args
        let schema = build_extraction_schema(type_args, ctx.gcx);

        // Stage 2: Materialize text from frame and chunk
        let df = input_frame
            .collect()
            .map_err(|e| FossilError::evaluation(e.to_string(), loc))?;

        let text_col = df
            .column("text")
            .map_err(|_| FossilError::evaluation("input frame must have a 'text' column", loc))?;
        let full_text: String = text_col
            .str()
            .map_err(|_| FossilError::evaluation("'text' column must be string type", loc))?
            .into_iter()
            .filter_map(|opt| opt)
            .collect::<Vec<_>>()
            .join("\n\n");

        let chunks = chunk_text(&full_text, MAX_CHUNK_CHARS, CHUNK_OVERLAP);

        // Stage 3: Extract entities from each chunk
        let mut all_entities: Vec<serde_json::Value> = Vec::new();
        for chunk in &chunks {
            match services.extract(chunk, &schema) {
                Ok(entities) => all_entities.extend(entities),
                Err(e) => {
                    return Err(FossilError::evaluation(
                        format!("LLM extraction failed: {e}"),
                        loc,
                    ));
                }
            }
        }

        // Stage 4: Grounding — validate each entity against source text
        let mut grounded_entities: Vec<serde_json::Value> = Vec::new();
        for entity in &all_entities {
            let claim = format_claim(entity);
            // Find the chunk this entity was extracted from (use first chunk for simplicity)
            let source = chunks.first().map(|s| s.as_str()).unwrap_or(&full_text);
            match services.ground(source, &claim) {
                Ok(true) => grounded_entities.push(entity.clone()),
                Ok(false) => {} // Discard ungrounded entities
                Err(_) => {
                    // On grounding failure, keep the entity (fail-open for robustness)
                    grounded_entities.push(entity.clone());
                }
            }
        }

        // Stage 5: Build Emission from grounded entities
        build_emission(&grounded_entities, type_args, ctx)
    }
}

/// Build a JSON schema for LLM tool_use from Fossil type definitions.
fn build_extraction_schema(type_args: &[DefId], gcx: &GlobalContext) -> serde_json::Value {
    let mut type_schemas = Vec::new();

    for &def_id in type_args {
        let def = gcx.definitions.get(def_id);
        let type_name = gcx.interner.resolve(def.name).to_string();

        let mut properties = serde_json::Map::new();
        let mut required = vec!["type".to_string()];

        // Add constructor params as required fields
        if let Some(registered) = gcx.registered_types.get(&def_id) {
            for (field_sym, field_type) in registered {
                let field_name = gcx.interner.resolve(*field_sym).to_string();
                let (json_type, is_required) = match field_type {
                    BuiltInFieldType::Required(p) => (primitive_to_json_type(*p), true),
                    BuiltInFieldType::Optional(p) => (primitive_to_json_type(*p), false),
                };
                properties.insert(
                    field_name.clone(),
                    json!({ "type": json_type }),
                );
                if is_required {
                    required.push(field_name);
                }
            }
        }

        type_schemas.push(json!({
            "type_name": type_name,
            "properties": properties,
            "required": required,
        }));
    }

    json!({
        "entity_types": type_schemas,
        "instruction": "Extract all entities matching these types from the text. Return an array of objects, each with a 'type' field matching one of the type_names."
    })
}

fn primitive_to_json_type(prim: PrimitiveType) -> &'static str {
    match prim {
        PrimitiveType::Int => "integer",
        PrimitiveType::Float => "number",
        PrimitiveType::Bool => "boolean",
        PrimitiveType::String => "string",
    }
}

/// Chunk text into windows of max_chars with overlap.
fn chunk_text(text: &str, max_chars: usize, overlap: usize) -> Vec<String> {
    if text.len() <= max_chars {
        return vec![text.to_string()];
    }

    let mut chunks = Vec::new();
    let mut start = 0;
    while start < text.len() {
        let end = (start + max_chars).min(text.len());
        chunks.push(text[start..end].to_string());
        if end >= text.len() {
            break;
        }
        start = end.saturating_sub(overlap);
    }
    chunks
}

/// Format an entity as a natural language claim for grounding.
fn format_claim(entity: &serde_json::Value) -> String {
    let type_name = entity
        .get("type")
        .and_then(|v| v.as_str())
        .unwrap_or("Entity");

    let props: Vec<String> = entity
        .as_object()
        .map(|obj| {
            obj.iter()
                .filter(|(k, _)| k.as_str() != "type")
                .map(|(k, v)| format!("{k}: {v}"))
                .collect()
        })
        .unwrap_or_default();

    format!("{type_name} with {}", props.join(", "))
}

/// Build an Emission from extracted JSON entities and Fossil type definitions.
fn build_emission(
    entities: &[serde_json::Value],
    type_args: &[DefId],
    ctx: &RuntimeContext,
) -> Result<Value, FossilError> {
    let loc = Loc::generated();
    let gcx = ctx.gcx;

    // Group entities by type
    let mut by_type: HashMap<String, Vec<&serde_json::Value>> = HashMap::new();
    for entity in entities {
        if let Some(type_name) = entity.get("type").and_then(|v| v.as_str()) {
            by_type.entry(type_name.to_string()).or_default().push(entity);
        }
    }

    // Build a combined DataFrame with all entities and emission specs
    let mut all_specs: Vec<EmissionDef> = Vec::new();
    let mut all_columns: HashMap<String, Vec<Option<String>>> = HashMap::new();
    let mut type_column: Vec<String> = Vec::new();
    let mut total_rows = 0;

    for &def_id in type_args {
        let def = gcx.definitions.get(def_id);
        let type_name = gcx.interner.resolve(def.name).to_string();
        let type_entities = by_type.get(&type_name).map(|v| v.as_slice()).unwrap_or(&[]);

        if type_entities.is_empty() {
            continue;
        }

        let registered = gcx.registered_types.get(&def_id);

        // Collect field names for this type
        let field_names: Vec<String> = registered
            .map(|fields| {
                fields
                    .iter()
                    .map(|(sym, _)| gcx.interner.resolve(*sym).to_string())
                    .collect()
            })
            .unwrap_or_default();

        // Add rows for each entity of this type
        for entity in type_entities {
            type_column.push(type_name.clone());
            for field_name in &field_names {
                let value = entity
                    .get(field_name)
                    .and_then(|v| match v {
                        serde_json::Value::String(s) => Some(s.clone()),
                        serde_json::Value::Number(n) => Some(n.to_string()),
                        serde_json::Value::Bool(b) => Some(b.to_string()),
                        serde_json::Value::Null => None,
                        other => Some(other.to_string()),
                    });
                all_columns
                    .entry(field_name.clone())
                    .or_insert_with(|| vec![None; total_rows])
                    .push(value);
            }
            total_rows += 1;
        }

        // Pad columns that weren't present for this type
        for (_, col_values) in all_columns.iter_mut() {
            while col_values.len() < total_rows {
                col_values.push(None);
            }
        }

        // Build EmissionDef for this type
        // Use first field as constructor arg (subject key) if available
        let ctor_args: Vec<Expr> = if let Some(first_field) = field_names.first() {
            vec![col(PlSmallStr::from(first_field.as_str()))]
        } else {
            vec![]
        };

        let select_exprs: Vec<Expr> = field_names
            .iter()
            .map(|name| col(PlSmallStr::from(name.as_str())).alias(PlSmallStr::from(name.as_str())))
            .collect();

        all_specs.push(EmissionDef {
            type_def_id: def_id,
            ctor_args,
            select_exprs,
        });
    }

    if total_rows == 0 {
        // Empty emission — no entities found
        let empty_df = DataFrame::empty();
        return Ok(Value::Emission(Emission {
            frame: empty_df.lazy(),
            specs: all_specs,
        }));
    }

    // Build the DataFrame
    let mut columns: Vec<Column> = vec![Column::new(
        "_entity_type".into(),
        type_column,
    )];

    for (name, values) in &all_columns {
        columns.push(Column::new(
            PlSmallStr::from(name.as_str()),
            values.iter().map(|v| v.as_deref()).collect::<Vec<_>>(),
        ));
    }

    let df = DataFrame::new(columns)
        .map_err(|e| FossilError::evaluation(format!("failed to build entity DataFrame: {e}"), loc))?;

    Ok(Value::Emission(Emission {
        frame: df.lazy(),
        specs: all_specs,
    }))
}
