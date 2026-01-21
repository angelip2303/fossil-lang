//! RDF (Resource Description Framework) serialization for knowledge graphs
//!
//! This module provides functionality to serialize Entity-wrapped records
//! to RDF formats (Turtle and N-Triples). It uses attributes on record fields
//! to extract RDF predicates.
//!
//! # Supported Destinations
//!
//! The serialization destination is determined by the URI:
//! - Local file: `Rdf::serialize(entities, "results.ttl")`
//! - HTTP POST: `Rdf::serialize(entities, "https://api.example.com/upload")`
//!
//! # Example
//!
//! ```fossil
//! type Person = {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/age")]
//!     age: int
//! }
//!
//! let alice = { name = "Alice", age = 30 }
//! let entity = Entity::wrap(alice)
//! let ttl = Rdf::serialize_turtle(entity, "http://example.com/alice")
//! ```

pub mod metadata;

pub use metadata::RdfMetadata;

use fossil_lang::ast::Loc;
use fossil_lang::ast::thir::{Polytype, Type, TypeKind, TypeVar, TypedHir};
use fossil_lang::error::RuntimeError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_providers::source::{DataSource, WritableSource};

use crate::entity::EntityMetadata;

/// Output destination for RDF serialization
///
/// Supports both local file output and HTTP POST for remote destinations.
enum OutputDestination {
    /// Local file - writes directly to disk
    File(std::io::BufWriter<std::fs::File>),
    /// HTTP destination - collects data in buffer, POSTs on finalize
    Http {
        buffer: Vec<u8>,
        url: String,
    },
}

impl OutputDestination {
    /// Creates a new output destination based on the URI
    fn new(uri: &str) -> Result<Self, RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let source = DataSource::detect(uri).map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Invalid destination URI: {}", e)),
                Loc::generated(),
            )
        })?;

        match source {
            DataSource::Local(path) => {
                // Create parent directories if needed
                if let Some(parent) = path.parent()
                    && !parent.exists() {
                        std::fs::create_dir_all(parent).map_err(|e| {
                            CompileError::new(
                                CompileErrorKind::Runtime(format!("Failed to create directory: {}", e)),
                                Loc::generated(),
                            )
                        })?;
                    }

                let file = std::fs::File::create(&path).map_err(|e| {
                    CompileError::new(
                        CompileErrorKind::Runtime(format!("Failed to create file: {}", e)),
                        Loc::generated(),
                    )
                })?;
                Ok(OutputDestination::File(std::io::BufWriter::with_capacity(512 * 1024, file)))
            }
            DataSource::Http(url) => {
                Ok(OutputDestination::Http {
                    buffer: Vec::with_capacity(64 * 1024),
                    url,
                })
            }
        }
    }

    /// Finalizes the output (flushes file or POSTs HTTP data)
    fn finalize(self) -> Result<(), RuntimeError> {
        use fossil_lang::error::{CompileError, CompileErrorKind};
        use std::io::Write;

        match self {
            OutputDestination::File(mut writer) => {
                writer.flush().map_err(|e| {
                    CompileError::new(
                        CompileErrorKind::Runtime(format!("Flush error: {}", e)),
                        Loc::generated(),
                    )
                })?;
                Ok(())
            }
            OutputDestination::Http { buffer, url } => {
                let source = DataSource::Http(url.clone());
                source.write_all(&buffer).map_err(|e| {
                    CompileError::new(
                        CompileErrorKind::Runtime(format!("Failed to POST to '{}': {}", url, e)),
                        Loc::generated(),
                    )
                })?;
                Ok(())
            }
        }
    }
}

impl std::io::Write for OutputDestination {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            OutputDestination::File(writer) => writer.write(buf),
            OutputDestination::Http { buffer, .. } => {
                buffer.extend_from_slice(buf);
                Ok(buf.len())
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            OutputDestination::File(writer) => writer.flush(),
            OutputDestination::Http { .. } => Ok(()),
        }
    }
}

/// Rdf::serialize function implementation
///
/// Signature: (List<Entity<T>>, string) -> Unit
///
/// Serializes a list of Entity-wrapped values to an RDF destination.
/// The destination can be a local file or an HTTP endpoint.
/// The format is determined by the file extension (.ttl for Turtle, .nt for N-Triples).
///
/// Prefixes are written once (Turtle format) and the data is written once
/// instead of appending, making this efficient for batch operations.
///
/// # Supported Destinations
/// - Local file: `"results.ttl"`, `"./output/data.nt"`
/// - HTTP POST: `"https://api.example.com/upload"`
///
/// # Example
/// ```fossil
/// let people = csv::load("people.csv")
/// let entities = map(people, fn(row) ->
///     row |> Entity::with_id(String::concat("http://example.com/person/", to_string(row.id)))
/// )
/// Rdf::serialize(entities, "people.ttl")
/// Rdf::serialize(entities, "https://api.example.com/rdf")
/// ```
pub struct RdfSerializeFunction;

impl FunctionImpl for RdfSerializeFunction {
    fn signature(
        &self,
        thir: &mut TypedHir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (List<T>, string) -> Unit
        let t_var = next_type_var();

        // First parameter: List<Entity<T>> (represented as List in type system)
        // For now, just use a type variable
        let list_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        // Second parameter: string (filename)
        let filename_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(fossil_lang::ast::ast::PrimitiveType::String),
        });

        // Output type: Unit
        let output_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(fossil_lang::ast::ast::PrimitiveType::Unit),
        });

        // Function type: (List<T>, string) -> Unit
        let fn_ty = thir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, filename_ty], output_ty),
        });

        // Polymorphic type: forall T. (List<T>, string) -> Unit
        Polytype::poly(vec![t_var], fn_ty)
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, RuntimeError> {
        use crate::entity::{LazyStreamingEntityBatch, LAZY_ENTITY_STREAM_TYPE_ID};
        use fossil_lang::error::{CompileError, CompileErrorKind};

        let mut args_iter = args.into_iter();

        let entities_value = args_iter.next().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("Rdf::serialize requires entities and filename".to_string()),
                Loc::generated(),
            )
        })?;

        let filename = match args_iter.next() {
            Some(Value::String(s)) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Rdf::serialize filename must be a string".to_string()),
                    Loc::generated(),
                ));
            }
        };

        // Main path: LazyStreamingEntityBatch (from List::map with Entity)
        if let Value::Extension { type_id, metadata, .. } = &entities_value
            && *type_id == LAZY_ENTITY_STREAM_TYPE_ID
                && let Some(batch) = metadata.as_any().downcast_ref::<LazyStreamingEntityBatch>() {
                    return serialize_lazy_streaming_batch(batch, filename.as_ref(), ctx);
                }

        // Fallback: List of Entity values (for small lists from non-streaming path)
        if let Value::List(entities) = &entities_value {
            if entities.is_empty() {
                return Ok(Value::Unit);
            }
            return serialize_entities_streaming(entities, filename.as_ref(), &ctx.gcx.interner);
        }

        Err(CompileError::new(
            CompileErrorKind::Runtime("Rdf::serialize expects Entity stream or list".to_string()),
            Loc::generated(),
        ))
    }
}

/// Streaming serialization for large entity lists
/// Writes directly to destination without collecting all data in memory
///
/// OPTIMIZATION: Detects when entities share the same DataFrame (via DataFrameRow)
/// and uses fully columnar serialization - no per-entity collection needed.
fn serialize_entities_streaming(
    entities: &[Value],
    destination: &str,
    interner: &fossil_lang::context::Interner,
) -> Result<Value, RuntimeError> {
    use crate::entity::{EntityMetadata, ENTITY_TYPE_ID};
    use fossil_lang::error::{CompileError, CompileErrorKind};
    use std::io::Write;

    // First pass: check if all entities share the same DataFrame (DataFrameRow optimization)
    // This is the common case from List::map with DataFrameRow
    let first_entity = entities.first().ok_or_else(|| {
        CompileError::new(
            CompileErrorKind::Runtime("Empty entity list".to_string()),
            Loc::generated(),
        )
    })?;

    // Extract first entity's metadata and check if it's DataFrameRow
    let (first_meta, first_inner) = match first_entity {
        Value::Extension { type_id, metadata, value } if *type_id == ENTITY_TYPE_ID => {
            let meta = metadata.as_any().downcast_ref::<EntityMetadata>().ok_or_else(|| {
                CompileError::new(
                    CompileErrorKind::Runtime("Invalid Entity metadata".to_string()),
                    Loc::generated(),
                )
            })?;
            (meta, value.as_ref())
        }
        _ => {
            return Err(CompileError::new(
                CompileErrorKind::Runtime("Rdf::serialize requires Entity values".to_string()),
                Loc::generated(),
            ));
        }
    };

    // Check if first entity uses DataFrameRow - if so, use columnar path
    if let Value::Record(shared_df, _) = first_inner {
        return serialize_dataframe_row_entities(entities, shared_df, first_meta, destination, interner);
    }

    // Fallback: per-entity streaming for LazyFrame entities
    let mut writer = OutputDestination::new(destination)?;

    let mut rdf_metadata: Option<RdfMetadata> = None;
    let mut predicate_cache: Option<Vec<(String, String)>> = None;

    for entity_value in entities {
        let (entity_meta, inner_value) = match entity_value {
            Value::Extension {
                type_id,
                metadata,
                value,
            } if *type_id == ENTITY_TYPE_ID => {
                let meta = metadata
                    .as_any()
                    .downcast_ref::<EntityMetadata>()
                    .ok_or_else(|| {
                        CompileError::new(
                            CompileErrorKind::Runtime("Invalid Entity metadata".to_string()),
                            Loc::generated(),
                        )
                    })?;
                (meta, value)
            }
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Rdf::serialize requires Entity values".to_string()),
                    Loc::generated(),
                ));
            }
        };

        if rdf_metadata.is_none() {
            rdf_metadata = entity_meta.rdf_metadata.clone();
        }

        let df = match inner_value.as_ref() {
            Value::Records(lf) => lf.clone().collect().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to collect: {}", e)),
                    Loc::generated(),
                )
            })?,
            Value::Record(df_arc, row_idx) => {
                // Extract single row as DataFrame
                df_arc.slice(*row_idx as i64, 1)
            }
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Entity value must be a record".to_string()),
                    Loc::generated(),
                ));
            }
        };

        if predicate_cache.is_none()
            && let Some(ref meta) = rdf_metadata {
                let cache: Vec<_> = df
                    .get_column_names()
                    .iter()
                    .filter_map(|&col_name| {
                        let col_sym = interner.lookup(col_name)?;
                        let predicate = meta.get_predicate(col_sym)?;
                        Some((col_name.to_string(), predicate.to_string()))
                    })
                    .collect();
                predicate_cache = Some(cache);
            }

        let subject = entity_meta.id.as_ref();
        let predicates = predicate_cache.as_ref().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime("No RDF metadata found".to_string()),
                Loc::generated(),
            )
        })?;

        for (col_name, predicate) in predicates {
            if let Ok(column) = df.column(col_name) {
                let series = column.as_materialized_series();
                if let Some(literal) = format_literal_streaming(series, 0)? {
                    writeln!(writer, "<{}> <{}> {} .", subject, predicate, literal)
                        .map_err(|e| {
                            CompileError::new(
                                CompileErrorKind::Runtime(format!("Write error: {}", e)),
                                Loc::generated(),
                            )
                        })?;
                }
            }
        }
    }

    writer.finalize()?;

    Ok(Value::Unit)
}

/// Fully columnar serialization for DataFrameRow entities
///
/// This is the fast path when List::map creates entities with DataFrameRow values.
/// All entities share the same Arc<DataFrame>, so we:
/// 1. Collect subject URIs from entity metadata
/// 2. Iterate the shared DataFrame once
/// 3. Write triples directly - no per-entity collection needed
fn serialize_dataframe_row_entities(
    entities: &[Value],
    shared_df: &std::sync::Arc<polars::prelude::DataFrame>,
    first_meta: &EntityMetadata,
    destination: &str,
    interner: &fossil_lang::context::Interner,
) -> Result<Value, RuntimeError> {
    use crate::entity::{EntityMetadata, ENTITY_TYPE_ID};
    use fossil_lang::error::{CompileError, CompileErrorKind};
    use std::io::Write;

    // Collect all subject URIs (cheap - just Arc<str> refs)
    let mut subjects: Vec<&str> = Vec::with_capacity(entities.len());
    let mut row_indices: Vec<usize> = Vec::with_capacity(entities.len());

    for entity in entities {
        match entity {
            Value::Extension { type_id, metadata, value } if *type_id == ENTITY_TYPE_ID => {
                let meta = metadata.as_any().downcast_ref::<EntityMetadata>().ok_or_else(|| {
                    CompileError::new(
                        CompileErrorKind::Runtime("Invalid Entity metadata".to_string()),
                        Loc::generated(),
                    )
                })?;
                subjects.push(meta.id.as_ref());

                // Get row index from DataFrameRow
                if let Value::Record(_, idx) = value.as_ref() {
                    row_indices.push(*idx);
                }
            }
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime("Expected Entity value".to_string()),
                    Loc::generated(),
                ));
            }
        }
    }

    // Get RDF metadata
    let rdf_metadata = first_meta.rdf_metadata.as_ref().ok_or_else(|| {
        CompileError::new(
            CompileErrorKind::Runtime("No RDF metadata found".to_string()),
            Loc::generated(),
        )
    })?;

    // Build predicate cache from shared DataFrame
    let predicates: Vec<_> = shared_df
        .get_column_names()
        .iter()
        .filter_map(|&col_name| {
            let col_sym = interner.lookup(col_name)?;
            let predicate = rdf_metadata.get_predicate(col_sym)?;
            Some((col_name.to_string(), predicate.to_string()))
        })
        .collect();

    // Create output destination (local file or HTTP)
    let mut writer = OutputDestination::new(destination)?;

    // Columnar serialization: iterate subjects with row indices
    for (subject, row_idx) in subjects.iter().zip(row_indices.iter()) {
        for (col_name, predicate) in &predicates {
            if let Ok(column) = shared_df.column(col_name) {
                let series = column.as_materialized_series();
                if let Some(literal) = format_literal_streaming(series, *row_idx)? {
                    writeln!(writer, "<{}> <{}> {} .", subject, predicate, literal)
                        .map_err(|e| {
                            CompileError::new(
                                CompileErrorKind::Runtime(format!("Write error: {}", e)),
                                Loc::generated(),
                            )
                        })?;
                }
            }
        }
    }

    writer.finalize()?;

    Ok(Value::Unit)
}

/// Streaming serialization for LazyStreamingEntityBatch
///
/// Collects LazyFrame once into DataFrame, then uses optimized columnar serialization.
/// Same performance as StreamingEntityBatch but with lazy loading.
fn serialize_lazy_streaming_batch(
    batch: &crate::entity::LazyStreamingEntityBatch,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};

    // Collect LazyFrame once (lazy loading from CSV)
    let df = batch.lf.clone().collect().map_err(|e| {
        CompileError::new(
            CompileErrorKind::Runtime(format!("Failed to collect: {}", e)),
            Loc::generated(),
        )
    })?;

    let row_count = df.height();

    // Pre-extract column series ONCE
    let columns_with_predicates: Vec<_> = df
        .get_column_names()
        .iter()
        .filter_map(|&col_name| {
            if col_name == batch.id_column.as_str() {
                return None;
            }
            let col_sym = ctx.gcx.interner.lookup(col_name)?;
            let predicate = batch.rdf_metadata.get_predicate(col_sym)?;
            let column = df.column(col_name).ok()?;
            let series = column.as_materialized_series().clone();
            Some((predicate.to_string(), series))
        })
        .collect();

    // Get ID series
    let id_column = df.column(&batch.id_column).map_err(|e| {
        CompileError::new(
            CompileErrorKind::Runtime(format!("ID column not found: {}", e)),
            Loc::generated(),
        )
    })?;
    let id_series = id_column.as_materialized_series();

    // Create output destination (local file or HTTP)
    let mut writer = OutputDestination::new(destination)?;

    // Reusable string buffer for subject URIs
    let mut subject_buf = String::with_capacity(batch.subject_prefix.len() + 20);

    // Pre-cast typed columns
    let typed_columns: Vec<_> = columns_with_predicates
        .iter()
        .map(|(predicate, series)| {
            let typed = TypedColumn::from_series(series);
            (predicate.as_str(), typed)
        })
        .collect();
    let typed_id = TypedColumn::from_series(id_series);

    // Fast columnar serialization
    for row_idx in 0..row_count {
        subject_buf.clear();
        subject_buf.push_str(&batch.subject_prefix);
        typed_id.append_value(&mut subject_buf, row_idx);

        for (predicate, typed_col) in &typed_columns {
            let _ = typed_col.write_triple(&mut writer, &subject_buf, predicate, row_idx);
        }
    }

    writer.finalize()?;

    Ok(Value::Unit)
}

/// Pre-cast typed column for zero-overhead access during iteration
enum TypedColumn<'a> {
    String(polars::prelude::StringChunked),
    Int64(&'a polars::prelude::Int64Chunked),
    Int32(&'a polars::prelude::Int32Chunked),
    UInt64(&'a polars::prelude::UInt64Chunked),
    UInt32(&'a polars::prelude::UInt32Chunked),
    Float64(&'a polars::prelude::Float64Chunked),
    Boolean(&'a polars::prelude::BooleanChunked),
    Unknown,
}

impl<'a> TypedColumn<'a> {
    fn from_series(series: &'a polars::prelude::Series) -> Self {
        use polars::prelude::*;
        match series.dtype() {
            DataType::String => {
                if let Ok(ca) = series.str() {
                    TypedColumn::String(ca.clone())
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::Int64 => {
                if let Ok(ca) = series.i64() {
                    TypedColumn::Int64(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::Int32 => {
                if let Ok(ca) = series.i32() {
                    TypedColumn::Int32(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::UInt64 => {
                if let Ok(ca) = series.u64() {
                    TypedColumn::UInt64(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::UInt32 => {
                if let Ok(ca) = series.u32() {
                    TypedColumn::UInt32(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::Float64 => {
                if let Ok(ca) = series.f64() {
                    TypedColumn::Float64(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            DataType::Boolean => {
                if let Ok(ca) = series.bool() {
                    TypedColumn::Boolean(ca)
                } else {
                    TypedColumn::Unknown
                }
            }
            _ => TypedColumn::Unknown,
        }
    }

    /// Append value to buffer (for subject URIs)
    fn append_value(&self, buf: &mut String, idx: usize) {
        use std::fmt::Write;
        match self {
            TypedColumn::Int64(ca) => {
                if let Some(v) = ca.get(idx) {
                    let _ = write!(buf, "{}", v);
                }
            }
            TypedColumn::Int32(ca) => {
                if let Some(v) = ca.get(idx) {
                    let _ = write!(buf, "{}", v);
                }
            }
            TypedColumn::UInt64(ca) => {
                if let Some(v) = ca.get(idx) {
                    let _ = write!(buf, "{}", v);
                }
            }
            TypedColumn::UInt32(ca) => {
                if let Some(v) = ca.get(idx) {
                    let _ = write!(buf, "{}", v);
                }
            }
            TypedColumn::String(ca) => {
                if let Some(v) = ca.get(idx) {
                    buf.push_str(v);
                }
            }
            _ => {}
        }
    }

    /// Write complete triple directly to writer (avoids intermediate String allocations)
    fn write_triple<W: std::io::Write>(
        &self,
        writer: &mut W,
        subject: &str,
        predicate: &str,
        idx: usize,
    ) -> std::io::Result<()> {
        match self {
            TypedColumn::String(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\" .", subject, predicate, escape_rdf_string(v))
                } else {
                    Ok(())
                }
            }
            TypedColumn::Int64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::Int32(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::UInt64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::UInt32(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::Float64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#double> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::Boolean(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(writer, "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean> .", subject, predicate, v)
                } else {
                    Ok(())
                }
            }
            TypedColumn::Unknown => Ok(()),
        }
    }
}

/// Format a literal value for streaming serialization
fn format_literal_streaming(
    series: &polars::prelude::Series,
    idx: usize,
) -> Result<Option<String>, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};
    use polars::prelude::*;

    match series.dtype() {
        DataType::String => {
            let values = series.str().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Cast error: {}", e)),
                    Loc::generated(),
                )
            })?;
            Ok(values.get(idx).map(|s| format!("\"{}\"", escape_rdf_string(s))))
        }
        DataType::Int64 | DataType::Int32 | DataType::UInt32 | DataType::UInt64 => {
            let values = series.cast(&DataType::Int64).map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Cast error: {}", e)),
                    Loc::generated(),
                )
            })?;
            let i64_values = values.i64().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Cast error: {}", e)),
                    Loc::generated(),
                )
            })?;
            Ok(i64_values
                .get(idx)
                .map(|v| format!("\"{}\"^^<http://www.w3.org/2001/XMLSchema#integer>", v)))
        }
        DataType::Boolean => {
            let values = series.bool().map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Cast error: {}", e)),
                    Loc::generated(),
                )
            })?;
            Ok(values
                .get(idx)
                .map(|v| format!("\"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean>", v)))
        }
        _ => Ok(None),
    }
}

/// Escape special characters in RDF string literals
fn escape_rdf_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(c),
        }
    }
    result
}
