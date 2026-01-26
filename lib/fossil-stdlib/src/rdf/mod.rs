//! RDF (Resource Description Framework) serialization for knowledge graphs
//!
//! This module provides functionality to serialize record plans to RDF formats.
//! Uses Polars' streaming capabilities for memory-efficient processing.
//!
//! # Example
//!
//! ```fossil
//! type Person = {
//!     #[rdf(uri = "http://xmlns.com/foaf/0.1/name")]
//!     name: string,
//!     age: int
//! }
//!
//! let people = Person::load()
//! let entities = List::map(people, fn(p) ->
//!     p |> Entity::with_id("http://example.com/person/${p.id}")
//! )
//! Rdf::serialize(entities, "output.nt")
//! ```

pub mod metadata;

pub use metadata::RdfMetadata;

use fossil_lang::ast::Loc;
use fossil_lang::error::RuntimeError;
use fossil_lang::ir::{Ir, Polytype, PrimitiveType, Type, TypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use polars::prelude::{PlPath, PlPathRef};

/// Rdf::serialize function implementation
///
/// Signature: (Records, string) -> Unit
///
/// Serializes a RecordsPlan with subject pattern to an RDF file.
/// Uses Polars' sink_batches for true streaming serialization.
pub struct RdfSerializeFunction;

impl FunctionImpl for RdfSerializeFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let t_var = next_type_var();

        let list_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Var(t_var),
        });

        let filename_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::String),
        });

        let output_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Primitive(PrimitiveType::Unit),
        });

        let fn_ty = ir.types.alloc(Type {
            loc: Loc::generated(),
            kind: TypeKind::Function(vec![list_ty, filename_ty], output_ty),
        });

        Polytype::poly(vec![t_var], fn_ty)
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

        let filename = match args_iter.next() {
            Some(Value::String(s)) => s,
            _ => {
                return Err(CompileError::new(
                    CompileErrorKind::Runtime(
                        "Rdf::serialize filename must be a string".to_string(),
                    ),
                    Loc::generated(),
                ));
            }
        };

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

        // Verify we have a subject pattern
        let subject_pattern = plan.subject_pattern.as_ref().ok_or_else(|| {
            CompileError::new(
                CompileErrorKind::Runtime(
                    "Rdf::serialize requires records with Entity::with_id applied".to_string(),
                ),
                Loc::generated(),
            )
        })?;

        // Get RDF metadata from type
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
                        "Rdf::serialize requires a typed record with #[rdf(...)] attributes"
                            .to_string(),
                    ),
                    Loc::generated(),
                )
            })?;

        serialize_streaming(
            &plan,
            subject_pattern,
            &rdf_metadata,
            filename.as_ref(),
            ctx,
        )
    }
}

/// RDF serialization using sink_batches callback
///
/// Uses Polars' sink_batches with the new streaming engine (`with_new_streaming(true)`).
/// The old `Engine::Streaming` API has bugs with sink_batches that drop rows.
fn serialize_streaming(
    plan: &fossil_lang::runtime::value::RecordsPlan,
    subject_pattern: &fossil_lang::runtime::value::SubjectPattern,
    rdf_metadata: &RdfMetadata,
    destination: &str,
    ctx: &RuntimeContext,
) -> Result<Value, RuntimeError> {
    use fossil_lang::error::{CompileError, CompileErrorKind};
    use polars::prelude::PlanCallback;
    use std::io::{BufWriter, Write};
    use std::sync::Mutex;

    let path = PlPath::from_str(destination);

    // Create buffered writer
    let writer: Box<dyn Write + Send> = match path.as_ref() {
        PlPathRef::Local(local_path) => {
            let file = std::fs::File::create(local_path).map_err(|e| {
                CompileError::new(
                    CompileErrorKind::Runtime(format!("Failed to create file: {}", e)),
                    Loc::generated(),
                )
            })?;
            Box::new(BufWriter::with_capacity(64 * 1024, file))
        }
        PlPathRef::Cloud(_) => {
            return Err(CompileError::new(
                CompileErrorKind::Runtime("Cloud storage not yet implemented".to_string()),
                Loc::generated(),
            ));
        }
    };

    let writer = std::sync::Arc::new(Mutex::new(writer));
    let writer_for_flush = writer.clone();

    // Capture for callback
    let subject_prefix = subject_pattern.prefix.clone();
    let id_column = subject_pattern.id_column.clone();
    let rdf_meta = rdf_metadata.clone();
    let interner = ctx.gcx.interner.clone();

    let callback = PlanCallback::new(move |df: polars::prelude::DataFrame| {
        let mut writer = writer.lock().unwrap();

        // Build predicate mappings for this batch
        // Note: id_column is included if it has an RDF predicate attribute
        let columns_with_predicates: Vec<_> = df
            .get_column_names()
            .iter()
            .filter_map(|&col_name| {
                let col_sym = interner.lookup(col_name)?;
                let predicate = rdf_meta.get_predicate(col_sym)?;
                let column = df.column(col_name).ok()?;
                let series = column.as_materialized_series().clone();
                Some((predicate.to_string(), series))
            })
            .collect();

        // Get ID column
        let id_col = match df.column(&id_column) {
            Ok(col) => col.as_materialized_series(),
            Err(_) => return Ok(true),
        };

        // Pre-cast typed columns
        let typed_columns: Vec<_> = columns_with_predicates
            .iter()
            .map(|(pred, series)| (pred.as_str(), TypedColumn::from_series(series)))
            .collect();
        let typed_id = TypedColumn::from_series(id_col);

        let mut subject_buf = String::with_capacity(subject_prefix.len() + 32);

        // Write triples for each row
        for row_idx in 0..df.height() {
            subject_buf.clear();
            subject_buf.push_str(&subject_prefix);
            typed_id.append_value(&mut subject_buf, row_idx);

            for (predicate, typed_col) in &typed_columns {
                let _ = typed_col.write_triple(&mut *writer, &subject_buf, predicate, row_idx);
            }
        }

        Ok(true)
    });

    // Create sink plan and execute with streaming engine
    let sink_lf = plan
        .lf
        .clone()
        .sink_batches(callback, true, None)
        .map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to create sink: {}", e)),
                Loc::generated(),
            )
        })?;

    // Execute the sink plan using the new streaming engine
    // Note: The old Engine::Streaming API has bugs with sink_batches (drops rows).
    // Using with_new_streaming(true) which works correctly.
    sink_lf
        .with_new_streaming(true)
        .collect()
        .map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Streaming serialization failed: {}", e)),
                Loc::generated(),
            )
        })?;

    // Flush the writer
    {
        let mut writer = writer_for_flush.lock().unwrap();
        writer.flush().map_err(|e| {
            CompileError::new(
                CompileErrorKind::Runtime(format!("Failed to flush output: {}", e)),
                Loc::generated(),
            )
        })?;
    }

    Ok(Value::Unit)
}

/// Pre-cast typed column for zero-overhead access
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
            DataType::String => series
                .str()
                .map(|ca| TypedColumn::String(ca.clone()))
                .unwrap_or(TypedColumn::Unknown),
            DataType::Int64 => series
                .i64()
                .map(TypedColumn::Int64)
                .unwrap_or(TypedColumn::Unknown),
            DataType::Int32 => series
                .i32()
                .map(TypedColumn::Int32)
                .unwrap_or(TypedColumn::Unknown),
            DataType::UInt64 => series
                .u64()
                .map(TypedColumn::UInt64)
                .unwrap_or(TypedColumn::Unknown),
            DataType::UInt32 => series
                .u32()
                .map(TypedColumn::UInt32)
                .unwrap_or(TypedColumn::Unknown),
            DataType::Float64 => series
                .f64()
                .map(TypedColumn::Float64)
                .unwrap_or(TypedColumn::Unknown),
            DataType::Boolean => series
                .bool()
                .map(TypedColumn::Boolean)
                .unwrap_or(TypedColumn::Unknown),
            _ => TypedColumn::Unknown,
        }
    }

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

    fn write_triple<W: std::io::Write + ?Sized>(
        &self,
        writer: &mut W,
        subject: &str,
        predicate: &str,
        idx: usize,
    ) -> std::io::Result<()> {
        match self {
            TypedColumn::String(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\" .",
                        subject,
                        predicate,
                        escape_rdf_string(v)
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::Int64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::Int32(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::UInt64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::UInt32(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#integer> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::Float64(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#double> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::Boolean(ca) => {
                if let Some(v) = ca.get(idx) {
                    writeln!(
                        writer,
                        "<{}> <{}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#boolean> .",
                        subject, predicate, v
                    )
                } else {
                    Ok(())
                }
            }
            TypedColumn::Unknown => Ok(()),
        }
    }
}

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
