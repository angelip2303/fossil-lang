//! Plugin system — 4 extension points for the Fossil compiler.
//!
//! Plugins extend the language with new data sources, transforms,
//! pipeline operations, and output formats. The compiler validates
//! and plans; the host executes.

pub mod registry;
pub mod source;
pub mod transform;
pub mod pipeline;
pub mod output;

pub use registry::PluginRegistry;
pub use source::{SourcePlugin, SchemaSource, LoadAction, PreprocessConfig};
pub use transform::{TransformPlugin, TransformOp};
pub use pipeline::{PipelinePlugin, PipelineStep};
pub use output::OutputPlugin;
