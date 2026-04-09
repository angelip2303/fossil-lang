//! PipelinePlugin — complex multi-step operations (Text.extract, etc.).

use serde::{Deserialize, Serialize};

/// A single step in a pipeline operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PipelineStep {
    Chunk { size: usize, overlap: usize },
    LlmExtract { json_schema: String, system_prompt: String },
    LlmGround { source_column: String },
    Dedup { key_columns: Vec<String> },
    LoadIntoTable { table_name: String },
}

/// Trait for pipeline plugins.
pub trait PipelinePlugin: Send + Sync {
    /// Function name in scripts (e.g., "Text.extract").
    fn name(&self) -> &'static str;

    /// Generate the pipeline steps for the compiled plan.
    fn plan(&self, type_schemas: &[String]) -> Vec<PipelineStep>;
}
