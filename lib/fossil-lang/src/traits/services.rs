//! External services trait for capabilities that require network I/O.
//!
//! Fossil is a pure computation engine with no network access.
//! The host (e.g. keasy server) implements this trait to provide
//! LLM-based entity extraction and grounding capabilities.
//!
//! Reference: Apple ODKE+ (Ontology-Driven Knowledge Extraction)
//! architecture with extraction + grounding validation stages.

/// External services provided by the host for LLM-based operations.
///
/// Implementations should use `tool_use` / structured output for extraction
/// and a separate validation call for grounding.
pub trait ExternalServices: Send + Sync {
    /// Extract structured entities from text using LLM tool_use.
    ///
    /// # Arguments
    /// * `text` - Source text to extract entities from
    /// * `schema` - JSON Schema describing expected entity types and their properties.
    ///   Generated automatically from Fossil type definitions.
    ///
    /// # Returns
    /// Array of JSON objects, each with a `"type"` field and property values
    /// matching the schema.
    fn extract(
        &self,
        text: &str,
        schema: &serde_json::Value,
    ) -> Result<Vec<serde_json::Value>, String>;

    /// Ground/validate a claim against source text (ODKE+ grounding stage).
    ///
    /// A separate LLM call that verifies whether an extracted entity is
    /// actually supported by the source text. This raises precision from
    /// ~91% to ~98.8% (Apple ODKE+ benchmark).
    ///
    /// # Arguments
    /// * `source_text` - The original text the entity was extracted from
    /// * `claim` - A natural language claim about the entity
    ///   (e.g. "Person 'Juan García' has role 'CEO' at organization 'ACME'")
    ///
    /// # Returns
    /// `true` if the claim is supported by the source text, `false` otherwise.
    fn ground(
        &self,
        source_text: &str,
        claim: &str,
    ) -> Result<bool, String>;
}
