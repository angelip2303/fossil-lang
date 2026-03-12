/// Abstraction over RDF triple serialization.
///
/// Decouples the Polars→triples logic in `RdfBatchWriter` from any specific
/// serialization backend (oxrdfio, N-Triples, fragments, …).
pub trait TripleWriter: Send {
    /// Write a single RDF triple (or quad when `graph` is `Some`).
    ///
    /// All components are passed as plain strings so that consumers
    /// don't need to depend on `oxrdf` types.
    fn write_triple(
        &mut self,
        subject: &str,
        predicate: &str,
        object: &str,
        datatype: Option<&str>,
        lang: Option<&str>,
        graph: Option<&str>,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;

    /// Flush any buffered data and finalize the writer.
    fn finish(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>>;
}
