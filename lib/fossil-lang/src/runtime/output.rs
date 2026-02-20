use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use crate::error::FossilError;

/// A resolved output destination: a writer + the original destination name.
pub struct OutputDestination {
    pub writer: Box<dyn Write + Send>,
    pub name: String,
}

impl OutputDestination {
    /// Get the file extension from the destination name.
    pub fn extension(&self) -> Option<String> {
        std::path::Path::new(&self.name)
            .extension()
            .map(|e| e.to_string_lossy().to_string())
    }
}

/// Trait for resolving output destinations to writable streams.
///
/// The CLI uses `LocalOutputResolver` which creates local files.
/// The server provides `CloudOutputResolver` which streams to cloud storage.
pub trait OutputResolver: Send + Sync {
    fn resolve_output(&self, destination: &str) -> Result<OutputDestination, FossilError>;

    /// Commit all pending outputs. Called after successful execution.
    fn commit(&self) -> Result<(), FossilError> {
        Ok(())
    }

    /// Abort all pending outputs. Called on failure/cancellation.
    fn abort(&self) {}
}

/// Default: creates local files (current CLI behavior).
pub struct LocalOutputResolver;

impl OutputResolver for LocalOutputResolver {
    fn resolve_output(&self, destination: &str) -> Result<OutputDestination, FossilError> {
        let path = PathBuf::from(destination);
        let file = File::create(&path).map_err(|e| {
            FossilError::evaluation(
                format!("Failed to create '{}': {}", destination, e),
                crate::ast::Loc::generated(),
            )
        })?;
        Ok(OutputDestination {
            writer: Box::new(BufWriter::new(file)),
            name: destination.to_string(),
        })
    }
}
