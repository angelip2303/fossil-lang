//! Unified data source abstraction for transparent URI-based data access.
//!
//! This module provides a unified interface for reading and writing data
//! from multiple sources (local files, HTTP/HTTPS) based solely on the URI.
//!
//! # Examples
//!
//! ```rust
//! use fossil_providers::source::DataSource;
//!
//! // Local file
//! let local = DataSource::detect("path/to/file.csv").unwrap();
//! assert!(local.is_local());
//!
//! // HTTP URL
//! let remote = DataSource::detect("https://example.com/data.csv").unwrap();
//! assert!(remote.is_http());
//! ```

use std::io::{Read, Write};
use std::path::PathBuf;

/// Error type for data source operations
#[derive(Debug)]
pub enum SourceError {
    /// Invalid URI format
    InvalidUri(String),
    /// Network error (HTTP)
    Network(String),
    /// IO error (local file)
    Io(std::io::Error),
    /// Timeout error
    Timeout(String),
    /// HTTP error with status code
    HttpStatus(u16, String),
}

impl std::fmt::Display for SourceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceError::InvalidUri(msg) => write!(f, "Invalid URI: {}", msg),
            SourceError::Network(msg) => write!(f, "Network error: {}", msg),
            SourceError::Io(e) => write!(f, "IO error: {}", e),
            SourceError::Timeout(msg) => write!(f, "Timeout: {}", msg),
            SourceError::HttpStatus(code, msg) => write!(f, "HTTP {} error: {}", code, msg),
        }
    }
}

impl std::error::Error for SourceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SourceError::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for SourceError {
    fn from(e: std::io::Error) -> Self {
        SourceError::Io(e)
    }
}

/// Represents a data source detected by URI
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataSource {
    /// Local file system path
    Local(PathBuf),
    /// HTTP/HTTPS URL
    Http(String),
}

impl DataSource {
    /// Detects the data source type based on the URI.
    ///
    /// # URI Detection Rules
    ///
    /// - `http://` or `https://` prefix → HTTP source
    /// - Everything else → Local file path
    ///
    /// # Examples
    ///
    /// ```rust
    /// use fossil_providers::source::DataSource;
    ///
    /// // HTTP URLs
    /// let http = DataSource::detect("https://example.com/data.csv").unwrap();
    /// assert!(matches!(http, DataSource::Http(_)));
    ///
    /// // Local paths (relative and absolute)
    /// let local = DataSource::detect("./data/file.csv").unwrap();
    /// assert!(matches!(local, DataSource::Local(_)));
    ///
    /// let abs = DataSource::detect("/home/user/data.csv").unwrap();
    /// assert!(matches!(abs, DataSource::Local(_)));
    /// ```
    pub fn detect(uri: &str) -> Result<Self, SourceError> {
        let trimmed = uri.trim();

        if trimmed.is_empty() {
            return Err(SourceError::InvalidUri("Empty URI".to_string()));
        }

        if trimmed.starts_with("http://") || trimmed.starts_with("https://") {
            Ok(DataSource::Http(trimmed.to_string()))
        } else {
            Ok(DataSource::Local(PathBuf::from(trimmed)))
        }
    }

    /// Returns true if this is a local file source.
    pub fn is_local(&self) -> bool {
        matches!(self, DataSource::Local(_))
    }

    /// Returns true if this is an HTTP/HTTPS source.
    pub fn is_http(&self) -> bool {
        matches!(self, DataSource::Http(_))
    }

    /// Returns the URI as a string slice.
    pub fn as_str(&self) -> &str {
        match self {
            DataSource::Local(p) => p.to_str().unwrap_or(""),
            DataSource::Http(url) => url,
        }
    }

    /// Returns the local path if this is a local source.
    pub fn as_path(&self) -> Option<&PathBuf> {
        match self {
            DataSource::Local(p) => Some(p),
            DataSource::Http(_) => None,
        }
    }

    /// Returns the URL if this is an HTTP source.
    pub fn as_url(&self) -> Option<&str> {
        match self {
            DataSource::Local(_) => None,
            DataSource::Http(url) => Some(url),
        }
    }
}

/// Trait for reading data from a source (used at compile-time for schema inference)
pub trait ReadableSource {
    /// Reads the entire content as bytes.
    ///
    /// For local files, reads the file directly.
    /// For HTTP sources, performs a GET request.
    fn read_all(&self) -> Result<Vec<u8>, SourceError>;

    /// Reads up to `max_bytes` for schema inference.
    ///
    /// This is more efficient for large files/responses when
    /// only a sample is needed for type inference.
    fn read_for_schema(&self, max_bytes: Option<usize>) -> Result<Vec<u8>, SourceError>;
}

/// Trait for writing data to a source (used at runtime)
pub trait WritableSource {
    /// Writes bytes to the destination.
    ///
    /// For local files, writes to the file.
    /// For HTTP sources, performs a POST/PUT request.
    fn write_all(&self, data: &[u8]) -> Result<(), SourceError>;
}

impl ReadableSource for DataSource {
    fn read_all(&self) -> Result<Vec<u8>, SourceError> {
        match self {
            DataSource::Local(path) => {
                std::fs::read(path).map_err(SourceError::from)
            }
            DataSource::Http(url) => {
                http_get(url, None)
            }
        }
    }

    fn read_for_schema(&self, max_bytes: Option<usize>) -> Result<Vec<u8>, SourceError> {
        match self {
            DataSource::Local(path) => {
                if let Some(limit) = max_bytes {
                    let mut file = std::fs::File::open(path)?;
                    let mut buffer = vec![0u8; limit];
                    let bytes_read = file.read(&mut buffer)?;
                    buffer.truncate(bytes_read);
                    Ok(buffer)
                } else {
                    std::fs::read(path).map_err(SourceError::from)
                }
            }
            DataSource::Http(url) => {
                http_get(url, max_bytes)
            }
        }
    }
}

impl WritableSource for DataSource {
    fn write_all(&self, data: &[u8]) -> Result<(), SourceError> {
        match self {
            DataSource::Local(path) => {
                // Create parent directories if needed
                if let Some(parent) = path.parent() {
                    if !parent.exists() {
                        std::fs::create_dir_all(parent)?;
                    }
                }
                let mut file = std::fs::File::create(path)?;
                file.write_all(data)?;
                Ok(())
            }
            DataSource::Http(url) => {
                http_post(url, data)
            }
        }
    }
}

/// Performs an HTTP GET request with optional byte limit.
///
/// Uses reqwest in blocking mode for compile-time operations.
fn http_get(url: &str, max_bytes: Option<usize>) -> Result<Vec<u8>, SourceError> {
    use std::time::Duration;

    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(30))
        .build()
        .map_err(|e| SourceError::Network(e.to_string()))?;

    let response = client
        .get(url)
        .send()
        .map_err(|e| {
            if e.is_timeout() {
                SourceError::Timeout(format!(
                    "Connection timeout after 30s fetching '{}'",
                    url
                ))
            } else {
                SourceError::Network(e.to_string())
            }
        })?;

    let status = response.status();
    if !status.is_success() {
        return Err(SourceError::HttpStatus(
            status.as_u16(),
            format!("Failed to fetch '{}': {}", url, status),
        ));
    }

    let bytes = if let Some(limit) = max_bytes {
        // Read only up to the limit
        let mut buffer = Vec::with_capacity(limit);
        response
            .take(limit as u64)
            .read_to_end(&mut buffer)
            .map_err(|e| SourceError::Network(e.to_string()))?;
        buffer
    } else {
        response
            .bytes()
            .map_err(|e| SourceError::Network(e.to_string()))?
            .to_vec()
    };

    Ok(bytes)
}

/// Performs an HTTP POST request to write data.
fn http_post(url: &str, data: &[u8]) -> Result<(), SourceError> {
    use std::time::Duration;

    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(60))
        .build()
        .map_err(|e| SourceError::Network(e.to_string()))?;

    let response = client
        .post(url)
        .body(data.to_vec())
        .header("Content-Type", "application/octet-stream")
        .send()
        .map_err(|e| {
            if e.is_timeout() {
                SourceError::Timeout(format!(
                    "Connection timeout after 60s posting to '{}'",
                    url
                ))
            } else {
                SourceError::Network(e.to_string())
            }
        })?;

    let status = response.status();
    if !status.is_success() {
        return Err(SourceError::HttpStatus(
            status.as_u16(),
            format!("Failed to post to '{}': {}", url, status),
        ));
    }

    Ok(())
}

/// Helper to create user-friendly error messages for provider errors.
pub fn format_source_error(source: &DataSource, operation: &str, error: &SourceError) -> String {
    let uri = source.as_str();

    match error {
        SourceError::Timeout(msg) => {
            format!(
                "Error: Could not {} from '{}'\n  Caused by: {}\n\n  Hint: Ensure the URL is accessible and the server is responding.",
                operation, uri, msg
            )
        }
        SourceError::HttpStatus(code, _) => {
            let hint = match *code {
                404 => "Hint: Check that the URL is correct and the resource exists.",
                401 | 403 => "Hint: The resource may require authentication.",
                500..=599 => "Hint: The server encountered an error. Try again later.",
                _ => "Hint: Check the URL and network connectivity.",
            };
            format!(
                "Error: Could not {} from '{}'\n  Caused by: HTTP {} error\n\n  {}",
                operation, uri, code, hint
            )
        }
        SourceError::Io(io_err) => {
            let hint = match io_err.kind() {
                std::io::ErrorKind::NotFound => "Hint: Check that the file path is correct.",
                std::io::ErrorKind::PermissionDenied => "Hint: Check file permissions.",
                _ => "Hint: Check the file path and permissions.",
            };
            format!(
                "Error: Could not {} from '{}'\n  Caused by: {}\n\n  {}",
                operation, uri, io_err, hint
            )
        }
        _ => {
            format!(
                "Error: Could not {} from '{}'\n  Caused by: {}",
                operation, uri, error
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_local_paths() {
        assert!(matches!(
            DataSource::detect("file.csv").unwrap(),
            DataSource::Local(_)
        ));
        assert!(matches!(
            DataSource::detect("./relative/path.csv").unwrap(),
            DataSource::Local(_)
        ));
        assert!(matches!(
            DataSource::detect("/absolute/path.csv").unwrap(),
            DataSource::Local(_)
        ));
        assert!(matches!(
            DataSource::detect("../parent/file.json").unwrap(),
            DataSource::Local(_)
        ));
    }

    #[test]
    fn test_detect_http_urls() {
        assert!(matches!(
            DataSource::detect("http://example.com/data.csv").unwrap(),
            DataSource::Http(_)
        ));
        assert!(matches!(
            DataSource::detect("https://api.example.com/data.json").unwrap(),
            DataSource::Http(_)
        ));
        assert!(matches!(
            DataSource::detect("https://raw.githubusercontent.com/file.csv").unwrap(),
            DataSource::Http(_)
        ));
    }

    #[test]
    fn test_empty_uri_error() {
        assert!(matches!(
            DataSource::detect(""),
            Err(SourceError::InvalidUri(_))
        ));
        assert!(matches!(
            DataSource::detect("   "),
            Err(SourceError::InvalidUri(_))
        ));
    }

    #[test]
    fn test_source_methods() {
        let local = DataSource::Local(PathBuf::from("test.csv"));
        assert!(local.is_local());
        assert!(!local.is_http());
        assert!(local.as_path().is_some());
        assert!(local.as_url().is_none());

        let http = DataSource::Http("https://example.com/data.csv".to_string());
        assert!(!http.is_local());
        assert!(http.is_http());
        assert!(http.as_path().is_none());
        assert!(http.as_url().is_some());
    }
}
