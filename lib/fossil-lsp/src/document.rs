use fossil_lang::error::CompileError;
use fossil_lang::passes::{GlobalContext, ThirProgram};
use std::sync::Arc;

/// Represents the state of a single document in the LSP
pub struct DocumentState {
    /// The document URI
    pub uri: String,
    /// The source text content
    pub text: String,
    /// The document version number
    pub version: i32,
    /// Compiled THIR program (if compilation succeeded)
    pub thir: Option<Arc<ThirProgram>>,
    /// Compilation errors
    pub errors: Vec<CompileError>,
    /// The GlobalContext containing the interner for symbol resolution
    pub gcx: GlobalContext,
}

impl DocumentState {
    pub fn new(uri: String, text: String, version: i32) -> Self {
        Self {
            uri,
            text,
            version,
            thir: None,
            errors: Vec::new(),
            gcx: GlobalContext::default(),
        }
    }

    /// Check if the document compiled successfully
    pub fn is_valid(&self) -> bool {
        self.thir.is_some() && self.errors.is_empty()
    }

    /// Convert LSP position (line, column) to byte offset
    pub fn position_to_offset(&self, line: u32, character: u32) -> Option<usize> {
        let mut offset = 0;
        for (i, text_line) in self.text.lines().enumerate() {
            if i == line as usize {
                return Some(offset + character as usize);
            }
            offset += text_line.len() + 1; // +1 for newline
        }
        None
    }

    /// Convert byte offset to LSP position (line, column)
    pub fn offset_to_position(&self, offset: usize) -> Option<(u32, u32)> {
        let mut current_offset = 0;
        for (line_num, text_line) in self.text.lines().enumerate() {
            let line_len = text_line.len();
            if current_offset + line_len >= offset {
                let character = offset - current_offset;
                return Some((line_num as u32, character as u32));
            }
            current_offset += line_len + 1; // +1 for newline
        }
        None
    }
}
