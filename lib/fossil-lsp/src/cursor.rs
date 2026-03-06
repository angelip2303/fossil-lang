/// Context around the cursor position, determined by pure text analysis.
/// No compilation needed — this works even with incomplete/broken source.
#[derive(Debug, PartialEq)]
pub enum CursorContext {
    /// After `identifier.` or `identifier.partial` — complete fields/methods.
    DotAccess { receiver: String, partial: String },
    /// After `@` — complete connection names.
    ConnectionRef { partial: String },
    /// General position — complete keywords, types, bindings.
    TopLevel { partial: String },
}

impl CursorContext {
    /// Pure text analysis — NO compilation needed.
    /// Parses backwards from cursor to determine context.
    pub fn resolve(source: &str, offset: usize) -> Self {
        let offset = offset.min(source.len());
        let before = &source[..offset];

        // Check for @partial (connection reference)
        if let Some(ctx) = Self::try_connection_ref(before) {
            return ctx;
        }

        // Check for identifier.partial (dot access)
        if let Some(ctx) = Self::try_dot_access(before) {
            return ctx;
        }

        // Fallback: top-level with partial identifier
        let partial = Self::extract_partial_ident(before);
        CursorContext::TopLevel { partial }
    }

    fn try_connection_ref(before: &str) -> Option<CursorContext> {
        // Walk backwards through alphanumeric/underscore/hyphen chars
        let partial_start = before
            .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .map(|i| i + 1)
            .unwrap_or(0);

        if partial_start == 0 {
            return None;
        }

        // Check if the character before the partial is '@'
        let before_partial = &before[..partial_start];
        if before_partial.ends_with('@') {
            let partial = before[partial_start..].to_string();
            return Some(CursorContext::ConnectionRef { partial });
        }

        // Check if cursor is right on '@'
        if before.ends_with('@') {
            return Some(CursorContext::ConnectionRef {
                partial: String::new(),
            });
        }

        None
    }

    fn try_dot_access(before: &str) -> Option<CursorContext> {
        // Find where the partial field name starts (characters after the dot)
        let partial_start = before
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| i + 1)
            .unwrap_or(0);

        let before_partial = &before[..partial_start];

        // Check if there's a dot immediately before the partial
        if before_partial.ends_with('.') {
            let partial = before[partial_start..].to_string();
            let before_dot = &before_partial[..before_partial.len() - 1];

            // Extract the receiver identifier before the dot
            let receiver = Self::extract_partial_ident(before_dot);
            if !receiver.is_empty() {
                return Some(CursorContext::DotAccess { receiver, partial });
            }
        }

        // Check if cursor is right on the dot: `identifier.`
        if before.ends_with('.') {
            let before_dot = &before[..before.len() - 1];
            let receiver = Self::extract_partial_ident(before_dot);
            if !receiver.is_empty() {
                return Some(CursorContext::DotAccess {
                    receiver,
                    partial: String::new(),
                });
            }
        }

        None
    }

    fn extract_partial_ident(s: &str) -> String {
        let start = s
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| i + 1)
            .unwrap_or(0);
        s[start..].to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dot_access_empty_partial() {
        let ctx = CursorContext::resolve("row.", 4);
        assert_eq!(
            ctx,
            CursorContext::DotAccess {
                receiver: "row".into(),
                partial: "".into()
            }
        );
    }

    #[test]
    fn dot_access_with_partial() {
        let ctx = CursorContext::resolve("row.na", 6);
        assert_eq!(
            ctx,
            CursorContext::DotAccess {
                receiver: "row".into(),
                partial: "na".into()
            }
        );
    }

    #[test]
    fn connection_ref_empty() {
        let ctx = CursorContext::resolve("csv!(@", 6);
        assert_eq!(
            ctx,
            CursorContext::ConnectionRef {
                partial: "".into()
            }
        );
    }

    #[test]
    fn connection_ref_with_partial() {
        let ctx = CursorContext::resolve("csv!(@my-conn", 14);
        assert_eq!(
            ctx,
            CursorContext::ConnectionRef {
                partial: "my-conn".into()
            }
        );
    }

    #[test]
    fn top_level_empty() {
        let ctx = CursorContext::resolve("", 0);
        assert_eq!(
            ctx,
            CursorContext::TopLevel {
                partial: "".into()
            }
        );
    }

    #[test]
    fn top_level_with_partial() {
        let ctx = CursorContext::resolve("let x = Rd", 11);
        assert_eq!(
            ctx,
            CursorContext::TopLevel {
                partial: "Rd".into()
            }
        );
    }

    #[test]
    fn module_dot_access() {
        let ctx = CursorContext::resolve("Rdf.", 4);
        assert_eq!(
            ctx,
            CursorContext::DotAccess {
                receiver: "Rdf".into(),
                partial: "".into()
            }
        );
    }
}
