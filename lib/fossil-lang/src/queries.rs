//! Top-level compilation queries.
//!
//! The new compilation path bypasses the legacy Compiler and uses
//! RQ lowering + SQL emission directly.

use crate::registry::Registry;

/// Create the default Registry with all built-in functions and attributes.
pub fn default_registry() -> Registry {
    let mut r = Registry::new();
    crate::builtins::register(&mut r);
    r
}
