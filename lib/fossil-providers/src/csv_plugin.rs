//! CSV SourcePlugin — thin, no I/O. Generates SQL for DuckDB read_csv().

use std::collections::HashMap;
use fossil_lang::plugins::source::*;

pub struct CsvPlugin;

impl SourcePlugin for CsvPlugin {
    fn name(&self) -> &'static str { "csv" }

    fn extensions(&self) -> &'static [&'static str] { &["csv"] }

    fn params(&self) -> Vec<ParamInfo> {
        vec![
            ParamInfo { name: "path", required: true, expected_type: Some("string") },
            ParamInfo { name: "delimiter", required: false, expected_type: Some("string") },
            ParamInfo { name: "has_header", required: false, expected_type: Some("bool") },
        ]
    }

    fn schema_source(&self, path: &str, args: &HashMap<String, String>) -> SchemaSource {
        let delimiter = args.get("delimiter").map(|d| format!(", delimiter='{d}'")).unwrap_or_default();
        SchemaSource::Sql(format!(
            "DESCRIBE SELECT * FROM read_csv('{path}', auto_detect=true{delimiter})"
        ))
    }

    fn load(&self, path: &str, args: &HashMap<String, String>) -> LoadAction {
        let delimiter = args.get("delimiter").map(|d| format!(", delimiter='{d}'")).unwrap_or_default();
        let has_header = args.get("has_header")
            .map(|h| format!(", header={h}"))
            .unwrap_or_default();
        LoadAction::Sql(format!(
            "SELECT * FROM read_csv('{path}', auto_detect=true{delimiter}{has_header})"
        ))
    }
}
