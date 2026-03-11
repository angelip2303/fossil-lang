pub mod template;

use polars::prelude::*;
use unicode_normalization::UnicodeNormalization;

/// Transliterate a string: NFD normalize then strip combining marks.
/// "Avilés" → "Aviles", "São Paulo" → "Sao Paulo"
fn transliterate(s: &str) -> String {
    s.nfd()
        .filter(|c| !unicode_normalization::char::is_combining_mark(*c))
        .collect()
}

pub fn slug(expr: Expr) -> Expr {
    expr.map(
        |s| {
            let ca = s.str()?;
            let out: StringChunked = ca.apply_into_string_amortized(|val, buf| {
                for c in transliterate(val).chars() {
                    if c.is_ascii_alphanumeric() {
                        buf.push(c.to_ascii_lowercase());
                    }
                }
            });
            Ok(out.into_column())
        },
        |_: &Schema, _: &Field| Ok(Field::new("slug".into(), DataType::String)),
    )
}

pub fn lower(expr: Expr) -> Expr {
    expr.str().to_lowercase()
}

pub fn upper(expr: Expr) -> Expr {
    expr.str().to_uppercase()
}

pub fn trim(expr: Expr) -> Expr {
    expr.str().strip_chars(lit(""))
}
