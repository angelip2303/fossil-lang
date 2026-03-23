use polars::prelude::*;

use fossil_lang::context::metadata::AttributeData;
use fossil_lang::context::{Interner, Symbol};
use fossil_lang::traits::provider::FunctionDef;

use super::{ColumnOp, TransformFunction};

/// SHA-256 pseudonymization: replaces values with a truncated hash (16 hex chars).
#[derive(Debug)]
pub struct Hash;

/// Full suppression: replaces all values with null (preserves original dtype).
#[derive(Debug)]
pub struct Redact;

/// Character masking: "María García" → "M**** G******".
#[derive(Debug)]
pub struct Mask;

/// k-anonymity suppression: nulls values that appear fewer than `k` times.
#[derive(Debug)]
pub struct Suppress {
    pub k: i64,
}

/// Column shuffling: randomly reorders values, preserving distribution.
#[derive(Debug)]
pub struct Shuffle;

/// Numeric generalization: bins values into ranges (e.g. 45 → 40 with bin=10).
#[derive(Debug)]
pub struct GeneralizeNumeric {
    pub bin: i64,
}

#[derive(Debug, Clone, Copy)]
pub enum TemporalUnit {
    Year,
    Month,
    Day,
}

impl TemporalUnit {
    fn slice_len(self) -> u64 {
        match self {
            Self::Year => 4,
            Self::Month => 7,
            Self::Day => 10,
        }
    }
}

/// Temporal generalization: truncates ISO-8601 date strings.
#[derive(Debug)]
pub struct GeneralizeTemporal {
    pub unit: TemporalUnit,
}

impl ColumnOp for Hash {
    fn apply(&self, expr: Expr) -> Expr {
        expr.map(
            |s| {
                use sha2::{Digest, Sha256};
                let ca = s.str()?;
                let out: StringChunked = ca.apply_into_string_amortized(|val, buf| {
                    let hash = Sha256::digest(val.as_bytes());
                    for byte in &hash[..8] {
                        use std::fmt::Write;
                        let _ = write!(buf, "{:02x}", byte);
                    }
                });
                Ok(out.into_column())
            },
            |_: &Schema, field: &Field| Ok(field.clone()),
        )
    }
}

impl ColumnOp for Redact {
    fn apply(&self, _expr: Expr) -> Expr {
        lit(NULL)
    }
}

impl ColumnOp for Mask {
    fn apply(&self, expr: Expr) -> Expr {
        expr.map(
            |s| {
                let ca = s.str()?;
                let out: StringChunked = ca.apply_into_string_amortized(|val, buf| {
                    for (i, word) in val.split_whitespace().enumerate() {
                        if i > 0 {
                            buf.push(' ');
                        }
                        let mut chars = word.chars();
                        if let Some(first) = chars.next() {
                            buf.push(first);
                            for _ in chars {
                                buf.push('*');
                            }
                        }
                    }
                });
                Ok(out.into_column())
            },
            |_: &Schema, field: &Field| Ok(field.clone()),
        )
    }
}

impl ColumnOp for Suppress {
    fn apply(&self, expr: Expr) -> Expr {
        let k = self.k;
        // len() counts all rows including nulls; count() would skip them
        when(
            expr.clone()
                .len()
                .over([expr.clone()])
                .lt(lit(k)),
        )
        .then(lit(NULL))
        .otherwise(expr)
    }
}

impl ColumnOp for Shuffle {
    fn apply(&self, expr: Expr) -> Expr {
        expr.map(
            |s| {
                let series = s.as_materialized_series();
                let shuffled = series.shuffle(None);
                Ok(shuffled.into_column())
            },
            |_: &Schema, field: &Field| Ok(field.clone()),
        )
    }
}

impl ColumnOp for GeneralizeNumeric {
    fn apply(&self, expr: Expr) -> Expr {
        let bin = self.bin;
        expr.floor_div(lit(bin)) * lit(bin)
    }
}

impl ColumnOp for GeneralizeTemporal {
    fn apply(&self, expr: Expr) -> Expr {
        expr.str().slice(lit(0), lit(self.unit.slice_len()))
    }
}

pub fn parse_anon_attr(attr_data: &AttributeData, interner: &Interner) -> Vec<Box<dyn ColumnOp>> {
    let mut ops: Vec<Box<dyn ColumnOp>> = Vec::new();
    let attr = fossil_lang::context::metadata::TypedAttribute::new(attr_data, interner);

    if let Some(keyword) = attr_data.first_positional_string(interner) {
        match keyword {
            "hash" => ops.push(Box::new(Hash)),
            "redact" => ops.push(Box::new(Redact)),
            "mask" => ops.push(Box::new(Mask)),
            "shuffle" => ops.push(Box::new(Shuffle)),
            "suppress" => {
                let k = attr.int("k").unwrap_or(5);
                ops.push(Box::new(Suppress { k }));
            }
            "generalize" => {
                if let Some(bin) = attr.int("bin") {
                    ops.push(Box::new(GeneralizeNumeric { bin }));
                } else if let Some(unit) = attr.string("truncate") {
                    let unit = match unit {
                        "year" => TemporalUnit::Year,
                        "month" => TemporalUnit::Month,
                        _ => TemporalUnit::Day,
                    };
                    ops.push(Box::new(GeneralizeTemporal { unit }));
                }
            }
            _ => {}
        }
    }

    ops
}

pub fn anon_functions(type_name: Symbol) -> Vec<FunctionDef> {
    vec![FunctionDef::new(
        "anonymize",
        TransformFunction {
            type_name,
            namespace: "anon",
            label: "anonymize",
            parse: parse_anon_attr,
        },
    )]
}
