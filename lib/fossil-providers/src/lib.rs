//! Fossil data source providers.
//!
//! Dual API: legacy init() for backward compat + new register() for FunctionDef.

pub mod csv;
pub mod excel;
pub mod utils;

use fossil_lang::passes::GlobalContext;
use fossil_lang::registry::{FunctionDef, ParamDef, Registry};

// ── Legacy init (kept until keasy migrates to Executor) ──────────────

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("csv", csv::CsvProvider);
    gcx.register_provider("excel", excel::ExcelProvider);
}

// ── New: const FunctionDef registrations ─────────────────────────────

pub const CSV_DEF: FunctionDef = FunctionDef::source(
    "csv",
    "SELECT * FROM read_csv('{path}', delim='{delimiter}', header={header})",
    &[ParamDef::required("path"), ParamDef::optional("delimiter"), ParamDef::optional("header")],
);

pub const EXCEL_DEF: FunctionDef = FunctionDef::source(
    "excel",
    "SELECT * FROM st_read('{path}', layer='{sheet}')",
    &[ParamDef::required("path"), ParamDef::optional("sheet")],
);

pub const PARQUET_DEF: FunctionDef = FunctionDef::source(
    "parquet",
    "SELECT * FROM read_parquet('{path}')",
    &[ParamDef::required("path")],
);

pub const SOURCES: &[FunctionDef] = &[CSV_DEF, EXCEL_DEF, PARQUET_DEF];

pub fn register(r: &mut Registry) {
    r.add_functions(SOURCES);
}
