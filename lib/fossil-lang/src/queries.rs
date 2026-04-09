//! Top-level compilation queries.
//!
//! 2 Salsa tracked functions + 4 public functions.
//! Tracked: plan (keasy execution), schema_needs (cloud pre-resolve).
//! Public: parse, lower, infer, rq — called directly by LSP/DCAT.
//!
//! The intermediate types (ParsedProgram, IrProgram) contain Arc<dyn PathResolver>
//! which can't implement Salsa's Update trait. Once GlobalContext is decomposed
//! into Salsa primitives (Phase 2), all queries become tracked.
//!
//! Reference: rust-analyzer query architecture, Salsa Calc tutorial.

use std::sync::OnceLock;

use crate::builtins;
use crate::common::PrimitiveType;
use crate::context::extract_type_metadata;
use crate::db::{Db, Diagnostic, Severity, SourceFile};
use crate::passes::parse::Parser;
use crate::passes::typecheck::TypeChecker;
use crate::passes::{GlobalContext, IrProgram, ParsedProgram};
use crate::plan::FossilPlan;
use crate::registry::{OpImpl, Registry};
use crate::rq::emit_sql::rq_to_sql;
use crate::rq::lower::RqLowering;
use crate::rq::RelationalQuery;

// ── Static registry (computed once) ─────────────────────────────────

pub fn registry() -> &'static Registry {
    static REG: OnceLock<Registry> = OnceLock::new();
    REG.get_or_init(|| {
        let mut r = Registry::new();
        builtins::register(&mut r);
        r
    })
}

// ── Schema needs (Salsa tracked, cloud batch) ───────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaRequest {
    pub provider: String,
    pub path: String,
}

/// Discover source schemas needed before compilation (pure, no I/O).
/// Host calls this first, resolves all schemas in parallel, then calls plan().
#[salsa::tracked]
pub fn schema_needs(db: &dyn Db, file: SourceFile) -> Vec<SchemaRequest> {
    let p = parse(db, file);
    find_provider_calls(&p)
}

// ── Plan (Salsa tracked, keasy execution) ───────────────────────────

/// Compile source to FossilPlan. The entry point for job execution.
#[salsa::tracked]
pub fn plan(db: &dyn Db, file: SourceFile) -> FossilPlan {
    use salsa::Accumulator;
    let rq = match rq(db, file) {
        Ok(rq) => rq,
        Err(errors) => {
            for msg in &errors {
                Diagnostic {
                    message: msg.clone(),
                    offset: 0,
                    len: 0,
                    severity: Severity::Error,
                }
                .accumulate(db);
            }
            return FossilPlan::empty();
        }
    };
    let sql = rq_to_sql(&rq);
    FossilPlan::from_rq(rq, sql)
}

// ── Public functions (LSP, DCAT, debugging) ─────────────────────────

/// Parse source text into an AST.
pub fn parse(db: &dyn Db, file: SourceFile) -> ParsedProgram {
    match Parser::parse(file.text(db), 0) {
        Ok(p) => p,
        Err(_errors) => ParsedProgram {
            ast: Default::default(),
            gcx: Default::default(),
        },
    }
}

/// Lower AST to IR. Registers builtins and resolves provider schemas.
pub fn lower(
    db: &dyn Db,
    file: SourceFile,
) -> Result<(crate::ir::Ir, GlobalContext, crate::ir::Resolutions), Vec<String>> {
    let p = parse(db, file);
    let mut gcx = p.gcx;
    let reg = registry();

    register_builtins(&mut gcx, reg);
    register_provider_schemas(db, &p.ast, &mut gcx, reg);

    let metadata = extract_type_metadata(&p.ast);
    crate::passes::lower::lower_with_metadata(p.ast, gcx, metadata)
        .map_err(|errors| errors.0.into_iter().map(|e| e.to_string()).collect())
}

/// Type-check the IR. Returns IrProgram with per-expression types.
pub fn infer(db: &dyn Db, file: SourceFile) -> Result<IrProgram, Vec<String>> {
    let (ir, gcx, resolutions) = lower(db, file)?;
    TypeChecker::new(ir, gcx, resolutions)
        .check()
        .map_err(|errors| errors.0.into_iter().map(|e| e.to_string()).collect())
}

/// Lower typed IR to RelationalQuery.
pub fn rq(db: &dyn Db, file: SourceFile) -> Result<RelationalQuery, Vec<String>> {
    let program = infer(db, file)?;
    let reg = registry();
    RqLowering::new(
        &program.ir,
        &program.gcx,
        &program.type_index,
        &program.resolutions,
        &program.typeck_results,
        reg,
    )
    .lower()
    .map_err(|e| vec![e.to_string()])
}

// ── Helpers ─────────────────────────────────────────────────────────

fn register_builtins(gcx: &mut GlobalContext, reg: &Registry) {
    use crate::context::DefKind;
    for func in &reg.functions {
        if func.namespace.is_empty() {
            let sym = gcx.interner.intern(func.name);
            gcx.definitions.insert(None, sym, DefKind::Let);
        } else {
            let ns_sym = gcx.interner.intern(func.namespace);
            let ns_def = gcx
                .definitions
                .get_by_symbol(ns_sym)
                .map(|d| d.id())
                .unwrap_or_else(|| gcx.definitions.insert(None, ns_sym, DefKind::Mod));
            let name_sym = gcx.interner.intern(func.name);
            gcx.definitions.insert(Some(ns_def), name_sym, DefKind::Let);
        }
    }
}

fn register_provider_schemas(
    db: &dyn Db,
    ast: &crate::ast::Ast,
    gcx: &mut GlobalContext,
    reg: &Registry,
) {
    let interner = &gcx.interner.clone();
    for call in find_provider_calls_from_ast(ast, interner) {
        let func = match reg.find_source(&call.provider) {
            Some(f) => f,
            None => continue,
        };
        let columns: Vec<(String, String)> = match &func.impl_ {
            OpImpl::SourceSql(_) => match db.source_schema(&call.provider, &call.path) {
                Some(cols) => cols,
                None => continue,
            },
            OpImpl::Preprocess { schema, .. } => {
                schema
                    .iter()
                    .map(|(n, t)| (n.to_string(), t.to_string()))
                    .collect()
            }
            _ => continue,
        };
        let fields: Vec<(&str, crate::context::global::BuiltInFieldType)> = columns
            .iter()
            .map(|(name, type_str)| {
                let prim = map_sql_type(type_str);
                (
                    name.as_str(),
                    crate::context::global::BuiltInFieldType::Required(prim),
                )
            })
            .collect();
        gcx.register_record_type_with_optionality(&call.provider, fields);
    }
}

fn find_provider_calls(parsed: &ParsedProgram) -> Vec<SchemaRequest> {
    find_provider_calls_from_ast(&parsed.ast, &parsed.gcx.interner)
}

fn find_provider_calls_from_ast(
    ast: &crate::ast::Ast,
    interner: &crate::context::Interner,
) -> Vec<SchemaRequest> {
    use crate::ast::{ExprKind, StmtKind};
    use crate::common::{Literal, ProviderArgument};
    let mut calls = Vec::new();

    for &stmt_id in &ast.root {
        let stmt = ast.stmts.get(stmt_id);
        let expr_id = match &stmt.kind {
            StmtKind::Let { value, .. } => Some(*value),
            _ => None,
        };
        if let Some(eid) = expr_id {
            let expr = ast.exprs.get(eid);
            if let ExprKind::ProviderInvocation { provider, args } = &expr.kind {
                let provider_name = provider.display(interner);
                let path = args.iter().find_map(|arg| match arg {
                    ProviderArgument::Positional(Literal::String(s)) => {
                        Some(interner.resolve(*s).to_string())
                    }
                    _ => None,
                });
                if let Some(path) = path {
                    calls.push(SchemaRequest {
                        provider: provider_name,
                        path,
                    });
                }
            }
        }
    }

    calls
}

fn map_sql_type(sql_type: &str) -> PrimitiveType {
    let upper = sql_type.to_uppercase();
    if upper.contains("INT") {
        PrimitiveType::Int
    } else if upper.contains("FLOAT")
        || upper.contains("DOUBLE")
        || upper.contains("DECIMAL")
        || upper.contains("REAL")
    {
        PrimitiveType::Float
    } else if upper.contains("BOOL") {
        PrimitiveType::Bool
    } else {
        PrimitiveType::String
    }
}

// ── FossilPlan::empty ───────────────────────────────────────────────

impl FossilPlan {
    pub fn empty() -> Self {
        Self {
            sources: Vec::new(),
            sql: String::new(),
            outputs: Vec::new(),
            rq: RelationalQuery::new(),
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;

    #[test]
    fn parse_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let p = parse(&db, file);
        assert!(!p.ast.root.is_empty());
    }

    #[test]
    fn lower_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let (ir, _gcx, _res) = lower(&db, file).expect("lower failed");
        assert!(!ir.root.is_empty());
    }

    #[test]
    fn infer_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let program = infer(&db, file).expect("infer failed");
        assert!(!program.ir.root.is_empty());
    }

    #[test]
    fn plan_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let p = plan(&db, file);
        assert!(p.sources.is_empty());
    }

    #[test]
    fn registry_is_populated() {
        let reg = registry();
        assert!(reg.find_source("csv").is_some());
        assert!(reg.find_source("parquet").is_some());
        assert!(reg.find_function("Rdf", "materialize").is_some());
    }

    #[test]
    fn map_sql_types() {
        assert_eq!(map_sql_type("INTEGER"), PrimitiveType::Int);
        assert_eq!(map_sql_type("BIGINT"), PrimitiveType::Int);
        assert_eq!(map_sql_type("DOUBLE"), PrimitiveType::Float);
        assert_eq!(map_sql_type("VARCHAR"), PrimitiveType::String);
        assert_eq!(map_sql_type("BOOLEAN"), PrimitiveType::Bool);
    }
}
