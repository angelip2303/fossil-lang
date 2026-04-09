//! Compilation queries — Salsa tracked functions.
//!
//! Each pass is a tracked query that reads from other queries.
//! Pattern: rust-analyzer's query architecture.
//!
//! Dependency graph:
//!   SourceFile → parse
//!   parse → lower (registers builtins + provider schemas into DefMap)
//!   lower → infer
//!   infer → rq → plan

use std::sync::OnceLock;

use crate::ast::Ast;
use crate::builtins;
use crate::common::PrimitiveType;
use crate::context::global::{BuiltInFieldType, DefMap, RegisteredTypes};
use crate::context::{DefKind, Symbol, extract_type_metadata};
use crate::db::{Db, Diagnostic, Severity, SourceFile};
use crate::passes::parse::Parser;
use crate::passes::typecheck::TypeChecker;
use crate::passes::{IrProgram, LowerResult};
use crate::plan::FossilPlan;
use crate::registry::{OpImpl, Registry};
use crate::rq::emit_sql::rq_to_sql;
use crate::rq::lower::RqLowering;
use crate::rq::RelationalQuery;

// ── Static registry ─────────────────────────────────────────────────

pub fn registry() -> &'static Registry {
    static REG: OnceLock<Registry> = OnceLock::new();
    REG.get_or_init(|| {
        let mut r = Registry::new();
        builtins::register(&mut r);
        r
    })
}

// ── schema_needs (pre-compilation, cloud batch) ─────────────────────

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaRequest {
    pub provider: String,
    pub path: String,
}

/// Discover source schemas needed before compilation (pure, no I/O).
/// Host resolves all in parallel, then calls plan().
#[salsa::tracked]
pub fn schema_needs(db: &dyn Db, file: SourceFile) -> Vec<SchemaRequest> {
    let ast = parse(db, file);
    find_provider_calls_from_ast(&ast)
}

// ── parse ───────────────────────────────────────────────────────────

/// Parse source text into AST.
#[salsa::tracked]
pub fn parse(db: &dyn Db, file: SourceFile) -> Ast {
    use salsa::Accumulator;
    match Parser::parse(file.text(db), 0) {
        Ok(ast) => ast,
        Err(errors) => {
            for e in &errors.0 {
                Diagnostic {
                    message: e.to_string(),
                    offset: 0,
                    len: 0,
                    severity: Severity::Error,
                }
                .accumulate(db);
            }
            Ast::default()
        }
    }
}

// ── lower ───────────────────────────────────────────────────────────

/// Lower AST to IR. Registers builtins and resolves provider schemas.
#[salsa::tracked]
pub fn lower(db: &dyn Db, file: SourceFile) -> LowerResult {
    use salsa::Accumulator;
    let ast = parse(db, file);
    let reg = registry();

    let mut def_map = DefMap::default();
    let mut registered_types = RegisteredTypes::new();

    register_builtins(db, &mut def_map, reg);
    register_provider_schemas_from_ast(db, &ast, &mut def_map, &mut registered_types, reg);

    let metadata = extract_type_metadata(&ast);
    match crate::passes::lower::lower_with_metadata(db, ast, def_map, registered_types, metadata) {
        Ok((ir, def_map, type_metadata, registered_types, resolutions)) => LowerResult {
            ir,
            def_map,
            type_metadata,
            registered_types,
            resolutions,
        },
        Err(errors) => {
            for e in errors.0 {
                Diagnostic {
                    message: e.to_string(),
                    offset: 0,
                    len: 0,
                    severity: Severity::Error,
                }
                .accumulate(db);
            }
            LowerResult {
                ir: Default::default(),
                def_map: Default::default(),
                type_metadata: Default::default(),
                registered_types: Default::default(),
                resolutions: Default::default(),
            }
        }
    }
}

// ── infer ───────────────────────────────────────────────────────────

/// Type-check the IR. Returns IrProgram with per-expression types.
#[salsa::tracked]
pub fn infer(db: &dyn Db, file: SourceFile) -> IrProgram {
    use salsa::Accumulator;
    let lowered = lower(db, file);
    match TypeChecker::new(
        db,
        lowered.ir,
        lowered.def_map,
        lowered.registered_types,
        lowered.type_metadata,
        lowered.resolutions,
    )
    .check()
    {
        Ok(program) => program,
        Err(errors) => {
            for e in errors.0 {
                Diagnostic {
                    message: e.to_string(),
                    offset: 0,
                    len: 0,
                    severity: Severity::Error,
                }
                .accumulate(db);
            }
            IrProgram {
                ir: Default::default(),
                def_map: Default::default(),
                registered_types: Default::default(),
                type_metadata: Default::default(),
                type_index: Default::default(),
                resolutions: Default::default(),
                typeck_results: Default::default(),
            }
        }
    }
}

// ── rq ──────────────────────────────────────────────────────────────

/// Lower typed IR to RelationalQuery.
#[salsa::tracked]
pub fn rq(db: &dyn Db, file: SourceFile) -> RelationalQuery {
    use salsa::Accumulator;
    let program = infer(db, file);
    let reg = registry();
    match RqLowering::new(
        db,
        &program.ir,
        &program.type_index,
        &program.resolutions,
        reg,
    )
    .lower()
    {
        Ok(rq) => rq,
        Err(e) => {
            Diagnostic {
                message: e.to_string(),
                offset: 0,
                len: 0,
                severity: Severity::Error,
            }
            .accumulate(db);
            RelationalQuery::new()
        }
    }
}

// ── plan ────────────────────────────────────────────────────────────

/// Compile source to FossilPlan.
#[salsa::tracked]
pub fn plan(db: &dyn Db, file: SourceFile) -> FossilPlan {
    let rq = rq(db, file);
    let sql = rq_to_sql(&rq);
    FossilPlan::from_rq(rq, sql)
}

// ── Helpers ─────────────────────────────────────────────────────────

fn register_builtins(db: &dyn Db, def_map: &mut DefMap, reg: &Registry) {
    for func in &reg.functions {
        if func.namespace.is_empty() {
            let sym = Symbol::intern(func.name);
            def_map.insert(db, None, sym, DefKind::Let);
        } else {
            let ns_sym = Symbol::intern(func.namespace);
            let ns_def = def_map
                .get_by_symbol(ns_sym)
                .unwrap_or_else(|| def_map.insert(db, None, ns_sym, DefKind::Mod));
            let name_sym = Symbol::intern(func.name);
            def_map.insert(db, Some(ns_def), name_sym, DefKind::Let);
        }
    }
}

fn register_provider_schemas_from_ast(
    db: &dyn Db,
    ast: &crate::ast::Ast,
    def_map: &mut DefMap,
    registered_types: &mut RegisteredTypes,
    reg: &Registry,
) {
    for call in find_provider_calls_from_ast(ast) {
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
        let fields: Vec<(&str, BuiltInFieldType)> = columns
            .iter()
            .map(|(name, type_str)| {
                let prim = map_sql_type(type_str);
                (name.as_str(), BuiltInFieldType::Required(prim))
            })
            .collect();
        def_map.register_record_type(db, registered_types, &call.provider, fields);
    }
}

fn find_provider_calls_from_ast(
    ast: &crate::ast::Ast,
) -> Vec<SchemaRequest> {
    use crate::ast::{ExprKind, StmtKind};
    use crate::common::{Literal, ProviderArgument};
    let mut calls = Vec::new();

    for &stmt_id in &ast.root {
        let stmt = ast.stmts.get(stmt_id);
        if let StmtKind::Let { value, .. } = &stmt.kind {
            let expr = ast.exprs.get(*value);
            if let ExprKind::ProviderInvocation { provider, args } = &expr.kind {
                let provider_name = provider.display_global();
                let path = args.iter().find_map(|arg| match arg {
                    ProviderArgument::Positional(Literal::String(s)) => {
                        Some(s.as_str())
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

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;

    #[test]
    fn parse_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let ast = parse(&db, file);
        assert!(!ast.root.is_empty());
    }

    #[test]
    fn lower_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let result = lower(&db, file);
        assert!(!result.ir.root.is_empty());
    }

    #[test]
    fn infer_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let program = infer(&db, file);
        assert!(!program.ir.root.is_empty());
    }

    #[test]
    fn rq_literal() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let r = rq(&db, file);
        assert!(r.transforms.is_empty());
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
        assert!(reg.find_source("excel").is_some());
    }

    #[test]
    fn map_sql_types() {
        assert_eq!(map_sql_type("INTEGER"), PrimitiveType::Int);
        assert_eq!(map_sql_type("BIGINT"), PrimitiveType::Int);
        assert_eq!(map_sql_type("DOUBLE"), PrimitiveType::Float);
        assert_eq!(map_sql_type("VARCHAR"), PrimitiveType::String);
        assert_eq!(map_sql_type("BOOLEAN"), PrimitiveType::Bool);
    }

    #[test]
    fn salsa_memoization() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        // Call plan twice — second should use cached result
        let p1 = plan(&db, file);
        let p2 = plan(&db, file);
        assert_eq!(p1.sql, p2.sql);
    }
}
