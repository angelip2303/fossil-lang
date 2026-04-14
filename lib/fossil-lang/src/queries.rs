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
//!
//! The registry is now a field of `FossilDb` (no global state).
//! Sinks are registered as DefMap entries during lower; sources are NOT in
//! DefMap — they are resolved directly via `db.registry().sources` by the
//! lowering and RQ phases.

use crate::ast::Ast;
use crate::common::PrimitiveType;
#[allow(unused_imports)] // trait must be in scope for method resolution
use crate::db::HasRegistry;
use crate::db::{Db, DefKindTag, Diagnostic, Severity, SourceFile, Symbol};
use crate::error::emit_error;
use crate::def_map::{BuiltInFieldType, DefMap, RegisteredTypes};
use crate::metadata::extract_type_metadata;
use crate::syntax::parse::Parser;
use crate::passes::typecheck::TypeChecker;
use crate::passes::{InferResult, LowerResult};
use crate::rq::lower::RqLowering;
use crate::rq::RelationalQuery;

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
    find_provider_calls_from_ast(db, &ast)
}

// ── parse ───────────────────────────────────────────────────────────

/// Parse source text into AST.
#[salsa::tracked]
pub fn parse(db: &dyn Db, file: SourceFile) -> Ast {
    match Parser::parse(db, file.text(db), 0) {
        Ok(ast) => ast,
        Err(errors) => {
            for e in errors {
                let _ = emit_error(db, e);
            }
            Ast::default()
        }
    }
}

// ── lower ───────────────────────────────────────────────────────────

/// Lower AST to IR. Registers sinks into DefMap and resolves provider schemas.
#[salsa::tracked]
pub fn lower(db: &dyn Db, file: SourceFile) -> LowerResult {
    let ast = parse(db, file);

    let mut def_map = DefMap::default();
    let mut registered_types = RegisteredTypes::new();

    register_sinks_in_def_map(db, &mut def_map);
    register_provider_schemas_from_ast(db, &ast, &mut def_map, &mut registered_types);

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
            for e in errors {
                let _ = emit_error(db, e);
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

/// Type-check the IR. Returns only the new artifacts from type-checking.
#[salsa::tracked]
pub fn infer(db: &dyn Db, file: SourceFile) -> InferResult {
    let lowered = lower(db, file);
    // The type checker emits its own diagnostics into the accumulator and
    // taints failed bindings with `Ty::Error`, so we always get a populated
    // (best-effort) `InferResult` back.
    TypeChecker::new(
        db,
        lowered.ir,
        lowered.def_map,
        lowered.registered_types,
        lowered.resolutions,
    )
    .check()
}

// ── rq ──────────────────────────────────────────────────────────────

/// Lower typed IR to RelationalQuery.
#[salsa::tracked]
pub fn rq(db: &dyn Db, file: SourceFile) -> RelationalQuery {
    let program = infer(db, file);
    match RqLowering::new(db, &program.ir, &program.type_index, &program.resolutions).lower() {
        Ok(rq) => rq,
        Err(e) => {
            let _ = emit_error(db, e);
            RelationalQuery::new()
        }
    }
}

// plan() removed — host generates plan from rq() using its SqlDialect.
// See FossilPlan::from_rq(rq, dialect).

// ── Helpers ─────────────────────────────────────────────────────────

/// Register sinks into the DefMap so qualified path resolution
/// (e.g. `Rdf.materialize`) succeeds.
/// Sources are NOT registered here — they are resolved directly by lowering
/// (`ProviderInvocation` → `SourceCall` IR node) without going through DefMap.
pub(crate) fn register_sinks_in_def_map(db: &dyn Db, def_map: &mut DefMap) {
    use crate::def_map::Namespace;
    for sink in db.registry().sinks.iter() {
        let ns_sym = Symbol::new(db, &sink.namespace);
        let ns_def = def_map
            .get_in_ns(ns_sym, Namespace::TypeNS)
            .unwrap_or_else(|| def_map.insert(db, None, ns_sym, DefKindTag::Mod));
        let name_sym = Symbol::new(db, &sink.name);
        def_map.insert(db, Some(ns_def), name_sym, DefKindTag::Let);
    }
}

pub(crate) fn register_provider_schemas_from_ast(
    db: &dyn Db,
    ast: &crate::ast::Ast,
    def_map: &mut DefMap,
    registered_types: &mut RegisteredTypes,
) {
    for call in find_provider_calls_from_ast(db, ast) {
        let src = match db.registry().sources.find(&call.provider) {
            Some(s) => s,
            None => continue,
        };
        // Static schema → use directly. Dynamic schema → ask host.
        let columns: Vec<(String, String)> = match &src.schema {
            Some(schema) => schema.clone(),
            None => match db.source_schema(&call.provider, &call.path) {
                Some(cols) => cols,
                None => continue,
            },
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

fn find_provider_calls_from_ast(db: &dyn Db, ast: &crate::ast::Ast) -> Vec<SchemaRequest> {
    use crate::ast::{ExprKind, StmtKind};
    use crate::common::{Literal, ProviderArgument};
    let mut calls = Vec::new();

    for &stmt_id in &ast.root {
        let stmt = &ast.stmts[stmt_id];
        if let StmtKind::Let { value, .. } = &stmt.kind {
            let expr = &ast.exprs[*value];
            if let ExprKind::ProviderInvocation { provider, args } = &expr.kind {
                let provider_name = provider.display(db);
                let path = args.iter().find_map(|arg| match arg {
                    ProviderArgument::Positional(Literal::String(s)) => {
                        Some(s.text(db).to_string())
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

impl Diagnostic {
    /// Build a Diagnostic from a FossilError, preserving span info from miette.
    pub(crate) fn from_error(err: &crate::error::FossilError) -> Self {
        let (offset, len) = err.span_info().unwrap_or((0, 0));
        Self {
            message: err.to_string(),
            offset,
            len,
            severity: Severity::Error,
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
        use crate::dialect::DefaultDialect;
        use crate::plan::FossilPlan;
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let r = rq(&db, file);
        let p = FossilPlan::from_rq(r, &DefaultDialect);
        assert!(p.sources.is_empty());
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
    fn tainted_compilation_does_not_re_yell() {
        // First let fails (undefined variable). The second let references
        // the failed binding. With ErrorGuaranteed taint propagation, the
        // second let should NOT add a new diagnostic — it should silently
        // unify against `Ty::Error`. Net: exactly one accumulated
        // diagnostic for the whole file.
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "let x = does_not_exist\nlet y = x".into(),
            "test".into(),
        );
        let _ = infer(&db, file);
        let diags = infer::accumulated::<Diagnostic>(&db, file);
        assert!(
            !diags.is_empty(),
            "expected at least one diagnostic for the bad first let"
        );
        assert_eq!(
            diags.len(),
            1,
            "downstream use of tainted binding must not re-yell — got {} diagnostics: {:?}",
            diags.len(),
            diags
        );
    }

    #[test]
    fn tainted_compilation_two_failing_lets_two_diagnostics() {
        // Two independent failing lets, each referenced downstream. With
        // ErrorGuaranteed taint propagation, we expect EXACTLY two
        // diagnostics — one per root cause — and zero re-yelled follow-ups.
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "let a = nope_a\nlet b = nope_b\nlet c = a\nlet d = b".into(),
            "test".into(),
        );
        let _ = infer(&db, file);
        let diags = infer::accumulated::<Diagnostic>(&db, file);
        assert_eq!(
            diags.len(),
            2,
            "two failing lets must produce exactly two diagnostics, got: {:?}",
            diags
        );
    }

    #[test]
    fn salsa_memoization() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".into(), "test".into());
        let r1 = rq(&db, file);
        let r2 = rq(&db, file);
        assert_eq!(r1, r2);
    }

    #[test]
    fn default_db_has_rdf_materialize_sink() {
        let db = FossilDb::default();
        assert!(db.registry().sinks.find("Rdf", "materialize").is_some());
    }

    #[test]
    fn default_db_has_clean_attributes() {
        let db = FossilDb::default();
        assert!(db.registry().attributes.find("clean", "trim").is_some());
        assert!(db.registry().attributes.find("clean", "slug").is_some());
    }
}
