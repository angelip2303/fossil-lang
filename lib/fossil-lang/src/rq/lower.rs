//! IR → RQ lowering.
//!
//! Walks the Fossil IR (same structure as IrEvaluator) but produces
//! RQ transforms instead of executing with Polars.
//!
//! Each ExprKind maps to an RqValue (column expression, table reference,
//! or entity emission).

use std::collections::HashMap;

use crate::context::{DefId, DefKind, Symbol};
use crate::error::FossilError;
use crate::ir::{ExprId, ExprKind, Ir, Resolutions, TypeIndex, TypeckResults};
use crate::passes::GlobalContext;
use crate::registry::Registry;
use crate::rq::{
    ColId, EmissionDecl, JoinKind, OutputDecl, RelationalQuery, RqExpr, RqLiteral, ScanSource,
    TableId, Transform,
};

/// Internal value during lowering — NOT part of the final RQ.
#[derive(Clone, Debug)]
enum RqValue {
    Unit,
    Expr(RqExpr),
    Table(TableId),
    Emission {
        table: TableId,
        specs: Vec<EmissionSpec>,
    },
    Reference {
        def_id: DefId,
        keys: Vec<RqExpr>,
    },
}

#[derive(Clone, Debug)]
struct EmissionSpec {
    type_def_id: DefId,
    type_name: String,
    select_items: Vec<(String, RqExpr)>,
    identity_exprs: Vec<(String, RqExpr)>,
}

impl RqValue {
    fn into_expr(self) -> Result<RqExpr, FossilError> {
        match self {
            RqValue::Expr(e) => Ok(e),
            other => Err(FossilError::evaluation(
                format!("Expected expression, got {:?}", other),
                crate::ast::Loc::generated(),
            )),
        }
    }

    fn into_table(self) -> Result<TableId, FossilError> {
        match self {
            RqValue::Table(t) => Ok(t),
            RqValue::Emission { table, .. } => Ok(table),
            other => Err(FossilError::evaluation(
                format!("Expected table, got {:?}", other),
                crate::ast::Loc::generated(),
            )),
        }
    }
}

/// RQ lowering context.
pub struct RqLowering<'a> {
    ir: &'a Ir,
    gcx: &'a GlobalContext,
    type_index: &'a TypeIndex,
    resolutions: &'a Resolutions,
    _typeck: &'a TypeckResults,
    registry: &'a Registry,
    env: HashMap<Symbol, RqValue>,
    rq: RelationalQuery,
    table_counter: usize,
}

impl<'a> RqLowering<'a> {
    pub fn new(
        ir: &'a Ir,
        gcx: &'a GlobalContext,
        type_index: &'a TypeIndex,
        resolutions: &'a Resolutions,
        typeck: &'a TypeckResults,
        registry: &'a Registry,
    ) -> Self {
        Self {
            ir,
            gcx,
            type_index,
            resolutions,
            _typeck: typeck,
            registry,
            env: HashMap::new(),
            rq: RelationalQuery::new(),
            table_counter: 0,
        }
    }

    /// Lower the entire program to a RelationalQuery.
    pub fn lower(mut self) -> Result<RelationalQuery, FossilError> {
        let root = self.ir.root.clone();
        for &stmt_id in &root {
            self.lower_stmt(stmt_id)?;
        }
        Ok(self.rq)
    }

    fn lower_stmt(&mut self, stmt_id: crate::ir::StmtId) -> Result<(), FossilError> {
        let stmt = self.ir.stmts.get(stmt_id);
        match &stmt.kind.clone() {
            crate::ir::StmtKind::Let { name, value, .. } => {
                let val = self.lower_expr(*value)?;
                self.env.insert(*name, val);
            }
            crate::ir::StmtKind::Type { .. } => {}
            crate::ir::StmtKind::Expr(expr_id) => {
                self.lower_expr(*expr_id)?;
            }
        }
        Ok(())
    }

    fn lower_expr(&mut self, expr_id: ExprId) -> Result<RqValue, FossilError> {
        let expr_id = self
            .resolutions
            .expr_rewrites
            .get(&expr_id)
            .copied()
            .unwrap_or(expr_id);
        let expr = self.ir.exprs.get(expr_id);
        let kind = expr.kind.clone();

        match &kind {
            ExprKind::Literal(lit) => {
                use crate::ir::Literal;
                Ok(RqValue::Expr(match lit {
                    Literal::Integer(i) => RqExpr::Lit(RqLiteral::Integer(*i)),
                    Literal::String(s) => {
                        let text = self.gcx.interner.resolve(*s).to_string();
                        RqExpr::Lit(RqLiteral::String(text))
                    }
                    Literal::Boolean(b) => RqExpr::Lit(RqLiteral::Boolean(*b)),
                }))
            }

            ExprKind::Identifier(_) => {
                let &def_id = self
                    .resolutions
                    .expr_defs
                    .get(&expr_id)
                    .ok_or_else(|| {
                        FossilError::evaluation(
                            "Unresolved identifier in RQ lowering",
                            self.ir.exprs.get(expr_id).loc,
                        )
                    })?;

                let def = self.gcx.definitions.get(def_id);

                if let Some(val) = self.env.get(&def.name) {
                    return Ok(val.clone());
                }

                match &def.kind {
                    DefKind::RecordConstructor => Ok(RqValue::Unit), // handled in RecordInstance
                    _ => Ok(RqValue::Unit),
                }
            }

            ExprKind::FieldAccess { field, .. } => {
                let field_name = self.gcx.interner.resolve(*field).to_string();
                let col = self.rq.intern_col(&field_name);
                Ok(RqValue::Expr(RqExpr::Col(col)))
            }

            ExprKind::Coalesce { value, default } => {
                let v = self.lower_expr(*value)?.into_expr()?;
                let d = self.lower_expr(*default)?.into_expr()?;
                Ok(RqValue::Expr(RqExpr::Coalesce(Box::new(v), Box::new(d))))
            }

            ExprKind::StringInterpolation { parts, exprs } => {
                let mut concat_parts = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    let s = self.gcx.interner.resolve(*part).to_string();
                    if !s.is_empty() {
                        concat_parts.push(RqExpr::Lit(RqLiteral::String(s)));
                    }
                    if i < exprs.len() {
                        let val = self.lower_expr(exprs[i])?.into_expr()?;
                        concat_parts.push(RqExpr::Cast(Box::new(val), "VARCHAR".into()));
                    }
                }
                Ok(RqValue::Expr(RqExpr::Concat(concat_parts)))
            }

            ExprKind::Join {
                left,
                right,
                left_on,
                right_on,
                suffix,
            } => {
                let left_table = self.lower_expr(*left)?.into_table()?;
                let right_table = self.lower_expr(*right)?.into_table()?;

                let on: Vec<(ColId, ColId)> = left_on
                    .iter()
                    .zip(right_on.iter())
                    .map(|(l, r)| {
                        let lname = self.gcx.interner.resolve(*l).to_string();
                        let rname = self.gcx.interner.resolve(*r).to_string();
                        (self.rq.intern_col(&lname), self.rq.intern_col(&rname))
                    })
                    .collect();

                let suffix_str = suffix.map(|s| self.gcx.interner.resolve(s).to_string());
                let output = self.next_table("joined");
                self.rq.transforms.push(Transform::Join {
                    left: left_table,
                    right: right_table,
                    output,
                    on,
                    kind: JoinKind::Inner,
                    suffix: suffix_str,
                });
                Ok(RqValue::Table(output))
            }

            ExprKind::Projection {
                source,
                binding,
                outputs,
            } => {
                let source_table = self.lower_expr(*source)?.into_table()?;
                self.env
                    .insert(*binding, RqValue::Table(source_table));

                let mut all_specs = Vec::new();
                for &out_expr in outputs {
                    let val = self.lower_expr(out_expr)?;
                    if let RqValue::Emission { specs, .. } = val {
                        all_specs.extend(specs);
                    }
                }

                if all_specs.is_empty() {
                    return Ok(RqValue::Table(source_table));
                }

                // For each emission spec, create a Project transform
                let spec = &all_specs[0]; // primary entity
                let output = self.next_table(&spec.type_name.to_lowercase());

                let columns: Vec<(ColId, RqExpr)> = spec
                    .select_items
                    .iter()
                    .map(|(name, expr)| (self.rq.intern_col(name), expr.clone()))
                    .collect();

                self.rq.transforms.push(Transform::Project {
                    input: source_table,
                    output,
                    columns,
                });

                // Register emission
                let identity_columns: Vec<ColId> = spec
                    .identity_exprs
                    .iter()
                    .map(|(name, _)| self.rq.intern_col(name))
                    .collect();

                let subject_template = if let Some((_, key_expr)) = spec.identity_exprs.first() {
                    // Build subject IRI: CONCAT(base, '/', key)
                    RqExpr::Concat(vec![
                        RqExpr::Lit(RqLiteral::String(format!(
                            "http://example.org/{}/",
                            spec.type_name
                        ))),
                        RqExpr::Cast(Box::new(key_expr.clone()), "VARCHAR".into()),
                    ])
                } else {
                    RqExpr::Lit(RqLiteral::Null)
                };

                let fields: Vec<(String, ColId)> = spec
                    .select_items
                    .iter()
                    .map(|(name, _)| (name.clone(), self.rq.intern_col(name)))
                    .collect();

                self.rq.emissions.push(EmissionDecl {
                    table: output,
                    type_name: spec.type_name.clone(),
                    subject_template,
                    fields,
                    identity_columns,
                });

                Ok(RqValue::Emission {
                    table: output,
                    specs: all_specs,
                })
            }

            ExprKind::RecordInstance {
                ctor_args,
                spread: _,
                fields,
                ..
            } => {
                let type_def_id = self.resolutions.expr_defs.get(&expr_id).copied();
                let type_name = type_def_id
                    .map(|id| {
                        let def = self.gcx.definitions.get(id);
                        self.gcx.interner.resolve(def.name).to_string()
                    })
                    .unwrap_or_else(|| "Unknown".to_string());

                let mut select_items = Vec::new();
                let mut identity_exprs = Vec::new();

                // Process ctor args (identity columns)
                if let Some(type_def_id) = type_def_id {
                    if let Some(info) = self.type_index.get(type_def_id) {
                        for (i, arg) in ctor_args.iter().enumerate() {
                            let val = self.lower_expr(arg.value())?.into_expr()?;
                            let name = info
                                .ctor_param_names
                                .get(i)
                                .map(|n| self.gcx.interner.resolve(*n).to_string())
                                .unwrap_or_else(|| format!("_ctor_{i}"));
                            identity_exprs.push((name, val));
                        }
                    }
                }

                // Process fields
                for (name_sym, expr_id) in fields {
                    let name = self.gcx.interner.resolve(*name_sym).to_string();
                    let val = self.lower_expr(*expr_id)?;
                    let rq_expr = match val {
                        RqValue::Expr(e) => e,
                        RqValue::Reference { keys, .. } => {
                            // Reference to another entity — use its key as the field value
                            keys.into_iter()
                                .next()
                                .unwrap_or(RqExpr::Lit(RqLiteral::Null))
                        }
                        _ => RqExpr::Lit(RqLiteral::Null),
                    };
                    select_items.push((name, rq_expr));
                }

                Ok(RqValue::Emission {
                    table: TableId(0), // placeholder, projection sets real table
                    specs: vec![EmissionSpec {
                        type_def_id: type_def_id.unwrap_or(DefId::new(0)),
                        type_name,
                        select_items,
                        identity_exprs,
                    }],
                })
            }

            ExprKind::Application {
                callee,
                args,
                ..
            } => {
                // Check if callee resolves to a known function
                let callee_def_id = self.resolutions.expr_defs.get(callee).copied();

                if let Some(def_id) = callee_def_id {
                    let def = self.gcx.definitions.get(def_id);
                    let func_name = self.gcx.interner.resolve(def.name).to_string();
                    let parent = def.parent().map(|p| {
                        let pdef = self.gcx.definitions.get(p);
                        self.gcx.interner.resolve(pdef.name).to_string()
                    });
                    let namespace = parent.as_deref().unwrap_or("");

                    // Try Registry lookup
                    if let Some(func_def) = self.registry.find_function(namespace, &func_name) {
                        return self.lower_registry_function(func_def, args);
                    }

                    // Try provider lookup (legacy: csv!, excel!)
                    if matches!(def.kind, DefKind::Provider(_)) {
                        return self.lower_provider_call(&func_name, args);
                    }

                    // Function call — evaluate args, try to apply
                    if let DefKind::Func(_) = &def.kind {
                        // For known output functions (Rdf.materialize), handle specially
                        if namespace == "Rdf" && func_name == "materialize" {
                            return self.lower_materialize(args);
                        }
                    }
                }

                // Generic application — evaluate and return first arg
                let mut result = RqValue::Unit;
                for arg in args {
                    result = self.lower_expr(arg.value())?;
                }
                Ok(result)
            }

            ExprKind::Ref { args, .. } => {
                let type_def_id = self
                    .resolutions
                    .expr_defs
                    .get(&expr_id)
                    .copied()
                    .unwrap_or(DefId::new(0));
                let keys: Vec<RqExpr> = args
                    .iter()
                    .map(|&a| self.lower_expr(a).and_then(|v| v.into_expr()))
                    .collect::<Result<_, _>>()?;
                Ok(RqValue::Reference {
                    def_id: type_def_id,
                    keys,
                })
            }

            ExprKind::Unit => Ok(RqValue::Unit),
        }
    }

    fn lower_provider_call(
        &mut self,
        provider_name: &str,
        args: &[crate::ir::Argument],
    ) -> Result<RqValue, FossilError> {
        // Look up provider as a source in registry
        if let Some(func_def) = self.registry.find_source(provider_name) {
            return self.lower_registry_function(func_def, args);
        }

        // Fallback: generate a SQL scan from the provider name
        let path = args
            .first()
            .and_then(|a| {
                let val = self.lower_expr(a.value()).ok()?;
                match val {
                    RqValue::Expr(RqExpr::Lit(RqLiteral::String(s))) => Some(s),
                    _ => None,
                }
            })
            .unwrap_or_default();

        let table = self.next_table(provider_name);
        let sql = format!(
            "SELECT * FROM read_{provider_name}('{path}')"
        );
        self.rq.transforms.push(Transform::Scan {
            output: table,
            source: ScanSource::Sql(sql),
        });
        Ok(RqValue::Table(table))
    }

    fn lower_registry_function(
        &mut self,
        func_def: &crate::registry::FunctionDef,
        args: &[crate::ir::Argument],
    ) -> Result<RqValue, FossilError> {
        match &func_def.impl_ {
            crate::registry::OpImpl::SourceSql(template) => {
                // Extract path from first arg
                let path = args
                    .first()
                    .and_then(|a| {
                        let val = self.lower_expr(a.value()).ok()?;
                        match val {
                            RqValue::Expr(RqExpr::Lit(RqLiteral::String(s))) => Some(s),
                            _ => None,
                        }
                    })
                    .unwrap_or_default();

                let sql = template.replace("{path}", &path);
                // Fill in optional params with defaults
                let sql = sql.replace("{delimiter}", ",");
                let sql = sql.replace("{header}", "true");
                let sql = sql.replace("{sheet}", "Sheet1");

                let table = self.next_table(&format!("src_{}", func_def.name));
                self.rq.transforms.push(Transform::Scan {
                    output: table,
                    source: ScanSource::Sql(sql),
                });
                Ok(RqValue::Table(table))
            }

            crate::registry::OpImpl::Preprocess { handler } => {
                let path = args
                    .first()
                    .and_then(|a| {
                        let val = self.lower_expr(a.value()).ok()?;
                        match val {
                            RqValue::Expr(RqExpr::Lit(RqLiteral::String(s))) => Some(s),
                            _ => None,
                        }
                    })
                    .unwrap_or_default();

                let table_name = format!("_preprocess_{}", func_def.name);
                let table = self.next_table(&table_name);
                self.rq.transforms.push(Transform::Scan {
                    output: table,
                    source: ScanSource::Preprocess {
                        handler: handler.to_string(),
                        source_path: path,
                        output_table: table_name,
                        schema: Vec::new(),
                    },
                });
                Ok(RqValue::Table(table))
            }

            crate::registry::OpImpl::Output { format } => {
                // Handle in lower_materialize
                let _ = format;
                self.lower_materialize(args)
            }

            crate::registry::OpImpl::Pipeline { handler } => {
                let _ = handler;
                // Pipeline steps are collected for the host
                Ok(RqValue::Unit)
            }
        }
    }

    fn lower_materialize(
        &mut self,
        args: &[crate::ir::Argument],
    ) -> Result<RqValue, FossilError> {
        // First arg: emission, second arg: path
        let path = args
            .get(1)
            .or(args.first())
            .and_then(|a| {
                let val = self.lower_expr(a.value()).ok()?;
                match val {
                    RqValue::Expr(RqExpr::Lit(RqLiteral::String(s))) => Some(s),
                    _ => None,
                }
            })
            .unwrap_or_else(|| "output/".to_string());

        // Register output pointing to existing emissions
        let emission_indices: Vec<usize> = (0..self.rq.emissions.len()).collect();
        self.rq.outputs.push(OutputDecl {
            emissions: emission_indices,
            format: "graphar".to_string(),
            path,
            params: HashMap::new(),
        });

        Ok(RqValue::Unit)
    }

    fn next_table(&mut self, prefix: &str) -> TableId {
        self.table_counter += 1;
        self.rq
            .alloc_table(&format!("{}_{}", prefix, self.table_counter))
    }
}


#[cfg(test)]
#[cfg(feature = "polars")]
mod tests {
    use super::*;
    use crate::rq::emit_sql::rq_to_sql;

    /// Helper: compile source text to an IrProgram.
    fn compile(source: &str) -> crate::passes::IrProgram {
        let gcx = crate::passes::GlobalContext::default();
        let compiler = crate::compiler::Compiler::with_context(gcx);
        let result = compiler
            .compile(crate::compiler::CompilerInput::Source {
                name: "test".to_string(),
                content: source.to_string(),
            })
            .expect("compilation failed");
        result.program
    }

    #[test]
    fn lower_literal_produces_rq() {
        let program = compile("let x = 42");
        let registry = Registry::new();
        let rq = RqLowering::new(
            &program.ir,
            &program.gcx,
            &program.type_index,
            &program.resolutions,
            &program.typeck_results,
            &registry,
        )
        .lower()
        .expect("lowering failed");

        assert!(rq.transforms.is_empty());
    }

    #[test]
    fn lower_with_registry_csv() {
        let program = compile("let x = 42");
        let mut registry = Registry::new();
        crate::builtins::register(&mut registry);

        let rq = RqLowering::new(
            &program.ir,
            &program.gcx,
            &program.type_index,
            &program.resolutions,
            &program.typeck_results,
            &registry,
        )
        .lower()
        .expect("lowering failed");

        // With registry but no CSV call, still no transforms
        assert!(rq.transforms.is_empty());
    }

    #[test]
    fn rq_to_sql_roundtrip() {
        // Build an RQ manually and verify SQL output
        let mut rq = RelationalQuery::new();
        let id = rq.intern_col("id");
        let name = rq.intern_col("name");

        let src = rq.alloc_table("src_1");
        rq.transforms.push(Transform::Scan {
            output: src,
            source: ScanSource::Sql("SELECT * FROM read_csv('players.csv')".into()),
        });

        let persons = rq.alloc_table("persons_1");
        let subject_col = rq.intern_col("_subject");
        rq.transforms.push(Transform::Project {
            input: src,
            output: persons,
            columns: vec![
                (
                    subject_col,
                    RqExpr::Concat(vec![
                        RqExpr::str_lit("http://example.org/Person/"),
                        RqExpr::col(id).cast("VARCHAR"),
                    ]),
                ),
                (name, RqExpr::col(name)),
            ],
        });

        rq.emissions.push(EmissionDecl {
            table: persons,
            type_name: "Person".into(),
            subject_template: RqExpr::Concat(vec![
                RqExpr::str_lit("http://example.org/Person/"),
                RqExpr::col(id).cast("VARCHAR"),
            ]),
            fields: vec![("name".into(), name)],
            identity_columns: vec![id],
        });

        rq.outputs.push(OutputDecl {
            emissions: vec![0],
            format: "graphar".into(),
            path: "output/".into(),
            params: std::collections::HashMap::new(),
        });

        let sql = rq_to_sql(&rq);
        assert!(sql.contains("src_1 AS ("));
        assert!(sql.contains("read_csv('players.csv')"));
        assert!(sql.contains("persons_1 AS ("));
        assert!(sql.contains("CONCAT('http://example.org/Person/', CAST(id AS VARCHAR)) AS _subject"));
        assert!(sql.contains("SELECT * FROM persons_1"));

        // Verify plan can be built
        let plan = crate::plan::FossilPlan::from_rq(rq, sql);
        assert_eq!(plan.outputs.len(), 1);
        assert_eq!(plan.outputs[0].format, "graphar");
        assert_eq!(plan.outputs[0].projections.len(), 1);
        assert_eq!(plan.outputs[0].projections[0].type_name, "Person");
    }
}
