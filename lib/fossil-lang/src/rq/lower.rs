//! IR → RQ lowering.
//!
//! Walks the Fossil IR (same structure as IrEvaluator) but produces
//! RQ transforms instead of executing with Polars.
//!
//! Each ExprKind maps to an RqValue (column expression, table reference,
//! or entity emission).

use std::collections::HashMap;

use crate::db::{DefKindTag, Symbol};
use crate::db::Db;
use crate::error::FossilError;
use crate::ir::{ExprId, ExprKind, Ir, Resolutions, TypeIndex};
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
        keys: Vec<RqExpr>,
    },
}

#[derive(Clone, Debug)]
struct EmissionSpec {
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
    db: &'a dyn Db,
    ir: &'a Ir,
    type_index: &'a TypeIndex,
    resolutions: &'a Resolutions,
    registry: &'a Registry,
    env: HashMap<Symbol, RqValue>,
    rq: RelationalQuery,
    table_counter: usize,
}

impl<'a> RqLowering<'a> {
    pub fn new(
        db: &'a dyn Db,
        ir: &'a Ir,
        type_index: &'a TypeIndex,
        resolutions: &'a Resolutions,
        registry: &'a Registry,
    ) -> Self {
        Self {
            db,
            ir,
            type_index,
            resolutions,
            registry,
            env: HashMap::new(),
            rq: RelationalQuery::new(),
            table_counter: 0,
        }
    }

    /// Lower the entire program to a RelationalQuery.
    pub fn lower(mut self) -> Result<RelationalQuery, FossilError> {
        let root: Vec<_> = self.ir.root.iter().copied().collect();
        for stmt_id in root {
            self.lower_stmt(stmt_id)?;
        }
        Ok(self.rq)
    }

    fn lower_stmt(&mut self, stmt_id: crate::ir::StmtId) -> Result<(), FossilError> {
        // Extract small values from IR stmt, then drop borrow
        let (name, value, _) = {
            let stmt = &self.ir.stmts[stmt_id];
            match &stmt.kind {
                crate::ir::StmtKind::Let { name, value, .. } => (Some(*name), Some(*value), false),
                crate::ir::StmtKind::Type { .. } => return Ok(()),
                crate::ir::StmtKind::Expr(expr_id) => (None, Some(*expr_id), true),
            }
        };
        if let (Some(name), Some(value)) = (name, value) {
            let val = self.lower_expr(value)?;
            self.env.insert(name, val);
        } else if let (None, Some(expr_id)) = (name, value) {
            self.lower_expr(expr_id)?;
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
        // Extract kind by cloning — ExprKind contains nested Vecs that are needed
        // by value in the match arms. TODO: further optimize with snapshot extraction.
        let kind = &self.ir.exprs[expr_id].kind.clone();

        match &kind {
            ExprKind::Literal(lit) => {
                use crate::ir::Literal;
                Ok(RqValue::Expr(match lit {
                    Literal::Integer(i) => RqExpr::Lit(RqLiteral::Integer(*i)),
                    Literal::String(s) => {
                        RqExpr::Lit(RqLiteral::String(s.text(self.db).to_string()))
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
                            self.ir.exprs[expr_id].loc,
                        )
                    })?;

                let name = def_id.name(self.db);

                if let Some(val) = self.env.get(&name) {
                    return Ok(val.clone());
                }

                match def_id.kind(self.db) {
                    DefKindTag::RecordConstructor => Ok(RqValue::Unit), // handled in RecordInstance
                    _ => Ok(RqValue::Unit),
                }
            }

            ExprKind::FieldAccess { field, .. } => {
                let field_name = field.text(self.db).to_string();
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
                    let s = part.text(self.db).to_string();
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
                        let lname = l.text(self.db).to_string();
                        let rname = r.text(self.db).to_string();
                        (self.rq.intern_col(&lname), self.rq.intern_col(&rname))
                    })
                    .collect();

                let suffix_str = suffix.map(|s| s.text(self.db).to_string());
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
                    .map(|id| id.name(self.db).text(self.db).to_string())
                    .unwrap_or_else(|| "_unresolved".to_string());

                let mut select_items = Vec::new();
                let mut identity_exprs = Vec::new();

                // Process ctor args (identity columns)
                if let Some(type_def_id) = type_def_id {
                    if let Some(info) = self.type_index.get(&type_def_id) {
                        for (i, arg) in ctor_args.iter().enumerate() {
                            let val = self.lower_expr(arg.value())?.into_expr()?;
                            let name = info
                                .ctor_param_names
                                .get(i)
                                .map(|n| n.text(self.db).to_string())
                                .unwrap_or_else(|| format!("_ctor_{i}"));
                            identity_exprs.push((name, val));
                        }
                    }
                }

                // Process fields
                for (name_sym, expr_id) in fields {
                    let name = name_sym.text(self.db).to_string();
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
                    let func_name = def_id.name(self.db).text(self.db).to_string();
                    let namespace = def_id.namespace(self.db)
                        .map(|ns| ns.text(self.db).to_string())
                        .unwrap_or_default();

                    // Try Registry lookup
                    if let Some(func_def) = self.registry.find_function(&namespace, &func_name) {
                        return self.lower_registry_function(func_def, args);
                    }

                    // Try provider lookup via Registry (csv!, excel!)
                    if self.registry.find_source(&func_name).is_some() {
                        return self.lower_provider_call(&func_name, args);
                    }

                    // For known output functions (Rdf.materialize), handle specially
                    if namespace == "Rdf" && func_name == "materialize" {
                        return self.lower_materialize(args);
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
                let keys: Vec<RqExpr> = args
                    .iter()
                    .map(|&a| self.lower_expr(a).and_then(|v| v.into_expr()))
                    .collect::<Result<_, _>>()?;
                Ok(RqValue::Reference { keys })
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

                let mut sql = template.replace("{path}", &path);
                // Fill in optional params with defaults from FunctionDef
                for param in func_def.params {
                    if !param.default.is_empty() {
                        sql = sql.replace(&format!("{{{}}}", param.name), param.default);
                    }
                }

                let table = self.next_table(&format!("src_{}", func_def.name));
                self.rq.transforms.push(Transform::Scan {
                    output: table,
                    source: ScanSource::Sql(sql),
                });
                Ok(RqValue::Table(table))
            }

            crate::registry::OpImpl::Preprocess { handler, .. } => {
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
