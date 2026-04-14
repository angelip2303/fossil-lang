//! IR → RQ lowering.
//!
//! Walks the Fossil IR and produces `sqlparser::ast::Cte` nodes directly,
//! alongside a source manifest (`SourceRef`) and the host-side materialization
//! plan (`EmissionDecl`/`OutputDecl`). There is no intermediate custom AST.
//!
//! Source scans register into `rq.sources` and flow downstream as plain
//! table-name references; the compiled SQL never inlines format-specific
//! reads like `read_csv(...)`. The host resolves each `SourceRef` against
//! its catalog before executing the query.

use std::collections::HashMap;

use sqlparser::ast::{
    self, BinaryOperator, Expr, Ident, Join, JoinConstraint, JoinOperator, SelectItem,
    TableWithJoins,
};

use crate::db::Db;
#[allow(unused_imports)] // trait must be in scope for method resolution
use crate::db::HasRegistry;
use crate::db::{DefKindTag, Symbol};
use crate::error::FossilError;
use crate::ir::{ExprId, ExprKind, Ir, Resolutions};
use crate::ty::typecheck::TypeIndex;
use crate::registry::SourceDef;
use crate::rq::{
    build::{self, cte, select_query, table_ref, twj, wildcard_item},
    EmissionDecl, OutputDecl, RelationalQuery, SourceRef,
};

/// Internal value during lowering — NOT part of the final RQ.
#[derive(Clone, Debug)]
enum RqValue {
    Unit,
    Expr(Expr),
    Table(Ident),
    Emission {
        table: Ident,
        specs: Vec<EmissionSpec>,
    },
    Reference {
        keys: Vec<Expr>,
    },
}

#[derive(Clone, Debug)]
struct EmissionSpec {
    type_name: String,
    select_items: Vec<(String, Expr)>,
    identity_exprs: Vec<(String, Expr)>,
}

impl RqValue {
    fn into_expr(self) -> Result<Expr, FossilError> {
        match self {
            RqValue::Expr(e) => Ok(e),
            other => Err(FossilError::evaluation(
                format!("Expected expression, got {:?}", other),
                crate::ast::Loc::generated(),
            )),
        }
    }

    fn into_table(self) -> Result<Ident, FossilError> {
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
    ) -> Self {
        Self {
            db,
            ir,
            type_index,
            resolutions,
            env: HashMap::new(),
            rq: RelationalQuery::new(),
            table_counter: 0,
        }
    }

    /// Find the named String value of an attribute argument on the type whose
    /// name matches `type_name`. Walks IR `Stmt::Type` nodes and inspects each
    /// attached `Attribute`. Returns the first match across all attributes —
    /// the compiler doesn't care which attribute supplied it (e.g. `#[rdf]`,
    /// `#[graph]`, etc.); only the key lookup matters.
    fn lookup_type_attr_string(&self, type_name: &str, key: &str) -> Option<String> {
        let key_sym = Symbol::new(self.db, key);
        for stmt in self.ir.stmts.iter() {
            if let crate::ir::StmtKind::Type { name, attrs, .. } = &stmt.1.kind {
                if name.text(self.db) != type_name {
                    continue;
                }
                for attr in attrs {
                    for arg in &attr.args {
                        if let crate::ast::AttributeArg::Named { key, value } = arg {
                            if *key == key_sym {
                                if let crate::ast::Literal::String(s) = value {
                                    return Some(s.text(self.db).to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
        None
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
                    Literal::Integer(i) => build::int_lit(*i),
                    Literal::String(s) => build::string_lit(s.text(self.db).to_string()),
                    Literal::Boolean(b) => build::bool_lit(*b),
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
                Ok(RqValue::Expr(build::col(field_name)))
            }

            ExprKind::Coalesce { value, default } => {
                let v = self.lower_expr(*value)?.into_expr()?;
                let d = self.lower_expr(*default)?.into_expr()?;
                Ok(RqValue::Expr(build::coalesce(v, d)))
            }

            ExprKind::StringInterpolation { parts, exprs } => {
                let mut concat_parts = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    let s = part.text(self.db).to_string();
                    if !s.is_empty() {
                        concat_parts.push(build::string_lit(s));
                    }
                    if i < exprs.len() {
                        let val = self.lower_expr(exprs[i])?.into_expr()?;
                        concat_parts.push(build::cast_varchar(val));
                    }
                }
                Ok(RqValue::Expr(build::concat(concat_parts)))
            }

            ExprKind::Join {
                left,
                right,
                left_on,
                right_on,
                suffix: _,
            } => {
                let left_table = self.lower_expr(*left)?.into_table()?;
                let right_table = self.lower_expr(*right)?.into_table()?;

                let on_pairs: Vec<(Ident, Ident)> = left_on
                    .iter()
                    .zip(right_on.iter())
                    .map(|(l, r)| {
                        (
                            Ident::new(l.text(self.db).to_string()),
                            Ident::new(r.text(self.db).to_string()),
                        )
                    })
                    .collect();

                let output = self.next_table("joined");
                let on_expr = join_on_expr(&left_table, &right_table, &on_pairs);
                let from = vec![TableWithJoins {
                    relation: table_ref(&left_table),
                    joins: vec![Join {
                        relation: table_ref(&right_table),
                        global: false,
                        join_operator: JoinOperator::Inner(JoinConstraint::On(on_expr)),
                    }],
                }];
                let query = select_query(vec![wildcard_item()], from, None);
                self.rq.ctes.push(cte(output.clone(), query));
                Ok(RqValue::Table(output))
            }

            ExprKind::Projection {
                source,
                binding,
                outputs,
            } => {
                let source_table = self.lower_expr(*source)?.into_table()?;
                self.env
                    .insert(*binding, RqValue::Table(source_table.clone()));

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

                let projection: Vec<SelectItem> = spec
                    .select_items
                    .iter()
                    .map(|(name, expr)| {
                        let alias = Ident::new(name.clone());
                        if let Expr::Identifier(id) = expr {
                            if id.value == alias.value {
                                return SelectItem::UnnamedExpr(expr.clone());
                            }
                        }
                        SelectItem::ExprWithAlias {
                            expr: expr.clone(),
                            alias,
                        }
                    })
                    .collect();

                let from = vec![twj(table_ref(&source_table))];
                let query = select_query(projection, from, None);
                self.rq.ctes.push(cte(output.clone(), query));

                // Register emission
                let identity_columns: Vec<Ident> = spec
                    .identity_exprs
                    .iter()
                    .map(|(name, _)| Ident::new(name.clone()))
                    .collect();

                let subject_template = if let Some((_, key_expr)) = spec.identity_exprs.first() {
                    // The base IRI is metadata: it lives on the type's own
                    // attributes (e.g. `#[rdf(base = "https://…/")] type User …`).
                    // fossil-lang doesn't know what "rdf" means — it just reads
                    // the named-arg `base` from any attribute on the type and
                    // uses its String literal verbatim. Falls back to a
                    // placeholder when no attribute provides one.
                    let base = self
                        .lookup_type_attr_string(&spec.type_name, "base")
                        .unwrap_or_else(|| "http://example.org/".to_string());
                    build::concat(vec![
                        build::string_lit(format!("{}{}/", base, spec.type_name)),
                        build::cast_varchar(key_expr.clone()),
                    ])
                } else {
                    build::null_lit()
                };

                let fields: Vec<(String, Ident)> = spec
                    .select_items
                    .iter()
                    .map(|(name, _)| (name.clone(), Ident::new(name.clone())))
                    .collect();

                self.rq.emissions.push(EmissionDecl {
                    table: output.clone(),
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
                            keys.into_iter().next().unwrap_or_else(build::null_lit)
                        }
                        _ => build::null_lit(),
                    };
                    select_items.push((name, rq_expr));
                }

                Ok(RqValue::Emission {
                    table: Ident::new(""), // placeholder, projection sets real table
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
                // Check if callee resolves to a known def (source or sink)
                let callee_def_id = self.resolutions.expr_defs.get(callee).copied();

                if let Some(def_id) = callee_def_id {
                    let func_name = def_id.name(self.db).text(self.db).to_string();
                    let namespace = def_id.namespace(self.db)
                        .map(|ns| ns.text(self.db).to_string())
                        .unwrap_or_default();

                    // Source lookup: simple identifier (no namespace) matches a registered source
                    if namespace.is_empty() {
                        if let Some(src) = self.db.registry().sources.find(&func_name).cloned() {
                            return self.lower_source(&src, args);
                        }
                    }

                    // Sink lookup: qualified path (namespace.name) matches a registered sink
                    if !namespace.is_empty()
                        && self
                            .db
                            .registry()
                            .sinks
                            .find(&namespace, &func_name)
                            .is_some()
                    {
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
                let keys: Vec<Expr> = args
                    .iter()
                    .map(|&a| self.lower_expr(a).and_then(|v| v.into_expr()))
                    .collect::<Result<_, _>>()?;
                Ok(RqValue::Reference { keys })
            }

            ExprKind::Unit => Ok(RqValue::Unit),
        }
    }

    fn lower_source(
        &mut self,
        src: &SourceDef,
        args: &[crate::ir::Argument],
    ) -> Result<RqValue, FossilError> {
        let path = self.extract_path(args);
        let params = self.extract_params(args, src);
        let alias = self.next_table(&format!("src_{}", src.name));
        self.rq.sources.push(SourceRef {
            alias: alias.clone(),
            format: src.name.clone(),
            path,
            params,
        });
        Ok(RqValue::Table(alias))
    }

    fn extract_path(&mut self, args: &[crate::ir::Argument]) -> String {
        args.first()
            .and_then(|a| {
                let val = self.lower_expr(a.value()).ok()?;
                match val {
                    RqValue::Expr(e) => build::expr_string_lit(&e).map(|s| s.to_string()),
                    _ => None,
                }
            })
            .unwrap_or_default()
    }

    fn extract_params(
        &mut self,
        args: &[crate::ir::Argument],
        src: &SourceDef,
    ) -> std::collections::HashMap<String, String> {
        let mut params = std::collections::HashMap::new();
        // Named args from the call
        for arg in args {
            if let crate::ir::Argument::Named { name, value } = arg {
                if let Ok(RqValue::Expr(e)) = self.lower_expr(*value) {
                    if let Some(val) = build::expr_to_param_string(&e) {
                        params.insert(name.text(self.db).to_string(), val);
                    }
                }
            }
        }
        // Fill defaults from SourceDef for missing params
        for param in &src.params {
            if !param.required && !params.contains_key(&param.name) {
                params.insert(param.name.clone(), param.default.clone());
            }
        }
        params
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
                    RqValue::Expr(e) => build::expr_string_lit(&e).map(|s| s.to_string()),
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

    fn next_table(&mut self, prefix: &str) -> Ident {
        self.table_counter += 1;
        Ident::new(format!("{}_{}", prefix, self.table_counter))
    }
}

fn join_on_expr(left: &Ident, right: &Ident, on: &[(Ident, Ident)]) -> Expr {
    let mut iter = on.iter().map(|(l, r)| Expr::BinaryOp {
        left: Box::new(Expr::CompoundIdentifier(vec![left.clone(), l.clone()])),
        op: BinaryOperator::Eq,
        right: Box::new(Expr::CompoundIdentifier(vec![right.clone(), r.clone()])),
    });
    let first = iter
        .next()
        .unwrap_or(Expr::Value(ast::Value::Boolean(true).into()));
    iter.fold(first, |acc, next| Expr::BinaryOp {
        left: Box::new(acc),
        op: BinaryOperator::And,
        right: Box::new(next),
    })
}
