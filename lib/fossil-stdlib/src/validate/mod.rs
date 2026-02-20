use std::sync::Arc;

use polars::prelude::*;

use fossil_lang::context::{DefId, DefKind, Symbol};
use fossil_lang::context::global::TypeInfo;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{Plan, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{FunctionDef, ModuleSpec};
use fossil_macros::FromAttrs;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValidateMode {
    Validate,
    Errors,
}

/// Generates `Type.validate()` and `Type.errors()` for record types with `#[validate]` attributes
pub fn validate_module_generator(
    validation_error_def_id: DefId,
) -> Arc<dyn Fn(&TypeInfo) -> Option<ModuleSpec> + Send + Sync> {
    Arc::new(move |info: &TypeInfo| {
        let validate_sym = info.interner.lookup("validate")?;
        let has_validate = info
            .fields
            .iter()
            .any(|f| f.attrs.iter().any(|a| a.name == validate_sym));

        if !has_validate {
            return None;
        }

        Some(ModuleSpec {
            functions: vec![
                FunctionDef::new("validate", ValidateFunction {
                    type_name: info.name,
                    mode: ValidateMode::Validate,
                    validation_error_def_id,
                }),
                FunctionDef::new("errors", ValidateFunction {
                    type_name: info.name,
                    mode: ValidateMode::Errors,
                    validation_error_def_id,
                }),
            ],
        })
    })
}

#[derive(Debug, Clone, FromAttrs)]
pub struct ValidateFieldAttrs {
    #[attr("validate.min_length", field)]
    pub min_length: Option<i64>,
    #[attr("validate.max_length", field)]
    pub max_length: Option<i64>,
    #[attr("validate.pattern", field)]
    pub pattern: Option<String>,
}

struct Constraint {
    field: String,
    name: &'static str,
    expected: String,
    /// Expression that is true when the constraint is SATISFIED
    valid_expr: Expr,
}

fn resolve_type_def_id(ctx: &RuntimeContext, type_name: Symbol) -> Option<DefId> {
    ctx.gcx
        .definitions
        .find_by_symbol(type_name, |k| matches!(k, DefKind::Type))
        .map(|d| d.id())
}

fn extract_constraints(ctx: &RuntimeContext, type_name: Symbol) -> Vec<Constraint> {
    let Some(type_def_id) = resolve_type_def_id(ctx, type_name) else {
        return Vec::new();
    };

    let Some(metadata) = ctx.gcx.type_metadata.get(&type_def_id) else {
        return Vec::new();
    };

    let mut constraints = Vec::new();

    for (field_sym, field_meta) in &metadata.field_metadata {
        let field_name = ctx.gcx.interner.resolve(*field_sym);
        let attrs = ValidateFieldAttrs::from_field_metadata(field_meta, &ctx.gcx.interner);

        if let Some(n) = attrs.min_length {
            constraints.push(Constraint {
                field: field_name.to_string(),
                name: "min_length",
                expected: n.to_string(),
                valid_expr: col(field_name).str().len_chars().gt_eq(lit(n as u32)),
            });
        }

        if let Some(n) = attrs.max_length {
            constraints.push(Constraint {
                field: field_name.to_string(),
                name: "max_length",
                expected: n.to_string(),
                valid_expr: col(field_name).str().len_chars().lt_eq(lit(n as u32)),
            });
        }

        if let Some(ref pat) = attrs.pattern {
            constraints.push(Constraint {
                field: field_name.to_string(),
                name: "pattern",
                expected: pat.clone(),
                valid_expr: col(field_name).str().contains(lit(pat.as_str()), false),
            });
        }
    }

    constraints
}

fn combine_valid_filter(constraints: &[Constraint]) -> Option<Expr> {
    constraints
        .iter()
        .map(|c| c.valid_expr.clone())
        .reduce(|a, b| a.and(b))
}

pub fn validation_error_schema() -> Schema {
    Schema::from_iter(VALIDATION_ERROR_FIELDS.map(|name| {
        Field::new(name.into(), DataType::String)
    }))
}

const VALIDATION_ERROR_FIELDS: [&str; 5] =
    ["source_type", "field", "constraint", "expected", "actual"];

pub struct ValidateFunction {
    type_name: Symbol,
    mode: ValidateMode,
    validation_error_def_id: DefId,
}

impl ValidateFunction {
    fn take_plan(args: Vec<Value>, label: &str) -> Result<Plan, FossilError> {
        let loc = fossil_lang::ast::Loc::generated();
        let input = args
            .into_iter()
            .next()
            .ok_or_else(|| FossilError::evaluation(format!("{label} requires a plan argument"), loc))?;
        match input {
            Value::Plan(p) => Ok(p),
            _ => Err(FossilError::evaluation(format!("{label} expects a Plan"), loc)),
        }
    }

    fn call_validate(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let plan = Self::take_plan(args, "validate")?;
        let constraints = extract_constraints(ctx, self.type_name);

        let plan = if let Some(filter) = combine_valid_filter(&constraints) {
            plan.filter(filter)
        } else {
            plan
        };

        Ok(Value::Plan(plan))
    }

    fn call_errors(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let plan = Self::take_plan(args, "errors")?;
        let constraints = extract_constraints(ctx, self.type_name);

        let Some(valid_filter) = combine_valid_filter(&constraints) else {
            return Ok(Value::Plan(Plan::empty(validation_error_schema())));
        };

        let type_name = ctx.gcx.interner.resolve(self.type_name).to_string();

        // Build when/then chains — reports first violated constraint per row
        let violation_exprs: Vec<_> = constraints
            .iter()
            .map(|c| (c, c.valid_expr.clone().not()))
            .collect();

        let field_expr = build_when_then(&violation_exprs, |c| lit(c.field.clone()));
        let constraint_expr = build_when_then(&violation_exprs, |c| lit(c.name));
        let expected_expr = build_when_then(&violation_exprs, |c| lit(c.expected.clone()));
        let actual_expr =
            build_when_then(&violation_exprs, |c| col(&c.field).cast(DataType::String));

        let select_exprs = vec![
            lit(type_name).alias("source_type"),
            field_expr.alias("field"),
            constraint_expr.alias("constraint"),
            expected_expr.alias("expected"),
            actual_expr.alias("actual"),
        ];

        let error_plan = plan
            .filter(valid_filter.not())
            .select(select_exprs);

        Ok(Value::Plan(error_plan))
    }
}

fn build_when_then(
    violation_exprs: &[(&Constraint, Expr)],
    value_fn: impl Fn(&Constraint) -> Expr,
) -> Expr {
    let null = lit(Null {}).cast(DataType::String);
    let mut result = null;
    for (constraint, violation_cond) in violation_exprs.iter().rev() {
        result = when(violation_cond.clone())
            .then(value_fn(constraint))
            .otherwise(result);
    }
    result
}

impl FunctionImpl for ValidateFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);

        match self.mode {
            ValidateMode::Validate => {
                Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty], t_ty))
            }
            ValidateMode::Errors => {
                let ret_ty = ir.named_type(self.validation_error_def_id);
                Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty], ret_ty))
            }
        }
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        match self.mode {
            ValidateMode::Validate => self.call_validate(args, ctx),
            ValidateMode::Errors => self.call_errors(args, ctx),
        }
    }
}
