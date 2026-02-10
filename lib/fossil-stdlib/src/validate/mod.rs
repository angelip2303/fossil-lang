//! ShEx-driven validation for BIM data
//!
//! `ValidWall.check` reads `@validate(...)` attributes from type metadata
//! and generates Polars boolean expressions to validate data against ShEx constraints.
//!
//! Types with `@validate(...)` attributes automatically get a `check()` method
//! via the auto-method system. The method returns `Validated<T>`.
//!
//! # Usage
//!
//! ```fossil
//! let source =
//!     IfcWall.load("building.ifc")
//!     |> fn wall ->
//!         ValidWall("http://example.com/wall/${wall.GlobalId}") {
//!             name = wall.Name,
//!         }
//!     end
//!
//! source |> ValidWall.check
//! ```

pub mod constraints;
pub mod metadata;

use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, StmtKind, TypeKind, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::{Plan, Transform, Value};
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};

use polars::prelude::*;

use metadata::ValidateFieldAttrs;
use constraints::field_checks;

/// A required field discovered by inspecting IR types.
/// Shared between `validate_plan()` and `extract_validate_rules()`.
pub(crate) struct RequiredField {
    pub field_name: String,
    pub field_sym: fossil_lang::context::Symbol,
    pub column_name: String,
}

/// Collect required (non-optional) fields for a type by inspecting the IR.
pub(crate) fn collect_required_fields(
    ir: &Ir,
    type_name: fossil_lang::context::Symbol,
    interner: &fossil_lang::context::Interner,
) -> Vec<RequiredField> {
    let Some(fields) = lookup_record_fields(ir, type_name) else {
        return Vec::new();
    };
    fields
        .iter()
        .filter(|(_, type_id)| !is_optional_type(ir, *type_id))
        .map(|(sym, _)| {
            let name = interner.resolve(*sym).to_string();
            let col_name = format!("_check_{}_required", name);
            RequiredField {
                field_name: name,
                field_sym: *sym,
                column_name: col_name,
            }
        })
        .collect()
}

pub struct ValidateCheckFunction;

impl FunctionImpl for ValidateCheckFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        // forall T. (T) -> Validated<T>
        let t_var = next_type_var();
        let t_ty = ir.var_type(t_var);

        let validated_def_id = gcx
            .interner
            .lookup("Validated")
            .and_then(|sym| gcx.definitions.get_by_symbol(sym).map(|d| d.id()))
            .expect("Validated type constructor must be registered");
        let validated_t = ir.app_type(validated_def_id, vec![t_ty]);

        Polytype::poly(vec![t_var], ir.fn_type(vec![t_ty], validated_t))
    }

    fn call(&self, args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        let mut args_iter = args.into_iter();

        let input = args_iter.next().ok_or_else(|| {
            FossilError::evaluation(
                "Validate.check requires a Plan argument".to_string(),
                fossil_lang::ast::Loc::generated(),
            )
        })?;

        match input {
            Value::Plan(plan) => validate_plan(plan, ctx),
            _ => Err(FossilError::evaluation(
                "Validate.check expects a Plan value".to_string(),
                fossil_lang::ast::Loc::generated(),
            )),
        }
    }
}

/// Check if an IR type is optional (`T?`)
pub(crate) fn is_optional_type(
    ir: &Ir,
    type_id: fossil_lang::ir::TypeId,
) -> bool {
    let ty = ir.types.get(type_id);
    matches!(&ty.kind, TypeKind::Optional(_))
}

/// Find the IR record type for a given type name by scanning IR statements.
pub(crate) fn lookup_record_fields(
    ir: &Ir,
    def_name: fossil_lang::context::Symbol,
) -> Option<Vec<(fossil_lang::context::Symbol, fossil_lang::ir::TypeId)>> {
    for &stmt_id in &ir.root {
        let stmt = ir.stmts.get(stmt_id);
        if let StmtKind::Type { name, ty, .. } = &stmt.kind
            && *name == def_name
        {
            let ty_node = ir.types.get(*ty);
            if let TypeKind::Record(fields) = &ty_node.kind {
                return Some(fields.to_fields());
            }
        }
    }
    None
}

/// Apply validation checks to a Plan based on type metadata and IR types.
///
/// Required checks are inferred from the IR: `T` fields get `is_not_null()` checks,
/// `T?` fields do not. Attribute-based checks (min_length, pattern, etc.)
/// are extracted from `@validate(...)` annotations.
fn validate_plan(mut plan: Plan, ctx: &RuntimeContext) -> Result<Value, FossilError> {
    let interner = &ctx.gcx.interner;
    let ir = ctx.ir;
    let mut check_exprs: Vec<Expr> = Vec::new();

    // Collect type DefIds to check — from outputs or from plan itself
    let type_def_ids = plan.type_def_ids();
    if type_def_ids.is_empty() {
        return Ok(Value::Plan(plan));
    }

    for def_id in type_def_ids {
        // Generate type-based required checks from IR field types
        let def = ctx.gcx.definitions.get(def_id);
        for rf in collect_required_fields(ir, def.name, interner) {
            check_exprs.push(
                col(&rf.field_name).is_not_null().alias(&rf.column_name),
            );
        }

        // Generate attribute-based checks (min_length, pattern, etc.)
        if let Some(tm) = ctx.gcx.type_metadata.get(&def_id) {
            for (field_sym, field_meta) in &tm.field_metadata {
                let field_name = interner.resolve(*field_sym);
                let attrs = ValidateFieldAttrs::from_field_metadata(field_meta, interner);
                for (expr, _constraint, _message) in field_checks(field_name, &attrs) {
                    check_exprs.push(expr);
                }
            }
        }
    }

    if check_exprs.is_empty() {
        // No validation constraints found, return plan unchanged
        return Ok(Value::Plan(plan));
    }

    // Add check columns as a Select transform
    // Keep all existing columns plus add the check columns
    let mut all_select: Vec<Expr> = vec![col("*")];
    all_select.extend(check_exprs);
    plan.transforms.push(Transform::Select(all_select));

    Ok(Value::Plan(plan))
}
