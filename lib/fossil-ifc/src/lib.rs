pub mod schema;
pub mod source;
pub(crate) mod step;

use std::path::PathBuf;

use fossil_lang::context::DefId;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::source::Source;

use polars::prelude::*;

use schema::{IfcEntityDef, ALL_ENTITIES};
use source::IfcSource;

/// Register all IFC entity types and their load functions
pub fn register(gcx: &mut GlobalContext) {
    for entity_def in ALL_ENTITIES {
        register_entity(gcx, entity_def);
    }
}

fn register_entity(gcx: &mut GlobalContext, entity_def: &'static IfcEntityDef) {
    let all_fields = entity_def.all_fields();

    let fields: Vec<(&str, _)> = all_fields
        .iter()
        .map(|f| (f.name, f.to_builtin_field_type()))
        .collect();

    let def_id = gcx.register_record_type_with_optionality(entity_def.fossil_name, fields);

    let schema = Schema::from_iter(all_fields.iter().map(|f| {
        Field::new(f.name.into(), f.data_type.clone())
    }));

    gcx.register_function(
        entity_def.fossil_name,
        "load",
        IfcEntityLoadFunction::new(entity_def, def_id, schema),
    );
}

struct IfcEntityLoadFunction {
    entity_def: &'static IfcEntityDef,
    def_id: DefId,
    schema: Schema,
}

impl IfcEntityLoadFunction {
    fn new(entity_def: &'static IfcEntityDef, def_id: DefId, schema: Schema) -> Self {
        Self {
            entity_def,
            def_id,
            schema,
        }
    }
}

impl FunctionImpl for IfcEntityLoadFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        _next_type_var: &mut dyn FnMut() -> TypeVar,
        _gcx: &GlobalContext,
    ) -> Polytype {
        let path_ty = ir.string_type();
        let record_ty = ir.named_type(self.def_id);
        let list_ty = ir.list_type(record_ty);
        Polytype::mono(ir.fn_type(vec![path_ty], list_ty))
    }

    fn call(&self, args: Vec<Value>, _ctx: &RuntimeContext) -> Result<Value, FossilError> {
        use fossil_lang::runtime::value::Plan;

        let path_str = args
            .first()
            .and_then(|v| v.as_literal_string())
            .ok_or_else(|| {
                FossilError::argument(
                    "'path' argument must be a string",
                    fossil_lang::ast::Loc::generated(),
                )
            })?;

        let path = PathBuf::from(&path_str);
        let source = IfcSource::new(path, self.entity_def);
        let plan =
            Plan::from_source_with_type(source.box_clone(), self.schema.clone(), self.def_id);

        Ok(Value::Plan(plan))
    }
}

