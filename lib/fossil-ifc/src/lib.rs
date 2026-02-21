pub mod schema;
pub mod source;
pub(crate) mod step;

use fossil_lang::ast::Loc;
use fossil_lang::context::DefKind;
use fossil_lang::error::FossilError;
use fossil_lang::ir::{Ir, Polytype, TypeVar};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::value::Value;
use fossil_lang::traits::function::{FunctionImpl, RuntimeContext};
use fossil_lang::traits::provider::{
    FieldSpec, FieldType, FunctionDef, ModuleSpec, ProviderArgs, ProviderContext, ProviderInfo,
    ProviderKind, ProviderOutput, ProviderParamInfo, ProviderSchema, TypeProviderImpl,
};
use fossil_lang::traits::source::Source;
use fossil_providers::utils::{resolve_path, validate_extension, validate_path};

use polars::prelude::*;

use schema::{datatype_to_primitive, IfcFieldDef, Optionality, ALL_ENTITIES};
use source::IfcSource;

fn ifc_field_to_field_type(f: &IfcFieldDef) -> FieldType {
    let prim = datatype_to_primitive(&f.data_type);
    match f.optional {
        Optionality::Required => FieldType::Primitive(prim),
        Optionality::Optional => FieldType::Optional(Box::new(FieldType::Primitive(prim))),
    }
}

pub fn init(gcx: &mut GlobalContext) {
    gcx.register_provider("ifc", IfcProvider);
}

pub struct IfcProvider;

impl TypeProviderImpl for IfcProvider {
    fn info(&self) -> ProviderInfo {
        ProviderInfo { extensions: &["ifc"], kind: ProviderKind::Data }
    }

    fn param_info(&self) -> Vec<ProviderParamInfo> {
        vec![
            ProviderParamInfo {
                name: "path",
                required: true,
                default: None,
            },
            ProviderParamInfo {
                name: "entity",
                required: true,
                default: None,
            },
        ]
    }

    fn provide(
        &self,
        args: &ProviderArgs,
        ctx: &mut ProviderContext,
        type_name: &str,
        loc: Loc,
    ) -> Result<ProviderOutput, FossilError> {
        let path_str = args.require_string("path", "ifc", loc)?;
        let entity_str = args.require_string("entity", "ifc", loc)?;

        let path = resolve_path(path_str);
        validate_extension(path.as_ref(), &["ifc"], loc)?;
        validate_path(path.as_ref(), loc)?;

        let entity_def = ALL_ENTITIES
            .iter()
            .find(|e| e.fossil_name == entity_str)
            .ok_or_else(|| {
                FossilError::data_error(format!("unknown IFC entity: {}", entity_str), loc)
            })?;

        let all_fields = entity_def.all_fields();
        let fields: Vec<FieldSpec> = all_fields
            .iter()
            .map(|f| FieldSpec {
                name: ctx.interner.intern(f.name),
                ty: ifc_field_to_field_type(f),
                attrs: vec![],
            })
            .collect();

        let polars_schema = Schema::from_iter(
            all_fields
                .iter()
                .map(|f| Field::new(f.name.into(), f.data_type.clone())),
        );

        let module_spec = ModuleSpec {
            functions: vec![FunctionDef::new("load", IfcLoadFunction {
                path: path.to_str().to_string(),
                entity_def,
                schema: polars_schema,
                type_name: type_name.to_string(),
            })],
        };

        Ok(ProviderOutput::new(ProviderSchema { fields })
            .with_module(module_spec))
    }
}

struct IfcLoadFunction {
    path: String,
    entity_def: &'static IfcEntityDef,
    schema: Schema,
    type_name: String,
}

impl FunctionImpl for IfcLoadFunction {
    fn signature(
        &self,
        ir: &mut Ir,
        next_type_var: &mut dyn FnMut() -> TypeVar,
        gcx: &GlobalContext,
    ) -> Polytype {
        let result_ty = match lookup_type_id(&self.type_name, gcx) {
            Some(type_id) => ir.named_type(type_id),
            None => ir.var_type(next_type_var()),
        };
        Polytype::mono(ir.fn_type(vec![], result_ty))
    }

    fn call(&self, _args: Vec<Value>, ctx: &RuntimeContext) -> Result<Value, FossilError> {
        use fossil_lang::runtime::value::Plan;

        let source = IfcSource::new(
            self.path.clone(),
            self.entity_def,
            ctx.gcx.file_reader.clone(),
        );
        let plan = Plan::from_source(source.box_clone(), self.schema.clone());
        Ok(Value::Plan(plan))
    }
}

fn lookup_type_id(
    name: &str,
    gcx: &GlobalContext,
) -> Option<fossil_lang::context::DefId> {
    gcx.interner.lookup(name).and_then(|sym| {
        gcx.definitions
            .find_by_symbol(sym, |k| matches!(k, DefKind::Type))
            .map(|d| d.id())
    })
}
