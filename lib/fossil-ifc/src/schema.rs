use fossil_lang::context::global::BuiltInFieldType;
use fossil_lang::ir::PrimitiveType;
use polars::prelude::DataType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Optionality {
    Required,
    Optional,
}

#[derive(Debug, Clone)]
pub struct IfcFieldDef {
    pub name: &'static str,
    pub position: usize,
    pub data_type: DataType,
    pub optional: Optionality,
}

impl IfcFieldDef {
    const fn required_string(name: &'static str, position: usize) -> Self {
        Self { name, position, data_type: DataType::String, optional: Optionality::Required }
    }

    const fn optional_string(name: &'static str, position: usize) -> Self {
        Self { name, position, data_type: DataType::String, optional: Optionality::Optional }
    }

    const fn optional_float(name: &'static str, position: usize) -> Self {
        Self { name, position, data_type: DataType::Float64, optional: Optionality::Optional }
    }

    pub fn to_builtin_field_type(&self) -> BuiltInFieldType {
        let prim = datatype_to_primitive(&self.data_type);
        match self.optional {
            Optionality::Required => BuiltInFieldType::Required(prim),
            Optionality::Optional => BuiltInFieldType::Optional(prim),
        }
    }
}

fn datatype_to_primitive(dt: &DataType) -> PrimitiveType {
    match dt {
        DataType::String => PrimitiveType::String,
        DataType::Float32 | DataType::Float64 => PrimitiveType::Float,
        DataType::Int8
        | DataType::Int16
        | DataType::Int32
        | DataType::Int64
        | DataType::UInt8
        | DataType::UInt16
        | DataType::UInt32
        | DataType::UInt64 => PrimitiveType::Int,
        DataType::Boolean => PrimitiveType::Bool,
        _ => PrimitiveType::String,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntityKind {
    Element,
    Spatial,
}

#[derive(Debug, Clone)]
pub struct IfcEntityDef {
    pub fossil_name: &'static str,
    pub step_name: &'static str,
    pub kind: EntityKind,
    pub custom_fields: &'static [IfcFieldDef],
}

impl IfcEntityDef {
    const fn element(fossil_name: &'static str, step_name: &'static str, custom_fields: &'static [IfcFieldDef]) -> Self {
        Self { fossil_name, step_name, kind: EntityKind::Element, custom_fields }
    }

    const fn spatial(fossil_name: &'static str, step_name: &'static str, custom_fields: &'static [IfcFieldDef]) -> Self {
        Self { fossil_name, step_name, kind: EntityKind::Spatial, custom_fields }
    }

    pub fn base_fields(&self) -> &'static [IfcFieldDef] {
        match self.kind {
            EntityKind::Element => ELEMENT_BASE_FIELDS,
            EntityKind::Spatial => SPATIAL_BASE_FIELDS,
        }
    }

    pub fn all_fields(&self) -> Vec<IfcFieldDef> {
        let mut fields = self.base_fields().to_vec();
        fields.extend_from_slice(self.custom_fields);
        fields
    }
}

// --- Base fields by entity kind ---

static ELEMENT_BASE_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::required_string("GlobalId", 0),
    IfcFieldDef::optional_string("Name", 2),
    IfcFieldDef::optional_string("Description", 3),
    IfcFieldDef::optional_string("ObjectType", 4),
    IfcFieldDef::optional_string("Tag", 7),
];

static SPATIAL_BASE_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::required_string("GlobalId", 0),
    IfcFieldDef::optional_string("Name", 2),
    IfcFieldDef::optional_string("Description", 3),
    IfcFieldDef::optional_string("ObjectType", 4),
    IfcFieldDef::optional_string("LongName", 7),
];

// --- Custom fields per entity ---

static IFC_WALL_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_string("PredefinedType", 8),
];

static IFC_DOOR_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_float("OverallHeight", 8),
    IfcFieldDef::optional_float("OverallWidth", 9),
    IfcFieldDef::optional_string("PredefinedType", 10),
    IfcFieldDef::optional_string("OperationType", 11),
];

static IFC_WINDOW_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_float("OverallHeight", 8),
    IfcFieldDef::optional_float("OverallWidth", 9),
    IfcFieldDef::optional_string("PredefinedType", 10),
];

static IFC_SLAB_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_string("PredefinedType", 8),
];

static IFC_ROOF_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_string("PredefinedType", 8),
];

static IFC_SPACE_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_string("PredefinedType", 9),
    IfcFieldDef::optional_float("ElevationWithFlooring", 10),
];

static IFC_BUILDING_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_float("ElevationOfRefHeight", 9),
    IfcFieldDef::optional_float("ElevationOfTerrain", 10),
];

static IFC_STOREY_FIELDS: &[IfcFieldDef] = &[
    IfcFieldDef::optional_float("Elevation", 9),
];

// --- Entity registry ---

pub static ALL_ENTITIES: &[IfcEntityDef] = &[
    IfcEntityDef::element("IfcWall",     "IFCWALL",           IFC_WALL_FIELDS),
    IfcEntityDef::element("IfcDoor",     "IFCDOOR",           IFC_DOOR_FIELDS),
    IfcEntityDef::element("IfcWindow",   "IFCWINDOW",         IFC_WINDOW_FIELDS),
    IfcEntityDef::element("IfcSlab",     "IFCSLAB",           IFC_SLAB_FIELDS),
    IfcEntityDef::element("IfcRoof",     "IFCROOF",           IFC_ROOF_FIELDS),
    IfcEntityDef::spatial("IfcSpace",    "IFCSPACE",          IFC_SPACE_FIELDS),
    IfcEntityDef::spatial("IfcBuilding", "IFCBUILDING",       IFC_BUILDING_FIELDS),
    IfcEntityDef::spatial("IfcStorey",   "IFCBUILDINGSTOREY", IFC_STOREY_FIELDS),
];
