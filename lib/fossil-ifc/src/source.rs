//! IFC data source with generic entity extraction

use std::path::PathBuf;

use fossil_lang::traits::source::Source;
use polars::prelude::*;

use crate::schema::{IfcEntityDef, IfcFieldDef};
use crate::step::{StepEntity, StepFile};

enum FieldValue {
    Str(Option<String>),
    Float(Option<f64>),
}

fn extract_field_value(entity: &StepEntity, field: &IfcFieldDef) -> FieldValue {
    let token = entity.get(field.position);
    match field.data_type {
        DataType::Float64 => FieldValue::Float(token.and_then(|t| t.as_float())),
        _ => FieldValue::Str(token.and_then(|t| t.as_string())),
    }
}

fn build_dataframe(entities: &[StepEntity], entity_def: &IfcEntityDef) -> PolarsResult<DataFrame> {
    let all_fields = entity_def.all_fields();
    let mut columns: Vec<Vec<FieldValue>> = all_fields.iter().map(|_| Vec::new()).collect();

    for entity in entities {
        for (i, field) in all_fields.iter().enumerate() {
            columns[i].push(extract_field_value(entity, field));
        }
    }

    let polars_columns: Vec<Column> = all_fields
        .iter()
        .zip(columns)
        .map(|(field, values)| match field.data_type {
            DataType::Float64 => {
                let floats: Vec<Option<f64>> = values
                    .into_iter()
                    .map(|v| match v {
                        FieldValue::Float(f) => f,
                        _ => None,
                    })
                    .collect();
                Column::new(field.name.into(), &floats)
            }
            _ => {
                let strings: Vec<Option<String>> = values
                    .into_iter()
                    .map(|v| match v {
                        FieldValue::Str(s) => s,
                        _ => None,
                    })
                    .collect();
                Column::new(field.name.into(), &strings)
            }
        })
        .collect();

    DataFrame::new(polars_columns)
}

#[derive(Debug, Clone)]
pub struct IfcSource {
    path: PathBuf,
    entity_def: &'static IfcEntityDef,
}

impl IfcSource {
    pub fn new(path: PathBuf, entity_def: &'static IfcEntityDef) -> Self {
        Self { path, entity_def }
    }
}

impl Source for IfcSource {
    fn to_lazy_frame(&self) -> PolarsResult<LazyFrame> {
        let step_file = StepFile::open(&self.path).map_err(|e| {
            PolarsError::ComputeError(format!("failed to parse IFC file: {}", e).into())
        })?;

        let entities = step_file.find_all(self.entity_def.step_name);
        let df = build_dataframe(&entities, self.entity_def)?;

        Ok(df.lazy())
    }

    fn box_clone(&self) -> Box<dyn Source> {
        Box::new(self.clone())
    }
}
