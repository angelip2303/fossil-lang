//! Standard element schema for document providers.
//!
//! All document providers (pdf!, docx!, etc.) return a LazyFrame with this schema:
//!
//! | Column       | Type    | Description                                    |
//! |-------------|---------|------------------------------------------------|
//! | element_id   | String  | UUID of the element                            |
//! | element_type | String  | Title, NarrativeText, Table, ListItem, etc.    |
//! | text         | String  | Extracted text content                         |
//! | page_num     | Int?    | Page number (null if not applicable)           |
//! | section      | String? | Heading/section hierarchy                      |
//! | metadata     | String  | JSON with format-specific metadata             |

use fossil_lang::common::PrimitiveType;
use fossil_lang::context::Interner;
use fossil_lang::traits::provider::{FieldSpec, FieldType};

pub const COL_ELEMENT_ID: &str = "element_id";
pub const COL_ELEMENT_TYPE: &str = "element_type";
pub const COL_TEXT: &str = "text";
pub const COL_PAGE_NUM: &str = "page_num";
pub const COL_SECTION: &str = "section";
pub const COL_METADATA: &str = "metadata";

// Element type constants (matching unstructured.io's element model)
pub const ELEMENT_TITLE: &str = "Title";
pub const ELEMENT_NARRATIVE_TEXT: &str = "NarrativeText";
pub const ELEMENT_TABLE: &str = "Table";
pub const ELEMENT_LIST_ITEM: &str = "ListItem";
pub const ELEMENT_IMAGE: &str = "Image";
pub const ELEMENT_HEADER: &str = "Header";
pub const ELEMENT_FOOTER: &str = "Footer";
pub const ELEMENT_PAGE_BREAK: &str = "PageBreak";

/// Build the standard element schema field specs.
pub fn element_field_specs(interner: &mut Interner) -> Vec<FieldSpec> {
    vec![
        FieldSpec {
            name: interner.intern(COL_ELEMENT_ID),
            ty: FieldType::Primitive(PrimitiveType::String),
            attrs: vec![],
        },
        FieldSpec {
            name: interner.intern(COL_ELEMENT_TYPE),
            ty: FieldType::Primitive(PrimitiveType::String),
            attrs: vec![],
        },
        FieldSpec {
            name: interner.intern(COL_TEXT),
            ty: FieldType::Primitive(PrimitiveType::String),
            attrs: vec![],
        },
        FieldSpec {
            name: interner.intern(COL_PAGE_NUM),
            ty: FieldType::Optional(Box::new(FieldType::Primitive(PrimitiveType::Int))),
            attrs: vec![],
        },
        FieldSpec {
            name: interner.intern(COL_SECTION),
            ty: FieldType::Optional(Box::new(FieldType::Primitive(PrimitiveType::String))),
            attrs: vec![],
        },
        FieldSpec {
            name: interner.intern(COL_METADATA),
            ty: FieldType::Primitive(PrimitiveType::String),
            attrs: vec![],
        },
    ]
}
