//! ItemTree: per-file structural layout of top-level items.
//!
//! Mirror of rust-analyzer's `hir-def/item_tree.rs` pattern:
//! provides an **invalidation barrier** for incremental compilation.
//! When editing inside a body, the ItemTree of the file is unchanged,
//! so downstream queries keyed by ItemLoc remain cached.
//!
//! Today the ItemTree is built alongside the existing lowering pass.
//! A future refactor can route per-item queries (let_body, typeck_let,
//! rq_of_let) through ItemLoc-interned IDs for full per-item granularity
//! (see plan: smooth-strolling-mccarthy.md issues #15-#17).

use crate::ast::{Ast, StmtKind};
use crate::db::{Db, SourceFile, Symbol};

/// Per-file item tree. Keyed by `SourceFile` via the Salsa query `file_item_tree`.
/// Items are stored as local entries; `ItemLoc<T>` uses `(SourceFile, usize)`
/// indices that are stable across body edits (body text changes don't
/// reorder the top-level items).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemTree {
    pub lets: Vec<LetItem>,
    pub types: Vec<TypeItem>,
    /// There is at most one pipeline (top-level expression) per file.
    pub pipeline: Option<PipelineItem>,
    /// Source order of all top-level items (for preserving dependency order
    /// when aggregating per-item RQ outputs into a file-level plan).
    pub order: Vec<ItemRef>,
}

/// Reference into the ItemTree arena for a single top-level item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemRef {
    Let(usize),
    Type(usize),
    Pipeline,
}

/// A top-level `let name = value` binding.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetItem {
    pub name: Symbol,
    /// Index into the source AST. Stable across body edits that don't
    /// reorder top-level items.
    pub ast_stmt_index: usize,
}

/// A top-level `type Name(...) do ... end` declaration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeItem {
    pub name: Symbol,
    pub ast_stmt_index: usize,
}

/// The single top-level expression statement (the pipeline).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PipelineItem {
    pub ast_stmt_index: usize,
}

/// Stable location of a `let` item: file + index in the ItemTree arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LetLoc {
    pub file: SourceFile,
    pub idx: usize,
}

/// Stable location of a type declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeDeclLoc {
    pub file: SourceFile,
    pub idx: usize,
}

/// Stable location of the pipeline (at most one per file).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PipelineLoc {
    pub file: SourceFile,
}

impl ItemTree {
    /// Build an ItemTree from a parsed AST.
    ///
    /// This is a lightweight pass over the top-level statements that
    /// produces a stable arena for each item kind.
    pub fn from_ast(ast: &Ast) -> Self {
        let mut tree = ItemTree::default();
        for (stmt_index, &stmt_id) in ast.root.iter().enumerate() {
            let stmt = &ast.stmts[stmt_id];
            match &stmt.kind {
                StmtKind::Let { name, .. } => {
                    let idx = tree.lets.len();
                    tree.lets.push(LetItem {
                        name: *name,
                        ast_stmt_index: stmt_index,
                    });
                    tree.order.push(ItemRef::Let(idx));
                }
                StmtKind::Type { name, .. } => {
                    let idx = tree.types.len();
                    tree.types.push(TypeItem {
                        name: *name,
                        ast_stmt_index: stmt_index,
                    });
                    tree.order.push(ItemRef::Type(idx));
                }
                StmtKind::Expr(_) => {
                    // Only the first expression statement is treated as the pipeline.
                    if tree.pipeline.is_none() {
                        tree.pipeline = Some(PipelineItem {
                            ast_stmt_index: stmt_index,
                        });
                        tree.order.push(ItemRef::Pipeline);
                    }
                }
            }
        }
        tree
    }

    pub fn find_let(&self, name: Symbol) -> Option<&LetItem> {
        self.lets.iter().find(|l| l.name == name)
    }

    pub fn find_type(&self, name: Symbol) -> Option<&TypeItem> {
        self.types.iter().find(|t| t.name == name)
    }

    pub fn let_count(&self) -> usize {
        self.lets.len()
    }

    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    pub fn has_pipeline(&self) -> bool {
        self.pipeline.is_some()
    }
}

/// Salsa-tracked query: `file_item_tree(file) -> ItemTree`.
///
/// **Invalidation barrier**: body-only edits (text changes inside a let value
/// or a type body) don't modify the ItemTree, so this query returns the same
/// cached value and downstream queries keyed on ItemLoc stay cached.
#[salsa::tracked]
pub fn file_item_tree(db: &dyn Db, file: SourceFile) -> ItemTree {
    let ast = crate::queries::parse(db, file);
    ItemTree::from_ast(&ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::FossilDb;

    #[test]
    fn empty_file_has_empty_item_tree() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, String::new(), "test".into());
        let tree = file_item_tree(&db, file);
        assert_eq!(tree.let_count(), 0);
        assert_eq!(tree.type_count(), 0);
        assert!(!tree.has_pipeline());
    }

    #[test]
    fn single_let_creates_one_item() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 42".to_string(), "test".into());
        let tree = file_item_tree(&db, file);
        assert_eq!(tree.let_count(), 1);
        assert_eq!(tree.type_count(), 0);
        assert_eq!(tree.order.len(), 1);
        assert!(matches!(tree.order[0], ItemRef::Let(0)));
    }

    #[test]
    fn multiple_lets_preserve_source_order() {
        let db = FossilDb::default();
        let file = SourceFile::new(
            &db,
            "let a = 1\nlet b = 2\nlet c = 3".to_string(),
            "test".into(),
        );
        let tree = file_item_tree(&db, file);
        assert_eq!(tree.let_count(), 3);
        // Order must match source order
        assert!(matches!(tree.order[0], ItemRef::Let(0)));
        assert!(matches!(tree.order[1], ItemRef::Let(1)));
        assert!(matches!(tree.order[2], ItemRef::Let(2)));
    }

    #[test]
    fn item_tree_is_salsa_memoized() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let x = 1".to_string(), "test".into());
        let t1 = file_item_tree(&db, file);
        let t2 = file_item_tree(&db, file);
        assert_eq!(t1, t2);
    }

    #[test]
    fn let_loc_is_stable() {
        let db = FossilDb::default();
        let file = SourceFile::new(&db, "let foo = 1\nlet bar = 2".to_string(), "test".into());
        let tree = file_item_tree(&db, file);

        // Construct a LetLoc for `foo` — should be idx 0
        let foo_loc = LetLoc { file, idx: 0 };
        assert_eq!(tree.lets[foo_loc.idx].name.text(&db), "foo");

        // Construct a LetLoc for `bar` — should be idx 1
        let bar_loc = LetLoc { file, idx: 1 };
        assert_eq!(tree.lets[bar_loc.idx].name.text(&db), "bar");
    }
}
