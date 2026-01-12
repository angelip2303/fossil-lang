//! Generic Folder pattern for AST transformations
//!
//! This module provides a generic trait-based approach for transforming between
//! different AST levels (ast::Ast → hir::Hir → thir::TypedHir).
//!
//! The Folder pattern is preferred over Visitor for fossil-lang because each
//! compilation pass produces a new AST structure rather than analyzing in-place.

/// Generic folder trait for transforming between AST levels
///
/// Implementations transform expressions, types, and statements from a source
/// AST level to a target AST level, handling differences like:
/// - Name resolution (Path → DefId)
/// - Desugaring (Pipe → Application)
/// - Type annotation (adding type fields to expressions)
///
/// # Type Parameters
/// - `From`: Source AST level (e.g., `ast::Ast`)
/// - `To`: Target AST level (e.g., `hir::Hir`)
pub trait Folder {
    type From: AstLevel;
    type To: AstLevel;
    type Error;

    /// Fold an expression from source to target
    fn fold_expr(
        &mut self,
        expr_id: <Self::From as AstLevel>::ExprId,
    ) -> Result<<Self::To as AstLevel>::ExprId, Self::Error>;

    /// Fold a type from source to target
    fn fold_type(
        &mut self,
        type_id: <Self::From as AstLevel>::TypeId,
    ) -> Result<<Self::To as AstLevel>::TypeId, Self::Error>;

    /// Fold a statement from source to target
    fn fold_stmt(
        &mut self,
        stmt_id: <Self::From as AstLevel>::StmtId,
    ) -> Result<<Self::To as AstLevel>::StmtId, Self::Error>;
}

/// Trait for AST levels to expose their components
///
/// Each AST level (ast::Ast, hir::Hir, thir::TypedHir) implements this trait
/// to provide uniform access to their arena-based storage.
pub trait AstLevel {
    type ExprId: Copy;
    type TypeId: Copy;
    type StmtId: Copy;
    type Expr;
    type Type;
    type Stmt;

    fn get_expr(&self, id: Self::ExprId) -> &Self::Expr;
    fn get_type(&self, id: Self::TypeId) -> &Self::Type;
    fn get_stmt(&self, id: Self::StmtId) -> &Self::Stmt;
    fn alloc_expr(&mut self, expr: Self::Expr) -> Self::ExprId;
    fn alloc_type(&mut self, ty: Self::Type) -> Self::TypeId;
    fn alloc_stmt(&mut self, stmt: Self::Stmt) -> Self::StmtId;
}

// Implementations for each AST level

// Note: We don't implement AstLevel for ast::Ast because statements are now
// owned (Vec<Stmt>) rather than referenced (Vec<StmtId>). The Folder pattern
// only applies to HIR → THIR transformation.

impl AstLevel for super::hir::Hir {
    type ExprId = super::hir::ExprId;
    type TypeId = super::hir::TypeId;
    type StmtId = super::hir::StmtId;
    type Expr = super::hir::Expr;
    type Type = super::hir::Type;
    type Stmt = super::hir::Stmt;

    fn get_expr(&self, id: Self::ExprId) -> &Self::Expr {
        self.exprs.get(id)
    }

    fn get_type(&self, id: Self::TypeId) -> &Self::Type {
        self.types.get(id)
    }

    fn get_stmt(&self, id: Self::StmtId) -> &Self::Stmt {
        self.stmts.get(id)
    }

    fn alloc_expr(&mut self, expr: Self::Expr) -> Self::ExprId {
        self.exprs.alloc(expr)
    }

    fn alloc_type(&mut self, ty: Self::Type) -> Self::TypeId {
        self.types.alloc(ty)
    }

    fn alloc_stmt(&mut self, stmt: Self::Stmt) -> Self::StmtId {
        self.stmts.alloc(stmt)
    }
}

impl AstLevel for super::thir::TypedHir {
    type ExprId = super::thir::ExprId;
    type TypeId = super::thir::TypeId;
    type StmtId = super::thir::StmtId;
    type Expr = super::thir::Expr;
    type Type = super::thir::Type;
    type Stmt = super::thir::Stmt;

    fn get_expr(&self, id: Self::ExprId) -> &Self::Expr {
        self.exprs.get(id)
    }

    fn get_type(&self, id: Self::TypeId) -> &Self::Type {
        self.types.get(id)
    }

    fn get_stmt(&self, id: Self::StmtId) -> &Self::Stmt {
        self.stmts.get(id)
    }

    fn alloc_expr(&mut self, expr: Self::Expr) -> Self::ExprId {
        self.exprs.alloc(expr)
    }

    fn alloc_type(&mut self, ty: Self::Type) -> Self::TypeId {
        self.types.alloc(ty)
    }

    fn alloc_stmt(&mut self, stmt: Self::Stmt) -> Self::StmtId {
        self.stmts.alloc(stmt)
    }
}
