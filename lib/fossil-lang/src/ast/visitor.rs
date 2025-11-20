use crate::ast::*;
use crate::context::Interner;

pub trait Visitor: Sized {
    type Error;

    fn visit_decl(
        &mut self,
        decl_id: DeclId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        walk_decl(self, decl_id, ast, interner)
    }

    fn visit_expr(
        &mut self,
        expr_id: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        walk_expr(self, expr_id, ast, interner)
    }

    fn visit_type(
        &mut self,
        type_id: TypeId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        walk_type(self, type_id, ast, interner)
    }

    fn visit_import_decl(
        &mut self,
        _decl_id: DeclId,
        _module: &Path,
        _alias: Symbol,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_let_decl(
        &mut self,
        _decl_id: DeclId,
        _name: Symbol,
        value: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_expr(value, ast, interner)
    }

    fn visit_type_decl(
        &mut self,
        _decl_id: DeclId,
        _name: Symbol,
        ty: TypeId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_type(ty, ast, interner)
    }

    fn visit_expr_decl(
        &mut self,
        _decl_id: DeclId,
        expr: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_expr(expr, ast, interner)
    }

    fn visit_identifier(
        &mut self,
        _expr_id: ExprId,
        _path: &Path,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_function(
        &mut self,
        _expr_id: ExprId,
        _params: &[Symbol],
        body: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_expr(body, ast, interner)
    }

    fn visit_application(
        &mut self,
        _expr_id: ExprId,
        callee: ExprId,
        args: &[ExprId],
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_expr(callee, ast, interner)?;
        for arg in args {
            self.visit_expr(*arg, ast, interner)?;
        }
        Ok(())
    }

    fn visit_pipe(
        &mut self,
        _expr_id: ExprId,
        lhs: ExprId,
        rhs: ExprId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_expr(lhs, ast, interner)?;
        self.visit_expr(rhs, ast, interner)
    }

    fn visit_list(
        &mut self,
        _expr_id: ExprId,
        items: &[ExprId],
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        for item in items {
            self.visit_expr(*item, ast, interner)?;
        }
        Ok(())
    }

    fn visit_record(
        &mut self,
        _expr_id: ExprId,
        fields: &[(Symbol, ExprId)],
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        for (_, expr) in fields {
            self.visit_expr(*expr, ast, interner)?;
        }
        Ok(())
    }

    fn visit_literal(
        &mut self,
        _expr_id: ExprId,
        _literal: &Literal,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_unit(
        &mut self,
        _expr_id: ExprId,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_type_named(
        &mut self,
        _type_id: TypeId,
        _path: &Path,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_type_primitive(
        &mut self,
        _type_id: TypeId,
        _prim: PrimitiveType,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_type_provider(
        &mut self,
        _type_id: TypeId,
        _provider: &Path,
        _args: &[Literal],
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_type_function(
        &mut self,
        _type_id: TypeId,
        params: &[TypeId],
        ret: TypeId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        for param in params {
            self.visit_type(*param, ast, interner)?;
        }
        self.visit_type(ret, ast, interner)
    }

    fn visit_type_list(
        &mut self,
        _type_id: TypeId,
        inner: TypeId,
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        self.visit_type(inner, ast, interner)
    }

    fn visit_type_record(
        &mut self,
        _type_id: TypeId,
        fields: &[(Symbol, TypeId)],
        ast: &Ast,
        interner: &Interner,
    ) -> Result<(), Self::Error> {
        for (_, ty) in fields {
            self.visit_type(*ty, ast, interner)?;
        }
        Ok(())
    }

    fn visit_type_var(
        &mut self,
        _type_id: TypeId,
        _var: TypeVar,
        _ast: &Ast,
        _interner: &Interner,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub fn walk_ast<V: Visitor>(
    visitor: &mut V,
    ast: &Ast,
    interner: &Interner,
) -> Result<(), V::Error> {
    for (decl_id, _) in ast.decls.iter() {
        visitor.visit_decl(decl_id, ast, interner)?;
    }
    Ok(())
}

pub fn walk_decl<V: Visitor>(
    visitor: &mut V,
    decl_id: DeclId,
    ast: &Ast,
    interner: &Interner,
) -> Result<(), V::Error> {
    match ast.decls.get(decl_id) {
        Decl::Import { module, alias } => {
            visitor.visit_import_decl(decl_id, module, *alias, ast, interner)
        }

        Decl::Let { name, value } => visitor.visit_let_decl(decl_id, *name, *value, ast, interner),

        Decl::Type { name, ty } => visitor.visit_type_decl(decl_id, *name, *ty, ast, interner),

        Decl::Expr(expr) => visitor.visit_expr_decl(decl_id, *expr, ast, interner),
    }
}

pub fn walk_expr<V: Visitor>(
    visitor: &mut V,
    expr_id: ExprId,
    ast: &Ast,
    interner: &Interner,
) -> Result<(), V::Error> {
    match ast.exprs.get(expr_id) {
        Expr::Identifier(path) => visitor.visit_identifier(expr_id, path, ast, interner),

        Expr::Function { params, body } => {
            visitor.visit_function(expr_id, params, *body, ast, interner)
        }

        Expr::Application { callee, args } => {
            visitor.visit_application(expr_id, *callee, args, ast, interner)
        }

        Expr::Pipe { lhs, rhs } => visitor.visit_pipe(expr_id, *lhs, *rhs, ast, interner),

        Expr::List(items) => visitor.visit_list(expr_id, items, ast, interner),

        Expr::Record(fields) => visitor.visit_record(expr_id, fields, ast, interner),

        Expr::Literal(lit) => visitor.visit_literal(expr_id, lit, ast, interner),

        Expr::Unit => visitor.visit_unit(expr_id, ast, interner),
    }
}

pub fn walk_type<V: Visitor>(
    visitor: &mut V,
    type_id: TypeId,
    ast: &Ast,
    interner: &Interner,
) -> Result<(), V::Error> {
    match ast.types.get(type_id) {
        Type::Named(path) => visitor.visit_type_named(type_id, path, ast, interner),

        Type::Primitive(prim) => visitor.visit_type_primitive(type_id, *prim, ast, interner),

        Type::Provider { provider, args } => {
            visitor.visit_type_provider(type_id, provider, args, ast, interner)
        }

        Type::Function(params, ret) => {
            visitor.visit_type_function(type_id, params, *ret, ast, interner)
        }

        Type::List(inner) => visitor.visit_type_list(type_id, *inner, ast, interner),

        Type::Record(fields) => visitor.visit_type_record(type_id, fields, ast, interner),

        Type::Var(var) => visitor.visit_type_var(type_id, *var, ast, interner),
    }
}
