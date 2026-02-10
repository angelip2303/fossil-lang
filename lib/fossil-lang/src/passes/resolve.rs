//! IR Name Resolution
//!
//! Resolves identifiers in the IR from Ident::Unresolved to Ident::Resolved.
//! Also desugars Pipe expressions.

use std::collections::HashMap;

use std::sync::Arc;

use crate::ast::Loc;
use crate::context::{DefId, DefKind, Symbol, TypeMetadata};
use crate::error::{FossilError, FossilErrors};
use crate::ir::{ExprId, ExprKind, Ident, Ir, Path, StmtId, StmtKind, TypeId, TypeKind};
use crate::passes::GlobalContext;

/// Scope for name resolution
#[derive(Default, Clone)]
struct Scope {
    values: HashMap<Symbol, DefId>,
    types: HashMap<Symbol, DefId>,
}

/// Stack of scopes for nested resolution
struct ScopeStack {
    scopes: Vec<Scope>,
}

impl ScopeStack {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("No scope available")
    }

    fn lookup_value(&self, name: Symbol) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&def_id) = scope.values.get(&name) {
                return Some(def_id);
            }
        }
        None
    }

    fn lookup_type(&self, name: Symbol) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&def_id) = scope.types.get(&name) {
                return Some(def_id);
            }
        }
        None
    }
}

/// IR Name Resolver
pub struct IrResolver {
    gcx: GlobalContext,
    ir: Ir,
    scopes: ScopeStack,
    /// Type metadata extracted from AST, keyed by type name
    pending_metadata: HashMap<Symbol, TypeMetadata>,
}

impl IrResolver {
    pub fn new(ir: Ir, gcx: GlobalContext) -> Self {
        Self {
            gcx,
            ir,
            scopes: ScopeStack::new(),
            pending_metadata: HashMap::new(),
        }
    }

    /// Add type metadata to be transferred during resolution
    pub fn with_type_metadata(mut self, metadata: HashMap<Symbol, TypeMetadata>) -> Self {
        self.pending_metadata = metadata;
        self
    }

    pub fn resolve(mut self) -> Result<(Ir, GlobalContext), FossilErrors> {
        let mut errors = FossilErrors::new();

        // Phase 1: Collect declarations
        self.collect_declarations(&mut errors);

        // Return early if there were declaration errors
        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 2: Resolve all statements
        let root = self.ir.root.clone();
        for stmt_id in root {
            self.resolve_stmt(stmt_id, &mut errors);
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok((self.ir, self.gcx))
    }

    fn collect_declarations(&mut self, errors: &mut FossilErrors) {
        let root = self.ir.root.clone();
        for stmt_id in root {
            let stmt = self.ir.stmts.get(stmt_id);
            let loc = stmt.loc;

            match &stmt.kind.clone() {
                StmtKind::Let { name, value, .. } => {
                    // Check if it's a function (forward declare)
                    let expr = self.ir.exprs.get(*value);
                    if matches!(expr.kind, ExprKind::Function { .. }) {
                        if self.scopes.current_mut().values.contains_key(name) {
                            let name_str = self.gcx.interner.resolve(*name).to_string();
                            errors.push(FossilError::already_defined(name_str, loc, loc));
                        } else {
                            let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                            self.scopes.current_mut().values.insert(*name, def_id);
                        }
                    }
                }

                StmtKind::Const { name, .. } => {
                    if self.scopes.current_mut().values.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Const);
                        self.scopes.current_mut().values.insert(*name, def_id);
                    }
                }

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Type);
                        self.scopes.current_mut().types.insert(*name, def_id);

                        // Transfer type metadata from pending to final storage
                        if let Some(mut metadata) = self.pending_metadata.remove(name) {
                            metadata.def_id = def_id;
                            self.gcx.type_metadata.insert(def_id, Arc::new(metadata));
                        }
                    }
                }

                StmtKind::Expr(_) => {}
            }
        }
    }

    fn resolve_stmt(&mut self, stmt_id: StmtId, errors: &mut FossilErrors) {
        let stmt = self.ir.stmts.get(stmt_id).clone();

        match &stmt.kind {
            StmtKind::Let {
                name, value, ..
            } => {
                // Resolve value
                self.resolve_expr(*value, errors);

                // Get or create DefId
                let def_id = self.scopes.lookup_value(*name).unwrap_or_else(|| {
                    let def_id = self.gcx.definitions.insert(None, *name, DefKind::Let);
                    self.scopes.current_mut().values.insert(*name, def_id);
                    def_id
                });

                // Update statement with DefId
                let stmt_mut = self.ir.stmts.get_mut(stmt_id);
                if let StmtKind::Let { def_id: d, .. } = &mut stmt_mut.kind {
                    *d = Some(def_id);
                }
            }

            StmtKind::Const { name, value, .. } => {
                self.resolve_expr(*value, errors);

                let def_id = self
                    .scopes
                    .lookup_value(*name)
                    .expect("Const should be declared");

                let stmt_mut = self.ir.stmts.get_mut(stmt_id);
                if let StmtKind::Const { def_id: d, .. } = &mut stmt_mut.kind {
                    *d = Some(def_id);
                }
            }

            StmtKind::Type { name, ty, .. } => {
                self.resolve_type(*ty, errors);

                // Generate constructor for record types
                // Look up the type's DefId to use as the constructor's parent
                let type_def_id = self.scopes.lookup_type(*name);
                self.generate_record_constructor(*name, *ty, type_def_id);
            }

            StmtKind::Expr(expr_id) => {
                self.resolve_expr(*expr_id, errors);
            }
        }
    }

    fn resolve_expr(&mut self, expr_id: ExprId, errors: &mut FossilErrors) {
        let expr = self.ir.exprs.get(expr_id).clone();

        match &expr.kind {
            ExprKind::Identifier(ident) => {
                if let Ident::Unresolved(path) = ident
                    && let Some(def_id) = self.resolve_value_path(path, expr.loc, errors)
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    expr_mut.kind = ExprKind::Identifier(Ident::Resolved(def_id));
                }
            }

            ExprKind::List(items) => {
                for &item in items {
                    self.resolve_expr(item, errors);
                }
            }

            ExprKind::NamedRecordConstruction { type_ident, fields } => {
                // Resolve the type identifier
                if let Ident::Unresolved(path) = type_ident
                    && let Some(def_id) = self.resolve_type_path(path, expr.loc, errors)
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::NamedRecordConstruction { type_ident: ti, .. } =
                        &mut expr_mut.kind
                    {
                        *ti = Ident::Resolved(def_id);
                    }
                }

                // Resolve field expressions
                for (_, field_expr) in fields {
                    self.resolve_expr(*field_expr, errors);
                }
            }

            ExprKind::Function { params, body, .. } => {
                self.scopes.push();

                // Add parameters to scope and update their DefIds
                let params_clone = params.clone();
                for (i, param) in params_clone.iter().enumerate() {
                    let def_id = self.gcx.definitions.insert(None, param.name, DefKind::Let);
                    self.scopes.current_mut().values.insert(param.name, def_id);

                    // Update the param's def_id in the IR
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::Function { params: ps, .. } = &mut expr_mut.kind {
                        ps[i].def_id = Some(def_id);
                    }

                    // Resolve default
                    if let Some(default) = param.default {
                        self.resolve_expr(default, errors);
                    }
                }

                self.resolve_expr(*body, errors);
                self.scopes.pop();
            }

            ExprKind::Application { callee, args } => {
                self.resolve_expr(*callee, errors);
                for arg in args {
                    self.resolve_expr(arg.value(), errors);
                }
            }

            ExprKind::FieldAccess { expr, .. } => {
                self.resolve_expr(*expr, errors);
            }

            ExprKind::Block { stmts } => {
                self.scopes.push();
                for &stmt_id in stmts {
                    self.resolve_stmt(stmt_id, errors);
                }
                self.scopes.pop();
            }

            ExprKind::StringInterpolation { exprs, .. } => {
                for &expr in exprs {
                    self.resolve_expr(expr, errors);
                }
            }

            ExprKind::Unit | ExprKind::Literal(_) => {}
        }
    }

    fn resolve_type(&mut self, type_id: TypeId, errors: &mut FossilErrors) {
        let ty = self.ir.types.get(type_id).clone();

        match &ty.kind {
            TypeKind::Named(ident) => {
                if let Ident::Unresolved(path) = ident
                    && let Some(def_id) = self.resolve_type_path(path, ty.loc, errors)
                {
                    let ty_mut = self.ir.types.get_mut(type_id);
                    ty_mut.kind = TypeKind::Named(Ident::Resolved(def_id));
                }
            }

            TypeKind::Function(params, ret) => {
                for &param in params {
                    self.resolve_type(param, errors);
                }
                self.resolve_type(*ret, errors);
            }

            TypeKind::List(inner) => {
                self.resolve_type(*inner, errors);
            }

            TypeKind::Optional(inner) => {
                self.resolve_type(*inner, errors);
            }

            TypeKind::Record(fields) => {
                self.resolve_record_fields(fields, errors);
            }

            TypeKind::App { ctor, args } => {
                // Resolve constructor
                if let Ident::Unresolved(path) = ctor
                    && let Some(def_id) = self.resolve_type_path(path, ty.loc, errors)
                {
                    let ty_mut = self.ir.types.get_mut(type_id);
                    if let TypeKind::App { ctor: c, .. } = &mut ty_mut.kind {
                        *c = Ident::Resolved(def_id);
                    }
                }

                for &arg in args {
                    self.resolve_type(arg, errors);
                }
            }

            TypeKind::Provider { args, .. } => {
                // Provider types are handled during expansion
                // Just make sure they don't have unresolved references
                for _arg in args {
                    // Provider args are literals, no resolution needed
                }
            }

            TypeKind::Unit | TypeKind::Primitive(_) | TypeKind::Var(_) => {}
        }
    }

    fn resolve_record_fields(
        &mut self,
        fields: &crate::ir::RecordFields,
        errors: &mut FossilErrors,
    ) {
        for (_, ty) in &fields.fields {
            self.resolve_type(*ty, errors);
        }
    }

    fn resolve_value_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_value(*name) {
                    return Some(def_id);
                }

                if let Some(def_id) = self
                    .gcx
                    .definitions
                    .resolve(&crate::ast::ast::Path::Simple(*name))
                {
                    return Some(def_id);
                }

                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(FossilError::undefined_variable(name_str, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = format!("{:?}", ast_path);
                    errors.push(FossilError::undefined_path(path_str, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(FossilError::internal(
                    "resolve",
                    "Relative paths not supported in IR resolver",
                    loc,
                ));
                None
            }
        }
    }

    fn resolve_type_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = self.scopes.lookup_type(*name) {
                    return Some(def_id);
                }

                if let Some(def_id) = self
                    .gcx
                    .definitions
                    .resolve(&crate::ast::ast::Path::Simple(*name))
                {
                    return Some(def_id);
                }

                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(FossilError::undefined_type(name_str, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = crate::ast::ast::Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = format!("{:?}", ast_path);
                    errors.push(FossilError::undefined_type(path_str, loc));
                    None
                })
            }

            Path::Relative { .. } => {
                errors.push(FossilError::internal(
                    "resolve",
                    "Relative paths not supported in IR resolver",
                    loc,
                ));
                None
            }
        }
    }

    fn generate_record_constructor(
        &mut self,
        type_name: Symbol,
        type_id: TypeId,
        type_def_id: Option<DefId>,
    ) {
        let ty = self.ir.types.get(type_id);
        if let TypeKind::Record(_) = &ty.kind {
            // Check if constructor name already exists
            if self.scopes.current_mut().values.contains_key(&type_name) {
                return;
            }

            // Register constructor with type's DefId as parent (for metadata lookup)
            let ctor_def_id =
                self.gcx
                    .definitions
                    .insert(type_def_id, type_name, DefKind::RecordConstructor);
            self.scopes
                .current_mut()
                .values
                .insert(type_name, ctor_def_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::extract_type_metadata;
    use crate::error::FossilError;
    use crate::ir::{ExprKind, Ident, StmtKind};
    use crate::passes::convert::ast_to_ir;
    use crate::passes::expand::ProviderExpander;
    use crate::passes::parse::Parser;

    fn resolve_ok(src: &str) -> (Ir, GlobalContext) {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = extract_type_metadata(&expand_result.ast);
        let ir = ast_to_ir(expand_result.ast);
        IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()
            .expect("resolve failed")
    }

    fn resolve_err(src: &str) -> crate::error::FossilErrors {
        let parsed = Parser::parse(src, 0).expect("parse failed");
        let expand_result = ProviderExpander::new((parsed.ast, parsed.gcx))
            .expand()
            .expect("expand failed");
        let ty = extract_type_metadata(&expand_result.ast);
        let ir = ast_to_ir(expand_result.ast);
        match IrResolver::new(ir, expand_result.gcx)
            .with_type_metadata(ty)
            .resolve()
        {
            Err(errors) => errors,
            Ok(_) => panic!("expected resolve error, but resolution succeeded"),
        }
    }

    // ── Success tests ────────────────────────────────────────────

    #[test]
    fn let_gets_def_id() {
        let (ir, _gcx) = resolve_ok("let x = 42");
        let stmt = ir.stmts.get(ir.root[0]);
        match &stmt.kind {
            StmtKind::Let { def_id, .. } => {
                assert!(def_id.is_some(), "let binding should have a DefId assigned");
            }
            other => panic!("expected Let statement, got {:?}", other),
        }
    }

    #[test]
    fn type_gets_def_id() {
        let (_ir, gcx) = resolve_ok("type T = { Name: string }");
        let name_sym = gcx.interner.lookup("T").expect("T should be interned");
        let def = gcx
            .definitions
            .get_by_symbol(name_sym)
            .expect("T should have a definition");
        assert!(matches!(def.kind, DefKind::Type));
    }

    #[test]
    fn variable_reference_resolves() {
        let (ir, _gcx) = resolve_ok("let x = 42\nlet y = x");
        let y_stmt = ir.stmts.get(ir.root[1]);
        let value_id = match &y_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let statement, got {:?}", other),
        };
        let value_expr = ir.exprs.get(value_id);
        assert!(
            matches!(&value_expr.kind, ExprKind::Identifier(Ident::Resolved(_))),
            "expected Ident::Resolved, got {:?}",
            value_expr.kind
        );
    }

    #[test]
    fn record_constructor_generated() {
        let src = "type T = { Name: string }\nT { Name = \"hi\" }";
        let (ir, _gcx) = resolve_ok(src);
        let expr_stmt = ir.stmts.get(ir.root[1]);
        let expr_id = match &expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        let expr = ir.exprs.get(expr_id);
        match &expr.kind {
            ExprKind::NamedRecordConstruction { type_ident, .. } => {
                assert!(
                    matches!(type_ident, Ident::Resolved(_)),
                    "expected Ident::Resolved for record type, got {:?}",
                    type_ident
                );
            }
            other => panic!("expected NamedRecordConstruction, got {:?}", other),
        }
    }

    #[test]
    fn function_param_gets_def_id() {
        let (ir, _gcx) = resolve_ok("let f = fn(x) -> x");
        let f_stmt = ir.stmts.get(ir.root[0]);
        let value_id = match &f_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let statement, got {:?}", other),
        };
        let func_expr = ir.exprs.get(value_id);
        match &func_expr.kind {
            ExprKind::Function { params, .. } => {
                assert!(!params.is_empty(), "function should have parameters");
                assert!(
                    params[0].def_id.is_some(),
                    "function parameter should have a DefId assigned"
                );
            }
            other => panic!("expected Function, got {:?}", other),
        }
    }

    #[test]
    fn forward_reference_function() {
        let src = "let f = fn(x) -> g(x)\nlet g = fn(x) -> x";
        let (ir, _gcx) = resolve_ok(src);
        let f_stmt = ir.stmts.get(ir.root[0]);
        let f_value = match &f_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let, got {:?}", other),
        };
        let func = ir.exprs.get(f_value);
        let body_id = match &func.kind {
            ExprKind::Function { body, .. } => *body,
            other => panic!("expected Function, got {:?}", other),
        };
        let body = ir.exprs.get(body_id);
        match &body.kind {
            ExprKind::Application { callee, .. } => {
                let callee_expr = ir.exprs.get(*callee);
                assert!(
                    matches!(&callee_expr.kind, ExprKind::Identifier(Ident::Resolved(_))),
                    "forward-referenced function g should be resolved, got {:?}",
                    callee_expr.kind
                );
            }
            other => panic!("expected Application, got {:?}", other),
        }
    }

    #[test]
    fn block_accesses_outer_scope() {
        // A block should be able to reference variables defined in the outer scope
        let src = "let x = 1\nlet y = { x }";
        let (ir, _gcx) = resolve_ok(src);

        let y_stmt = ir.stmts.get(ir.root[1]);
        let y_value = match &y_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let, got {:?}", other),
        };
        let block_expr = ir.exprs.get(y_value);
        let block_stmts = match &block_expr.kind {
            ExprKind::Block { stmts } => stmts.clone(),
            other => panic!("expected Block, got {:?}", other),
        };

        // The expression `x` inside the block should resolve
        let inner_expr_stmt = ir.stmts.get(block_stmts[0]);
        let inner_expr_id = match &inner_expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        let inner_expr = ir.exprs.get(inner_expr_id);
        assert!(
            matches!(&inner_expr.kind, ExprKind::Identifier(Ident::Resolved(_))),
            "block should resolve reference to outer x, got {:?}",
            inner_expr.kind
        );
    }

    #[test]
    fn multiple_let_bindings_resolve() {
        let src = "let a = 1\nlet b = 2\nlet c = a";
        let (ir, _gcx) = resolve_ok(src);
        let c_stmt = ir.stmts.get(ir.root[2]);
        let c_value = match &c_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let, got {:?}", other),
        };
        let c_expr = ir.exprs.get(c_value);
        assert!(
            matches!(&c_expr.kind, ExprKind::Identifier(Ident::Resolved(_))),
            "c should resolve reference to a, got {:?}",
            c_expr.kind
        );
    }

    #[test]
    fn list_elements_resolve() {
        let src = "let x = 1\nlet y = [x, x]";
        let (ir, _gcx) = resolve_ok(src);
        let y_stmt = ir.stmts.get(ir.root[1]);
        let y_value = match &y_stmt.kind {
            StmtKind::Let { value, .. } => *value,
            other => panic!("expected Let, got {:?}", other),
        };
        let list_expr = ir.exprs.get(y_value);
        match &list_expr.kind {
            ExprKind::List(items) => {
                for &item_id in items {
                    let item = ir.exprs.get(item_id);
                    assert!(
                        matches!(&item.kind, ExprKind::Identifier(Ident::Resolved(_))),
                        "list element should resolve, got {:?}",
                        item.kind
                    );
                }
            }
            other => panic!("expected List, got {:?}", other),
        }
    }

    // ── Error tests ──────────────────────────────────────────────

    #[test]
    fn undefined_variable() {
        let errors = resolve_err("x");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::UndefinedVariable { name, .. } if name == "x"),
            "expected UndefinedVariable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type() {
        let errors = resolve_err("type T = { Name: Foo }");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::UndefinedType { path, .. } if path == "Foo"),
            "expected UndefinedType for Foo, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_path() {
        let src = "let y = A.b";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(
                &errors.0[0],
                FossilError::UndefinedPath { .. } | FossilError::UndefinedVariable { .. }
            ),
            "expected UndefinedPath or UndefinedVariable for A.b, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn duplicate_type_def() {
        let src = "type T = { }\ntype T = { }";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::AlreadyDefined { name, .. } if name == "T"),
            "expected AlreadyDefined for T, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn duplicate_let_function() {
        let src = "let f = fn(x) -> x\nlet f = fn(y) -> y";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::AlreadyDefined { name, .. } if name == "f"),
            "expected AlreadyDefined for f, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn forward_reference_non_function() {
        let src = "let y = x\nlet x = 42";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::UndefinedVariable { name, .. } if name == "x"),
            "expected UndefinedVariable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type_in_record_construction() {
        let src = "Foo { Name = \"hi\" }";
        let errors = resolve_err(src);
        assert!(!errors.is_empty(), "should have at least one error");
        let has_type_error = errors.0.iter().any(|e| {
            matches!(
                e,
                FossilError::UndefinedType { .. } | FossilError::UndefinedVariable { .. }
            )
        });
        assert!(
            has_type_error,
            "expected UndefinedType or UndefinedVariable for Foo, got {:?}",
            errors.0
        );
    }

    #[test]
    fn undefined_variable_in_block() {
        let src = "let y = { x }";
        let errors = resolve_err(src);
        assert!(!errors.is_empty(), "should have at least one error");
        assert!(
            matches!(&errors.0[0], FossilError::UndefinedVariable { name, .. } if name == "x"),
            "expected UndefinedVariable for x inside block, got {:?}",
            errors.0[0]
        );
    }
}
