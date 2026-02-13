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
                StmtKind::Let { .. } => {}

                StmtKind::Type { name, .. } => {
                    if self.scopes.current_mut().types.contains_key(name) {
                        let name_str = self.gcx.interner.resolve(*name).to_string();
                        errors.push(FossilError::already_defined(name_str, loc, loc));
                    } else {
                        let def_id = self.gcx.definitions.insert(None, *name, DefKind::Type);
                        self.scopes.current_mut().types.insert(*name, def_id);

                        // Store def_id on the IR statement
                        let stmt_mut = self.ir.stmts.get_mut(stmt_id);
                        if let StmtKind::Type { def_id: d, .. } = &mut stmt_mut.kind {
                            *d = Some(def_id);
                        }

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
            StmtKind::Let { name, value, .. } => {
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
                if let Ident::Unresolved(path) = ident {
                    match path {
                        Path::Simple(_) => {
                            if let Some(def_id) = self.resolve_value_path(path, expr.loc, errors) {
                                let expr_mut = self.ir.exprs.get_mut(expr_id);
                                expr_mut.kind = ExprKind::Identifier(Ident::Resolved(def_id));
                            }
                        }
                        Path::Qualified(parts) => {
                            let ast_path = Path::Qualified(parts.clone());
                            if let Some(def_id) = self.gcx.definitions.resolve(&ast_path) {
                                let expr_mut = self.ir.exprs.get_mut(expr_id);
                                expr_mut.kind = ExprKind::Identifier(Ident::Resolved(def_id));
                            } else if parts.len() >= 2
                                && self.scopes.lookup_value(parts[0]).is_some()
                            {
                                // Rewrite A.b.c as FieldAccess chain when A is a local binding
                                let base_def_id = self.scopes.lookup_value(parts[0]).unwrap();
                                let base = self.ir.exprs.alloc(crate::ir::Expr {
                                    kind: ExprKind::Identifier(Ident::Resolved(base_def_id)),
                                    ty: crate::ir::TypeRef::Unknown,
                                    loc: expr.loc,
                                });
                                let mut current = base;
                                for &field_sym in &parts[1..] {
                                    current = self.ir.exprs.alloc(crate::ir::Expr {
                                        kind: ExprKind::FieldAccess {
                                            expr: current,
                                            field: field_sym,
                                        },
                                        ty: crate::ir::TypeRef::Unknown,
                                        loc: expr.loc,
                                    });
                                }
                                // Replace original expression with the outermost FieldAccess
                                let final_expr = self.ir.exprs.get(current).clone();
                                let expr_mut = self.ir.exprs.get_mut(expr_id);
                                expr_mut.kind = final_expr.kind;
                            } else {
                                let path_str = ast_path.display(&self.gcx.interner);
                                errors.push(FossilError::undefined_path(path_str, expr.loc));
                            }
                        }
                    }
                }
            }

            ExprKind::RecordInstance { type_ident, ctor_args, fields } => {
                // Resolve the type identifier
                if let Ident::Unresolved(path) = type_ident
                    && let Some(def_id) = self.resolve_type_path(path, expr.loc, errors)
                {
                    let expr_mut = self.ir.exprs.get_mut(expr_id);
                    if let ExprKind::RecordInstance { type_ident: ti, .. } = &mut expr_mut.kind {
                        *ti = Ident::Resolved(def_id);
                    }
                }

                // Resolve constructor arguments
                for arg in ctor_args {
                    self.resolve_expr(arg.value(), errors);
                }

                // Resolve field expressions
                for (_, field_expr) in fields {
                    self.resolve_expr(*field_expr, errors);
                }
            }

            ExprKind::Projection { source, binding, outputs, .. } => {
                // Resolve source first
                self.resolve_expr(*source, errors);

                // Create a new scope for the projection body
                self.scopes.push();
                let def_id = self.gcx.definitions.insert(None, *binding, DefKind::Let);
                self.scopes.current_mut().values.insert(*binding, def_id);

                // Update binding_def in the IR
                let expr_mut = self.ir.exprs.get_mut(expr_id);
                if let ExprKind::Projection { binding_def, .. } = &mut expr_mut.kind {
                    *binding_def = Some(def_id);
                }

                // Resolve output expressions
                let outputs = outputs.clone();
                for output in &outputs {
                    self.resolve_expr(*output, errors);
                }
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

            TypeKind::Optional(inner) => {
                self.resolve_type(*inner, errors);
            }

            TypeKind::Record(fields) => {
                self.resolve_record_fields(fields, errors);
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

    fn resolve_path(
        &self,
        path: &Path,
        loc: Loc,
        scope_lookup: impl Fn(&ScopeStack, Symbol) -> Option<DefId>,
        make_error: impl Fn(String, Loc) -> FossilError,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        match path {
            Path::Simple(name) => {
                if let Some(def_id) = scope_lookup(&self.scopes, *name) {
                    return Some(def_id);
                }

                if let Some(def_id) = self.gcx.definitions.resolve(&Path::Simple(*name)) {
                    return Some(def_id);
                }

                let name_str = self.gcx.interner.resolve(*name).to_string();
                errors.push(make_error(name_str, loc));
                None
            }

            Path::Qualified(parts) => {
                let ast_path = Path::Qualified(parts.clone());
                self.gcx.definitions.resolve(&ast_path).or_else(|| {
                    let path_str = ast_path.display(&self.gcx.interner);
                    errors.push(make_error(path_str, loc));
                    None
                })
            }
        }
    }

    fn resolve_value_path(
        &self,
        path: &Path,
        loc: Loc,
        errors: &mut FossilErrors,
    ) -> Option<DefId> {
        self.resolve_path(path, loc, ScopeStack::lookup_value, FossilError::undefined_variable, errors)
    }

    fn resolve_type_path(&self, path: &Path, loc: Loc, errors: &mut FossilErrors) -> Option<DefId> {
        self.resolve_path(path, loc, ScopeStack::lookup_type, FossilError::undefined_type, errors)
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
        let (_ir, gcx) = resolve_ok("type T do Name: string end");
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
        let src = "type T do Name: string end\nT { Name = \"hi\" }";
        let (ir, _gcx) = resolve_ok(src);
        let expr_stmt = ir.stmts.get(ir.root[1]);
        let expr_id = match &expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        let expr = ir.exprs.get(expr_id);
        match &expr.kind {
            ExprKind::RecordInstance { type_ident, .. } => {
                assert!(
                    matches!(type_ident, Ident::Resolved(_)),
                    "expected Ident::Resolved for record type, got {:?}",
                    type_ident
                );
            }
            other => panic!("expected RecordInstance, got {:?}", other),
        }
    }

    #[test]
    fn projection_param_gets_def_id() {
        let (ir, _gcx) = resolve_ok("let x = 42\nx |> fn row -> row end");
        let expr_stmt = ir.stmts.get(ir.root[1]);
        let expr_id = match &expr_stmt.kind {
            StmtKind::Expr(id) => *id,
            other => panic!("expected Expr statement, got {:?}", other),
        };
        let proj_expr = ir.exprs.get(expr_id);
        match &proj_expr.kind {
            ExprKind::Projection { binding_def, .. } => {
                assert!(
                    binding_def.is_some(),
                    "projection binding should have a DefId assigned"
                );
            }
            other => panic!("expected Projection, got {:?}", other),
        }
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

    // ── Error tests ──────────────────────────────────────────────

    #[test]
    fn undefined_variable() {
        let errors = resolve_err("x");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "variable", name, .. } if name == "x"),
            "expected Undefined variable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type() {
        let errors = resolve_err("type T do Name: Foo end");
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "type", name, .. } if name == "Foo"),
            "expected Undefined type for Foo, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_path() {
        let src = "let y = A.b";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { .. }),
            "expected Undefined for A.b, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn duplicate_type_def() {
        let src = "type T do end\ntype T do end";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::AlreadyDefined { name, .. } if name == "T"),
            "expected AlreadyDefined for T, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn forward_reference_non_function() {
        let src = "let y = x\nlet x = 42";
        let errors = resolve_err(src);
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(&errors.0[0], FossilError::Undefined { kind: "variable", name, .. } if name == "x"),
            "expected Undefined variable for x, got {:?}",
            errors.0[0]
        );
    }

    #[test]
    fn undefined_type_in_record_construction() {
        let src = "Foo { Name = \"hi\" }";
        let errors = resolve_err(src);
        assert!(!errors.is_empty(), "should have at least one error");
        let has_type_error = errors
            .0
            .iter()
            .any(|e| matches!(e, FossilError::Undefined { .. }));
        assert!(
            has_type_error,
            "expected Undefined for Foo, got {:?}",
            errors.0
        );
    }
}
