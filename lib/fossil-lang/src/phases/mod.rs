use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::*;
use crate::context::Interner;
use crate::module::BindingId;

pub mod parse;
pub mod resolve;
pub mod typecheck;
pub mod typegen;

#[derive(Default)]
pub struct AstCtx {
    pub ast: Rc<RefCell<Ast>>,
    pub symbols: Rc<RefCell<Interner>>,
}

impl AstCtx {
    pub fn ast(&self) -> RefMut<Ast> {
        self.ast.borrow_mut()
    }

    pub fn symbols(&self) -> RefMut<Interner> {
        self.symbols.borrow_mut()
    }

    pub fn take(self) -> (Ast, Interner) {
        let ast = self.ast.take();
        let symbols = self.symbols.take();
        (ast, symbols)
    }
}

/// The parsing result, e.g. AST + symbol table
pub struct ParsedProgram {
    pub ast: Ast,
    pub symbols: Interner,
}

/// The resolution result, i.e. associates names with their definitions
pub struct ResolvedProgram {
    pub ast: Ast,
    pub symbols: Interner,
    pub resolution: ResolutionTable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingRef {
    Local(DeclId),
    Module(BindingId),
}

#[derive(Debug, Default)]
pub struct ResolutionTable {
    pub exprs: HashMap<ExprId, BindingRef>,
    pub types: HashMap<TypeId, BindingRef>,
    pub providers: HashMap<TypeId, BindingRef>,
}

/// The result of type checking
#[derive(Debug)]
pub struct TypedProgram {
    pub ast: Ast,
    pub symbols: Interner,
    pub resolution: ResolutionTable,
    pub expr_types: HashMap<ExprId, TypeId>,
}

impl std::fmt::Display for TypedProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Type Inference Results ===\n")?;

        // Print declarations with their inferred types
        for (decl_id, decl) in self.ast.decls.iter() {
            match decl {
                Decl::Let { name, value } => {
                    let name_str = self.symbols.resolve(*name);
                    let value_ty = self.expr_types.get(value);

                    if let Some(ty_id) = value_ty {
                        let ty_str = self.type_to_string(*ty_id);
                        writeln!(f, "let {} : {}", name_str, ty_str)?;
                    }
                }
                Decl::Type { name, ty } => {
                    let name_str = self.symbols.resolve(*name);
                    let ty_str = self.type_to_string(*ty);
                    writeln!(f, "type {} = {}", name_str, ty_str)?;
                }
                Decl::Expr(expr_id) => {
                    if let Some(ty_id) = self.expr_types.get(expr_id) {
                        let ty_str = self.type_to_string(*ty_id);
                        writeln!(f, "<expr> : {}", ty_str)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl TypedProgram {
    fn type_to_string(&self, ty_id: TypeId) -> String {
        match self.ast.types.get(ty_id) {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Int => "int".to_string(),
                PrimitiveType::String => "string".to_string(),
                PrimitiveType::Bool => "bool".to_string(),
            },
            Type::Var(var) => format!("'{}", var),
            Type::List(inner) => format!("[{}]", self.type_to_string(*inner)),
            Type::Record(fields) => {
                let field_strs: Vec<_> = fields
                    .iter()
                    .map(|(name, ty)| {
                        let name_str = self.symbols.resolve(*name);
                        let ty_str = self.type_to_string(*ty);
                        format!("{}: {}", name_str, ty_str)
                    })
                    .collect();
                format!("{{ {} }}", field_strs.join(", "))
            }
            Type::Function(params, ret) => {
                let param_strs: Vec<_> = params.iter().map(|p| self.type_to_string(*p)).collect();
                let ret_str = self.type_to_string(*ret);
                format!("({}) -> {}", param_strs.join(", "), ret_str)
            }
            Type::Named(name) => {
                let name_str = self.symbols.resolve(*name);
                name_str.to_string()
            }
            Type::Provider { .. } => "<provider>".to_string(),
        }
    }
}
