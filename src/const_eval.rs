use crate::{ast::*, error::Error};

pub struct ConstEvaluator<'a> {
    pub ast: &'a Ast,
}

impl<'a> ConstEvaluator<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { ast }
    }

    pub fn eval_to_string(&self, node_id: NodeId) -> Result<String, Error> {
        match &self.ast.get_expr(node_id).kind {
            ExprKind::String(s) => Ok(s.clone()),

            // ExprKind::BinaryOp { left, op: BinaryOp::Concat, right } => {
            //     let l = self.eval_to_string(*left)?;
            //     let r = self.eval_to_string(*right)?;
            //     Ok(format!("{}{}", l, r))
            // }
            //
            _ => Err(Error::new("Expected compile-time string literal".into())),
        }
    }
}
