#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Default, Debug, Clone)]
pub struct Ast {
    pub exprs: Vec<Expr>,
    pub stmts: Vec<Stmt>,
    pub types: Vec<Type>,
    pub names: Vec<String>,
}

impl Ast {
    pub fn add_expr(&mut self, expr: ExprKind) -> NodeId {
        let id = NodeId(self.exprs.len());
        self.exprs.push(Expr { id, kind: expr });
        id
    }

    pub fn get_expr(&self, id: NodeId) -> &Expr {
        &self.exprs[id.0]
    }

    pub fn add_stmt(&mut self, stmt: StmtKind) -> NodeId {
        let id = NodeId(self.stmts.len());
        self.stmts.push(Stmt { id, kind: stmt });
        id
    }

    pub fn get_stmt(&self, id: NodeId) -> &Stmt {
        &self.stmts[id.0]
    }

    pub fn add_type(&mut self, ty: TypeKind) -> NodeId {
        let id = NodeId(self.types.len());
        self.types.push(Type { id, kind: ty });
        id
    }

    pub fn get_type(&self, id: NodeId) -> &Type {
        &self.types[id.0]
    }

    pub fn add_name(&mut self, name: impl Into<String>) -> NodeId {
        let id = NodeId(self.names.len());
        self.names.push(name.into());
        id
    }

    pub fn get_name(&self, id: NodeId) -> &str {
        &self.names[id.0]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    Import { path: NodeId, alias: Option<NodeId> },
    Let { name: NodeId, value: NodeId },
    Type { name: NodeId, ty: NodeId },
    Expr(NodeId),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Unit,
    String(String),
    Integer(i64),
    Boolean(bool),
    List(Vec<NodeId>),
    Record(Vec<(NodeId, NodeId)>),
    Identifier(NodeId),
    Path(Vec<NodeId>),
    UnaryOp {
        op: UnaryOp,
        operand: NodeId,
    },
    BinaryOp {
        left: NodeId,
        op: BinaryOp,
        right: NodeId,
    },
    Pipe {
        left: NodeId,
        right: NodeId,
    },
    MemberAccess {
        object: NodeId,
        field: NodeId,
    },
    Function {
        param: Param,
        body: NodeId,
    },
    Call {
        callee: NodeId,
        arg: NodeId,
    },
    Cast {
        expr: NodeId,
        ty: NodeId,
    },
    TypeAnnotation {
        expr: NodeId,
        ty: NodeId,
    },
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Unit,
    Int,
    Bool,
    String,
    Var(Vec<NodeId>),              // TODO: should we have two: identifer and Path?
    List(NodeId),                  // TODO: can the list element type be a Path?
    Record(Vec<(NodeId, NodeId)>), // TODO: can the record rhs be a Path?
    Function { from: NodeId, to: NodeId }, // TODO: can the function types (from and to) be Paths?
    Provider(Vec<NodeId>, Vec<Arg>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: NodeId,
    pub default_value: Option<NodeId>,
    pub ty: Option<NodeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg {
    pub name: Option<NodeId>,
    pub value: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {}
