pub mod ast;

pub use self::ast::{
    Argument, Ast, Attribute, AttributeArg, ConstructorParam, Expr, ExprId, ExprKind, Literal,
    Path, PrimitiveType, ProviderArgument, RecordField, Stmt, StmtId, StmtKind, Type, TypeId,
    TypeKind,
};

pub type SourceId = usize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Loc {
    pub source: SourceId,
    pub span: Span,
}

impl Loc {
    pub fn new(source: SourceId, span: impl Into<Span>) -> Self {
        Loc {
            source,
            span: span.into(),
        }
    }

    pub fn generated() -> Self {
        Loc {
            source: 0,
            span: Span::new(0, 0),
        }
    }

    /// Merge two locations into one spanning from start of self to end of other.
    pub fn merge(self, other: Loc) -> Loc {
        Loc {
            source: self.source,
            span: Span::new(self.span.start, other.span.end),
        }
    }
}

impl chumsky::span::Span for Loc {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Loc {
            source: context,
            span: range.into(),
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.span.start
    }

    fn end(&self) -> Self::Offset {
        self.span.end
    }
}
