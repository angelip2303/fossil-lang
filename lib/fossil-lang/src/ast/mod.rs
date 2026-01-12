pub mod ast;
pub mod folder;
pub mod hir;
pub mod thir;
// pub mod visitor;

pub type SourceId = usize;
pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc {
    pub source: SourceId,
    pub span: Span,
}

impl Loc {
    pub fn merge(self, other: Loc) -> Loc {
        Loc {
            source: self.source,
            span: self.span.start..other.span.end,
        }
    }

    /// Create a synthetic/generated location (used for generated code)
    pub fn generated() -> Self {
        Loc {
            source: 0,
            span: 0..0,
        }
    }
}

impl chumsky::span::Span for Loc {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Loc {
            source: context,
            span: range,
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

impl ariadne::Span for Loc {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}
