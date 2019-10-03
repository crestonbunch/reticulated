use std::cell::RefCell;
use std::rc::Rc;

pub trait SyntaxConstruct {}
use lexer::Span;

/// Type alias for a SyntaxNode wrapped by an `Rc` and `RefCell`. Most of the
/// time we use a SyntaxNode, it needs to be wrapped so this cuts down on
/// the boilerplate.
pub type SNode<S> = Rc<RefCell<SyntaxNode<S>>>;

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxNode<S> {
    pub inner: S,
    pub span: Span,
}

impl<S> SyntaxNode<S> {
    pub fn new(inner: S, span: Span) -> SNode<S> {
        Rc::new(RefCell::new(SyntaxNode { inner, span }))
    }
}
