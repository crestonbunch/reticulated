use std::cell::RefCell;
use std::rc::Rc;

use crate::{CompOp, Expr, SNode, SyntaxConstruct, SyntaxNode};
use lexer::Span;

#[derive(Debug, Eq, PartialEq)]
pub struct Arg {
    arg: String,
    annotation: Option<SNode<Expr>>,
}

impl Arg {
    pub fn new(
        arg: String,
        annotation: Option<SNode<Expr>>,
        span: Span,
    ) -> SNode<Arg> {
        SyntaxNode::new(Arg { arg, annotation }, span)
    }
}

impl SyntaxConstruct for SNode<Arg> {}
