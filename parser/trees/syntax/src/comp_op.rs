use crate::{SNode, SyntaxConstruct, SyntaxNode};
use lexer::Span;

#[derive(Debug, Eq, PartialEq)]
pub enum CompOp {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
    Is,
    IsNot,
    In,
    NotIn,
}

impl CompOp {
    pub fn new(comp_op: CompOp, span: Span) -> SNode<CompOp> {
        SyntaxNode::new(comp_op, span)
    }
}

impl SyntaxConstruct for SNode<CompOp> {}
