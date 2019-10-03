use std::cell::RefCell;
use std::rc::Rc;

use crate::{CompOp, SNode, SyntaxConstruct, SyntaxNode};
use lexer::Span;

#[derive(Debug, Eq, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Invert,
    Not,
    UAdd,
    USub,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    BoolOp(BoolOp, Vec<SNode<Expr>>),
    UnaryOp(UnaryOp, SNode<Expr>),
    Compare(SNode<Expr>, Vec<(SNode<CompOp>, SNode<Expr>)>),
}

impl Expr {
    pub fn new_bool_op(
        op: BoolOp,
        values: Vec<SNode<Expr>>,
        span: Span,
    ) -> SNode<Expr> {
        SyntaxNode::new(Expr::BoolOp(op, values), span)
    }

    pub fn new_unary_op(
        op: UnaryOp,
        operand: SNode<Expr>,
        span: Span,
    ) -> SNode<Expr> {
        SyntaxNode::new(Expr::UnaryOp(op, operand), span)
    }

    pub fn new_compare(
        left: SNode<Expr>,
        comparators: Vec<(SNode<CompOp>, SNode<Expr>)>,
        span: Span,
    ) -> SNode<Expr> {
        SyntaxNode::new(Expr::Compare(left, comparators), span)
    }
}

impl SyntaxConstruct for SNode<Expr> {}
