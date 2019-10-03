use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::Comparison;
use lexer::tokens::{NumberBase, StringFlag, Token};
use lexer::{tok, Span};
use syntax::{Expr, SNode, UnaryOp};

enum NotTestChild {
    NotTest(PNode<NotTest>),
    Comparison(PNode<Comparison>),
    None,
}

pub struct NotTest {
    child: NotTestChild,
}

impl NotTest {
    fn new() -> NotTest {
        NotTest {
            child: NotTestChild::None,
        }
    }
}

impl ParseNode<NotTest> {
    fn add_not_test(&mut self, n: PNode<NotTest>) {
        self.inner.child = NotTestChild::NotTest(n);
    }

    fn add_comparison(&mut self, n: PNode<Comparison>) {
        self.inner.child = NotTestChild::Comparison(n);
    }

    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::NotTest(n) => Ok(self.add_not_test(Rc::clone(n))),
            NodeProxy::Comparison(n) => Ok(self.add_comparison(Rc::clone(n))),
            _ => Err("only NotTest|Comparison can be child to NotTest"),
        }
    }

    fn add_token(&mut self, _: &Token) -> Result<(), &'static str> {
        Err("unexpected token as child to NotTest")
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        match self.inner.child {
            NotTestChild::NotTest(ref n) => Ok(Expr::new_unary_op(
                UnaryOp::Not,
                n.into_syntax()?,
                self.span,
            )),
            NotTestChild::Comparison(ref n) => Ok(Expr::new_unary_op(
                UnaryOp::Not,
                n.into_syntax()?,
                self.span,
            )),
            NotTestChild::None => Err("no child given for NotTest"),
        }
    }
}

impl_parse_construct!(NotTest, Expr);
