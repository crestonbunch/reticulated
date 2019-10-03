use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::NotTest;
use lexer::tokens::Token;
use lexer::{tok, Span};
use syntax::{BoolOp, Expr, SNode};

pub struct AndTest {
    children: Vec<PNode<NotTest>>,
}

impl AndTest {
    fn new() -> AndTest {
        AndTest { children: vec![] }
    }
}

impl ParseNode<AndTest> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::NotTest(n) => Ok(self.inner.children.push(Rc::clone(n))),
            _ => Err("only NotTest allowed as child to AndTest"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            tok!("and") => Ok(()),
            _ => Err("unexpected token in AndTest"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        let values = self
            .inner
            .children
            .iter()
            .map(|child| child.into_syntax())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::new_bool_op(BoolOp::And, values, self.span))
    }
}

impl_parse_construct!(AndTest, Expr);
