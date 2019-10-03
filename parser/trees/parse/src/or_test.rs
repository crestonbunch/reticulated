use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::AndTest;
use lexer::tokens::Token;
use lexer::{tok, Span};
use syntax::{BoolOp, Expr, SNode};

pub struct OrTest {
    children: Vec<PNode<AndTest>>,
}

impl OrTest {
    fn new() -> OrTest {
        OrTest { children: vec![] }
    }
}

impl ParseNode<OrTest> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::AndTest(n) => Ok(self.inner.children.push(Rc::clone(n))),
            _ => Err("only AndTest allowed as child to OrTest"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            tok!("or") => Ok(()),
            _ => Err("unexpected token in OrTest"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        let values = self
            .inner
            .children
            .iter()
            .map(|child| child.into_syntax())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::new_bool_op(BoolOp::Or, values, self.span))
    }
}

impl_parse_construct!(OrTest, Expr);
