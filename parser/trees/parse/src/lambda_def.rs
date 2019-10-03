use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::{NumberBase, StringFlag, Token};
use lexer::{tok, Span};
use syntax::{Expr, SNode};

pub struct LambdaDef {
}

impl LambdaDef {
    fn new() -> LambdaDef {
        LambdaDef {
        }
    }
}

impl ParseNode<LambdaDef> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(LambdaDef, Expr);
