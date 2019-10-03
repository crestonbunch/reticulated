use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{LambdaDef, OrTest};
use lexer::tokens::{NumberBase, StringFlag, Token};
use lexer::{tok, Span};
use syntax::{Expr, SNode};

enum TestChild {
    OrTest(PNode<OrTest>),
    LambdaDef(PNode<LambdaDef>),
    Ternary(PNode<OrTest>, PNode<OrTest>, PNode<Test>),
}

pub struct Test {}

impl Test {
    fn new() -> Test {
        Test {}
    }
}

impl ParseNode<Test> {
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

impl_parse_construct!(Test, Expr);
