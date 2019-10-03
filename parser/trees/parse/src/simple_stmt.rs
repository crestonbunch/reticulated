use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{SNode, Stmts};

pub struct SimpleStmt {}

impl SimpleStmt {
    fn new() -> SimpleStmt {
        SimpleStmt {}
    }
}

impl ParseNode<SimpleStmt> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn create_syntax(&self) -> Result<SNode<Stmts>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(SimpleStmt, Stmts);
