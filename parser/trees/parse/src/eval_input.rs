use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Module, SNode};

pub struct EvalInput {}

impl EvalInput {
    fn new() -> EvalInput {
        EvalInput {}
    }
}

impl ParseNode<EvalInput> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn create_syntax(&self) -> Result<SNode<Module>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(EvalInput, Module);
