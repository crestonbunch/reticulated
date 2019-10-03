use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{CompoundStmt, SimpleStmt};
use lexer::tokens::Token;
use lexer::{tok, Span};
use syntax::{Module, SNode};

enum SingleInputChild {
    SimpleStmt(PNode<SimpleStmt>),
    CompoundStmt(PNode<CompoundStmt>),
}

pub struct SingleInput {
    child: Option<SingleInputChild>,
}

impl SingleInput {
    fn new() -> SingleInput {
        SingleInput { child: None }
    }
}

impl ParseNode<SingleInput> {
    fn add_simple_stmt(&mut self, n: PNode<SimpleStmt>) {
        self.inner.child = Some(SingleInputChild::SimpleStmt(n));
    }

    fn add_compound_stmt(&mut self, n: PNode<CompoundStmt>) {
        self.inner.child = Some(SingleInputChild::CompoundStmt(n));
    }

    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::SimpleStmt(n) => Ok(self.add_simple_stmt(Rc::clone(n))),
            NodeProxy::CompoundStmt(n) => {
                Ok(self.add_compound_stmt(Rc::clone(n)))
            }
            _ => Err(
                "only SimpleStmt|CompoundStmt allowed as child to SingleInput",
            ),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            tok!("\n") => Ok(()),
            _ => Err("unexpected token in SingleInput"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Module>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(SingleInput, Module);
