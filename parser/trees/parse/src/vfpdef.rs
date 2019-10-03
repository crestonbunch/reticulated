use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Arg, SNode};

pub struct VfpDef {
    child: Option<Token>,
}

impl VfpDef {
    fn new() -> VfpDef {
        VfpDef { child: None }
    }
}

impl ParseNode<VfpDef> {
    fn add_node(&mut self, _: &NodeProxy) -> Result<(), &'static str> {
        Err("unexpected node as child to VfpDef")
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            Token::Name(..) => Ok({ self.inner.child = Some(Token::clone(t)) }),
            _ => Err("unexpected token as child to VfpDef"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Arg>, &'static str> {
        if let Some(Token::Name(ref tok)) = self.inner.child {
            Ok(Arg::new(String::clone(tok), None, self.span))
        } else {
            Err("could not build syntax node from VfpDef")
        }
    }
}

impl_parse_construct!(VfpDef, Arg);
