use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::Test;
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Arg, SNode};

pub struct TfpDef {
    child: Option<Token>,
    annotation: Option<PNode<Test>>,
}

impl TfpDef {
    fn new() -> TfpDef {
        TfpDef {
            child: None,
            annotation: None,
        }
    }
}

impl ParseNode<TfpDef> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::Test(c) => {
                Ok({ self.inner.annotation = Some(Rc::clone(c)) })
            }
            _ => Err("only Test allowed as child to TfpDef"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            Token::Name(..) => Ok({ self.inner.child = Some(Token::clone(t)) }),
            _ => Err("unexpected token as child to TfpDef"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Arg>, &'static str> {
        let mut ann = None;
        if let Some(ref annotation) = self.inner.annotation {
            ann = Some(annotation.into_syntax()?);
        }
        if let Some(Token::Name(ref tok)) = self.inner.child {
            Ok(Arg::new(String::clone(tok), ann, self.span))
        } else {
            Err("could not build syntax node from TfpDef")
        }
    }
}

impl_parse_construct!(TfpDef, Arg);
