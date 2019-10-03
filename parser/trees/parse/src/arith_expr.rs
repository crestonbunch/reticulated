use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{Child, Term};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Expr, SNode};

pub struct ArithExpr {
    terms: Vec<PNode<Term>>,
    ops: Vec<Token>,
}

impl ArithExpr {
    fn new() -> ArithExpr {
        ArithExpr {
            terms: vec![],
            ops: vec![],
        }
    }
}

impl ParseNode<ArithExpr> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::Term(term) => Ok(self.inner.terms.push(Rc::clone(term))),
            _ => Err("only Term can be children of ArithExpr"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        Ok(self.inner.ops.push(Token::clone(t)))
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(ArithExpr, Expr);
