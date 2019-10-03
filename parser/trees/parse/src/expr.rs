use std::cell::RefCell;
use std::rc::Rc;

use crate::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Expr as SyntaxExpr, SNode};

pub struct Expr {
    children: Vec<PNode<Expr>>,
}

impl Expr {
    fn new() -> Expr {
        Expr { children: vec![] }
    }
}

impl ParseNode<Expr> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn create_syntax(&self) -> Result<SNode<SyntaxExpr>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(Expr, SyntaxExpr);
