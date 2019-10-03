use std::cell::RefCell;
use std::rc::Rc;

use crate::{Child, NodeProxy, PNode, ParseConstruct, ParseNode, Stmt};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Module, SNode};

pub struct FileInput {
    children: Vec<PNode<Stmt>>,
}

impl FileInput {
    fn new() -> FileInput {
        FileInput { children: vec![] }
    }
}

impl ParseNode<FileInput> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::Stmt(stmt) => {
                Ok(self.inner.children.push(Rc::clone(stmt)))
            }
            _ => Err("only Stmt can be children of FileInput"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            Token::NewLine => Ok(()),
            Token::EndMarker => Ok(()),
            _ => Err("unexpected token passed as child to FileInput"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Module>, &'static str> {
        let body = self
            .inner
            .children
            .iter()
            .map(|child| child.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;
        Ok(Module::new_module(body, self.span))
    }
}

impl_parse_construct!(FileInput, Module);
