use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{CompoundStmt, SimpleStmt};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{SNode, Stmt as SyntaxStmt};

enum StmtChild {
    Simple(PNode<SimpleStmt>),
    Compound(PNode<CompoundStmt>),
    None,
}

pub struct Stmt {
    child: StmtChild,
}

impl Stmt {
    fn new() -> Stmt {
        Stmt {
            child: StmtChild::None,
        }
    }
}

impl ParseNode<Stmt> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::SimpleStmt(stmt) => {
                self.inner.child = StmtChild::Simple(Rc::clone(stmt))
            }
            NodeProxy::CompoundStmt(stmt) => {
                self.inner.child = StmtChild::Compound(Rc::clone(stmt))
            }
            _ => {
                return Err("only SimpleStmt|CompoundStmt can be child of Stmt")
            }
        };
        Ok(())
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        Err("unexpected token passed as child to Stmt")
    }

    fn create_syntax(&self) -> Result<SNode<SyntaxStmt>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(Stmt, SyntaxStmt);
