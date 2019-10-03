use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::{NumberBase, StringFlag, Token};
use lexer::{tok, Span};
use syntax::{Expr, SNode};

enum AtomChild {
    Name(String),
    Number(String, NumberBase),
    Strings(Vec<(String, Option<StringFlag>)>),
    YieldExpr,
    TestListComp,
    DictOrSetMaker,
    Ellipsis,
    None,
    True,
    False,
}

pub struct Atom {
    child: AtomChild,
}

impl Atom {
    fn new() -> Atom {
        Atom {
            child: AtomChild::None,
        }
    }
}

impl ParseNode<Atom> {
    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        unimplemented!()
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            tok!("(")
            | tok!(")")
            | tok!("[")
            | tok!("]")
            | tok!("{")
            | tok!("}") => Ok(()),
            tok!("...") => Ok({ self.inner.child = AtomChild::Ellipsis }),
            tok!("None") => Ok({ self.inner.child = AtomChild::None }),
            tok!("True") => Ok({ self.inner.child = AtomChild::True }),
            tok!("False") => Ok({ self.inner.child = AtomChild::False }),
            Token::Name(v) => {
                Ok({ self.inner.child = AtomChild::Name(String::clone(v)) })
            }
            Token::String(v, f) => {
                if let AtomChild::Strings(ref strings) = self.inner.child {
                    let mut strings = Vec::clone(strings);
                    strings.push((String::clone(v), *f));
                    Ok({ self.inner.child = AtomChild::Strings(strings) })
                } else {
                    let strings = vec![(String::clone(v), *f)];
                    Ok({ self.inner.child = AtomChild::Strings(strings) })
                }
            }
            Token::Number(v, b) => Ok({
                self.inner.child = AtomChild::Number(String::clone(v), *b)
            }),
            _ => Err("unexpected token as child to Atom"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Expr>, &'static str> {
        unimplemented!();
    }
}

impl_parse_construct!(Atom, Expr);
