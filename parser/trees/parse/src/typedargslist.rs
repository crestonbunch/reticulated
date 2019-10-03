use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{Test, TfpDef};
use lexer::tokens::Token;
use lexer::{tok, Span};
use syntax::{Arguments, Expr, SNode};

enum State {
    ParseKwOrPos,
    ParseVarArgs,
    ParseKwOnlyArgs,
    ParseKwArgs,
    Done,
}

pub struct TypedArgsList {
    // Parameters after '/' (or when '/' is omitted). Can be keyword or
    // positional arguments.
    args: Vec<PNode<TfpDef>>,
    // Parameters before '/' (See PEP 570). These are positional only arguments.
    pos_only_args: Vec<PNode<TfpDef>>,
    // *
    var_args: Option<PNode<TfpDef>>,
    // Keyword args after a * var arg
    kw_only_args: Vec<PNode<TfpDef>>,
    // **
    kw_args: Option<PNode<TfpDef>>,
    // State in the parsing state machine
    state: State,
    // Default values for positional-or-keyword arguments
    defaults: Vec<Option<PNode<Test>>>,
    // Default values for keyword-only arguments
    kw_defaults: Vec<Option<PNode<Test>>>,
}

impl TypedArgsList {
    fn new() -> TypedArgsList {
        TypedArgsList {
            args: vec![],
            pos_only_args: vec![],
            var_args: None,
            kw_only_args: vec![],
            kw_args: None,
            state: State::ParseKwOrPos,
            defaults: vec![],
            kw_defaults: vec![],
        }
    }
}

impl ParseNode<TypedArgsList> {
    fn add_test(&mut self, test: &PNode<Test>) -> Result<(), &'static str> {
        match self.inner.state {
            State::ParseKwOrPos => {
                if let Some(_) = self.inner.defaults.pop() {
                    Ok(self.inner.defaults.push(Some(Rc::clone(test))))
                } else {
                    Err("could not pop arg for default")
                }
            }
            State::ParseVarArgs => Err("cannot add default to var args"),
            State::ParseKwOnlyArgs => {
                if let Some(_) = self.inner.kw_defaults.pop() {
                    Ok(self.inner.kw_defaults.push(Some(Rc::clone(test))))
                } else {
                    Err("could not pop arg for default")
                }
            }
            State::ParseKwArgs => Err("cannot add default to kwargs"),
            State::Done => Err("cannot add default"),
        }
    }

    fn add_tfp_def(&mut self, n: &PNode<TfpDef>) -> Result<(), &'static str> {
        match self.inner.state {
            State::ParseKwOrPos => Ok({
                self.inner.args.push(Rc::clone(n));
                self.inner.defaults.push(None);
            }),
            State::ParseVarArgs => {
                self.inner.state = State::ParseKwOnlyArgs;
                Ok({ self.inner.var_args = Some(Rc::clone(n)) })
            }
            State::ParseKwOnlyArgs => Ok({
                self.inner.kw_only_args.push(Rc::clone(n));
                self.inner.kw_defaults.push(None);
            }),
            State::ParseKwArgs => {
                self.inner.state = State::Done;
                Ok({ self.inner.kw_args = Some(Rc::clone(n)) })
            }
            State::Done => Err("cannot add more args"),
        }
    }

    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::Test(n) => self.add_test(n),
            NodeProxy::TfpDef(n) => self.add_tfp_def(n),
            _ => Err("only Test|TfpDef allowed as child to TypedArgsList"),
        }
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        match t {
            tok!("*") => Ok({ self.inner.state = State::ParseVarArgs }),
            tok!("**") => Ok({ self.inner.state = State::ParseKwArgs }),
            tok!("/") => Ok({
                self.inner.pos_only_args = Vec::clone(&self.inner.args);
                self.inner.args = vec![];
            }),
            tok!(",") => Ok(()),
            tok!("=") => Ok(()),
            _ => Err("unexpected token parsing TypedArgsList"),
        }
    }

    fn create_syntax(&self) -> Result<SNode<Arguments>, &'static str> {
        let args = self
            .inner
            .args
            .iter()
            .map(|x| x.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;
        let pos_only_args = self
            .inner
            .pos_only_args
            .iter()
            .map(|x| x.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;
        let var_arg = self
            .inner
            .var_args
            .clone()
            .map(|x| x.into_syntax().map(|y| Some(y)))
            .unwrap_or(Ok(None))?;
        let kw_only_args = self
            .inner
            .kw_only_args
            .iter()
            .map(|x| x.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;
        let kw_arg = self
            .inner
            .kw_args
            .clone()
            .map(|x| x.into_syntax().map(|y| Some(y)))
            .unwrap_or(Ok(None))?;
        let defaults = self
            .inner
            .defaults
            .iter()
            .map(|x| x.clone().map(|y| y.into_syntax().map(|y| Some(y))))
            .map(|x| x.unwrap_or(Ok(None)))
            .collect::<Result<Vec<_>, &'static str>>()?;
        let kw_defaults = self
            .inner
            .kw_defaults
            .iter()
            .map(|x| x.clone().map(|y| y.into_syntax().map(|y| Some(y))))
            .map(|x| x.unwrap_or(Ok(None)))
            .collect::<Result<Vec<_>, &'static str>>()?;
        Ok(Arguments::new(
            args,
            pos_only_args,
            var_arg,
            kw_only_args,
            kw_arg,
            defaults,
            kw_defaults,
            self.span,
        ))
    }
}

impl_parse_construct!(TypedArgsList, Arguments);
