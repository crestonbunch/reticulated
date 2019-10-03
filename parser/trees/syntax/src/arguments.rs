use std::cell::RefCell;
use std::rc::Rc;

use crate::{Arg, Expr, SNode, SyntaxConstruct, SyntaxNode};
use lexer::Span;

#[derive(Debug, Eq, PartialEq)]
pub struct Arguments {
    args: Vec<SNode<Arg>>,
    pos_only_args: Vec<SNode<Arg>>,
    var_arg: Option<SNode<Arg>>,
    kw_only_args: Vec<SNode<Arg>>,
    kw_arg: Option<SNode<Arg>>,
    defaults: Vec<Option<SNode<Expr>>>,
    kw_defaults: Vec<Option<SNode<Expr>>>,
}

impl Arguments {
    pub fn new(
        args: Vec<SNode<Arg>>,
        pos_only_args: Vec<SNode<Arg>>,
        var_arg: Option<SNode<Arg>>,
        kw_only_args: Vec<SNode<Arg>>,
        kw_arg: Option<SNode<Arg>>,
        defaults: Vec<Option<SNode<Expr>>>,
        kw_defaults: Vec<Option<SNode<Expr>>>,
        span: Span,
    ) -> SNode<Arguments> {
        SyntaxNode::new(
            Arguments {
                args,
                pos_only_args,
                var_arg,
                kw_only_args,
                kw_arg,
                defaults,
                kw_defaults,
            },
            span,
        )
    }
}

impl SyntaxConstruct for SNode<Arguments> {}
