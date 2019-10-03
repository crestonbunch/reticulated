use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use lexer::tokens::Token;
use lexer::{tok, Span};
use syntax::{CompOp as SynCompOp, SNode};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum CompOpChild {
    Lt,
    Gt,
    Eq,
    GtE,
    LtE,
    NotEq,
    In,
    NotIn,
    Is,
    IsNot,
    None,
}

/// Represents one of the comparison operators in a parse tree. In Python syntax
/// it will represent one of:
/// `<`, `>`, `==`, `>=`, `<=`, `!=`, `in`, `not in`, `is`, `is not`.
/// Used by a [`Comparison`]
/// parse node to build a [`syntax::Expr::Compare`] node.
///
/// Conversion into a syntax node yields a [`syntax::CompOp`].
///
/// [`Comparison`]: struct.Comparison.html
/// [`syntax::Expr::Compare`]: ../syntax/enum.Expr.html
/// [`syntax::CompOp`]: ../syntax/enum.CompOp.html
pub struct CompOp {
    child: CompOpChild,
    negated: bool,
}

impl CompOp {
    fn new() -> CompOp {
        CompOp {
            child: CompOpChild::None,
            negated: false,
        }
    }
}

impl ParseNode<CompOp> {
    fn add_node(&mut self, _: &NodeProxy) -> Result<(), &'static str> {
        Err("nodes are not allowed as children to CompOp")
    }

    fn add_token(&mut self, t: &Token) -> Result<(), &'static str> {
        let child = self.inner.child;
        self.inner.child = match t {
            tok!("<") => CompOpChild::Lt,
            tok!(">") => CompOpChild::Gt,
            tok!("==") => CompOpChild::Eq,
            tok!(">=") => CompOpChild::GtE,
            tok!("<=") => CompOpChild::LtE,
            tok!("!=") => CompOpChild::NotEq,
            tok!("in") if self.inner.negated => CompOpChild::NotIn,
            tok!("in") => CompOpChild::In,
            tok!("is") => CompOpChild::Is,
            tok!("not") if child == CompOpChild::Is => CompOpChild::IsNot,
            tok!("not") => {
                self.inner.negated = true;
                CompOpChild::None
            }
            _ => return Err("unexpected token as child to CompOp"),
        };
        Ok(())
    }

    fn create_syntax(&self) -> Result<SNode<SynCompOp>, &'static str> {
        let span = self.span;
        let op = match self.inner.child {
            CompOpChild::Lt => SynCompOp::new(SynCompOp::Lt, span),
            CompOpChild::Gt => SynCompOp::new(SynCompOp::Gt, span),
            CompOpChild::Eq => SynCompOp::new(SynCompOp::Eq, span),
            CompOpChild::GtE => SynCompOp::new(SynCompOp::GtE, span),
            CompOpChild::LtE => SynCompOp::new(SynCompOp::LtE, span),
            CompOpChild::NotEq => SynCompOp::new(SynCompOp::NotEq, span),
            CompOpChild::In => SynCompOp::new(SynCompOp::In, span),
            CompOpChild::NotIn => SynCompOp::new(SynCompOp::NotIn, span),
            CompOpChild::Is => SynCompOp::new(SynCompOp::Is, span),
            CompOpChild::IsNot => SynCompOp::new(SynCompOp::IsNot, span),
            CompOpChild::None => return Err("missing comparison for CompOp"),
        };
        Ok(op)
    }
}

impl_parse_construct!(CompOp, SynCompOp);

#[cfg(test)]
mod tests {
    use super::*;

    use lexer::{tok, Span};
    use syntax::CompOp as SynCompOp;

    #[test]
    fn it_works() {
        let test_cases = vec![
            (vec![tok!("<")], CompOpChild::Lt, SynCompOp::Lt),
            (vec![tok!(">")], CompOpChild::Gt, SynCompOp::Gt),
            (vec![tok!("==")], CompOpChild::Eq, SynCompOp::Eq),
            (vec![tok!(">=")], CompOpChild::GtE, SynCompOp::GtE),
            (vec![tok!("<=")], CompOpChild::LtE, SynCompOp::LtE),
            (vec![tok!("!=")], CompOpChild::NotEq, SynCompOp::NotEq),
            (vec![tok!("in")], CompOpChild::In, SynCompOp::In),
            (
                vec![tok!("not"), tok!("in")],
                CompOpChild::NotIn,
                SynCompOp::NotIn,
            ),
            (vec![tok!("is")], CompOpChild::Is, SynCompOp::Is),
            (
                vec![tok!("is"), tok!("not")],
                CompOpChild::IsNot,
                SynCompOp::IsNot,
            ),
        ];

        for (input, expect, syn) in test_cases {
            let zero_span = Span::zero();
            let comp_op =
                <PNode<CompOp> as ParseConstruct<CompOp>>::new(zero_span);
            for t in input {
                comp_op.borrow_mut().add_token(&t).unwrap();
            }

            assert_eq!(expect, comp_op.borrow().inner.child);
            assert_eq!(
                SynCompOp::new(syn, zero_span),
                comp_op.into_syntax().unwrap(),
            );
        }
    }
}
