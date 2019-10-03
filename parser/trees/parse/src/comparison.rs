use std::cell::RefCell;
use std::rc::Rc;

use crate::node::{Child, NodeProxy, PNode, ParseConstruct, ParseNode};
use crate::{CompOp, Expr};
use lexer::tokens::Token;
use lexer::Span;
use syntax::{Expr as SyntaxExpr, SNode};

/// A parse node representing a comparison between two or more [`Expr`] nodes.
/// The comparison operators are defined by [`CompOp`] nodes. In Python syntax
/// this looks like `expr < 5`, `expr is not None`, `5 < expr < 10`,
/// `expr1 != expr2`, etc.
///
/// Conversion into a syntax node yields a [`syntax::Expr::Compare`].
///
/// [`Expr`]: struct.Expr.html
/// [`CompOp`]: struct.CompOp.html
/// [`syntax::Expr::Compare`]: ../syntax/enum.Expr.html
pub struct Comparison {
    children: Vec<PNode<Expr>>,
    ops: Vec<PNode<CompOp>>,
}

impl Comparison {
    fn new() -> Comparison {
        Comparison {
            children: vec![],
            ops: vec![],
        }
    }
}

impl ParseNode<Comparison> {
    fn add_comp_op(&mut self, comp_op: PNode<CompOp>) {
        self.inner.ops.push(comp_op);
    }

    fn add_expr(&mut self, expr: PNode<Expr>) {
        self.inner.children.push(expr);
    }

    fn add_node(&mut self, n: &NodeProxy) -> Result<(), &'static str> {
        match n {
            NodeProxy::CompOp(comp_op) => {
                Ok(self.add_comp_op(Rc::clone(comp_op)))
            }
            NodeProxy::Expr(expr) => Ok(self.add_expr(Rc::clone(expr))),
            _ => Err("only CompOp|Expr can be child to Comparison"),
        }
    }

    fn add_token(&mut self, _: &Token) -> Result<(), &'static str> {
        Err("unexpected token as child to Comparison")
    }

    fn create_syntax(&self) -> Result<SNode<SyntaxExpr>, &'static str> {
        let ops = self
            .inner
            .ops
            .iter()
            .map(|op| op.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;
        let nodes = self
            .inner
            .children
            .iter()
            .map(|child| child.into_syntax())
            .collect::<Result<Vec<_>, &'static str>>()?;

        if let Some((left, comparators)) = nodes.split_first() {
            let left = Rc::clone(left);
            let comparators = Vec::from(comparators);
            let ops = ops.into_iter().zip(comparators).collect::<Vec<_>>();
            Ok(SyntaxExpr::new_compare(left, ops, self.span))
        } else {
            Err("no comparators provided in comparison")
        }
    }
}

impl_parse_construct!(Comparison, SyntaxExpr);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::{Child, ParseConstruct, UnwrapNode};
    use grammar::NonTerminal;
    use lexer::tok;

    #[test]
    fn it_works() {
        let left = NodeProxy::new(NonTerminal::Expr, Span::new(0, 0, 0, 4));
        let mut op = NodeProxy::new(NonTerminal::CompOp, Span::zero());
        let right = NodeProxy::new(NonTerminal::Expr, Span::new(1, 2, 1, 3));
        let mut comp = NodeProxy::new(NonTerminal::Comparison, Span::zero());

        op.add_child(Child::Token(&tok!("<"), &Span::new(0, 5, 0, 6)))
            .unwrap();
        comp.add_child(Child::Node(&left)).unwrap();
        comp.add_child(Child::Node(&op)).unwrap();
        comp.add_child(Child::Node(&right)).unwrap();

        let comp = UnwrapNode::<Comparison, _>::unwrap(comp).unwrap();
        let left = UnwrapNode::<Expr, _>::unwrap(left).unwrap();
        let op = UnwrapNode::<CompOp, _>::unwrap(op).unwrap();
        let right = UnwrapNode::<Expr, _>::unwrap(right).unwrap();

        let new_span = Span::new(0, 0, 1, 3);
        assert_eq!(comp.borrow().span, new_span);

        let actual: SNode<SyntaxExpr> = comp.into_syntax().unwrap();
        let expect = SyntaxExpr::new_compare(
            left.into_syntax().unwrap(),
            vec![(op.into_syntax().unwrap(), right.into_syntax().unwrap())],
            new_span,
        );
        assert_eq!(actual, expect);
    }
}
