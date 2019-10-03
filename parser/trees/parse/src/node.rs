extern crate lexer;

use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    AndTest, ArithExpr, CompOp, Comparison, CompoundStmt, EvalInput, Expr,
    FileInput, LambdaDef, NotTest, OrTest, SimpleStmt, SingleInput, Stmt, Term,
    Test, TfpDef, TypedArgsList, VarArgsList, VfpDef, YieldArg, YieldExpr,
};
use grammar::NonTerminal;
use lexer::tokens::Token;
use lexer::Span;
use syntax::SyntaxConstruct;

pub enum Child<'a> {
    Node(&'a NodeProxy),
    Token(&'a Token, &'a Span),
}

macro_rules! node_construct {
    ($nt:ident, $span:ident, $($i:ident)*) => {
        match $nt {
            $(NonTerminal::$i => {
                NodeProxy::$i(<PNode<$i> as ParseConstruct<$i>>::new($span))
            })*
            _ => unimplemented!(),
        }
    };
}

macro_rules! node_add_child {
    ($s:ident, $child:ident, $($i:ident)*) => {
        match $s {
            $(NodeProxy::$i(n) => n.add_child($child),)*
        }
    };
}

macro_rules! node_get_span {
    ($s:ident, $child:ident, $($i:ident)*) => {
        match $s {
            $(NodeProxy::$i(n) => n.borrow().span,)*
        }
    };
}

macro_rules! define_nodes {
    ($($i:ident)*) => {
        #[doc="A `NodeProxy` is a special wrapper around a [`ParseNode<P>`]."]
        #[doc="It maps a [`NonTerminal`] enum into an enum corresponding "]
        #[doc="to a node of a parse tree, proxying such methods as"]
        #[doc="[`add_child`] to the underlying `ParseNode` implementation."]
        #[doc=""]
        #[doc="[`ParseNode<P>`]: struct.ParseNode.html"]
        #[doc="[`NonTerminal`]: ../../grammar/enum.NonTerminal.html"]
        #[doc="[`add_child`]: trait.ParseConstruct.html#tymethod.add_child"]
        pub enum NodeProxy {
            $($i(PNode<$i>),)*
        }

        impl NodeProxy {
            pub fn new(nt: NonTerminal, span: Span) -> NodeProxy {
                node_construct! {
                    nt, span, $($i )*
                }
            }

            pub fn add_child(&mut self, child: Child) -> Result<(), &'static str> {
                node_add_child! {
                    self,
                    child,
                    $($i )*
                }
            }

            pub fn span(&self) -> Span {
                node_get_span! {
                    self,
                    child,
                    $($i )*
                }
            }
        }

        pub trait UnwrapNode<T, E> {
            fn unwrap(self) -> Result<PNode<T>, E>;
        }

        $(impl UnwrapNode<$i, &'static str> for NodeProxy {
            fn unwrap(self) -> Result<PNode<$i>, &'static str> {
                if let NodeProxy::$i(n) = self {
                    Ok(n)
                } else {
                    Err(concat!("cannot unwrap ", stringify!($i)))
                }
            }
        })*
    };
}

define_nodes! {
    AndTest
    ArithExpr
    Comparison
    CompOp
    CompoundStmt
    EvalInput
    Expr
    FileInput
    LambdaDef
    NotTest
    OrTest
    SimpleStmt
    SingleInput
    Stmt
    Term
    Test
    TfpDef
    TypedArgsList
    VarArgsList
    VfpDef
    YieldArg
    YieldExpr
}

/// Type alias for a ParseNode wrapped by an `Rc` and `RefCell`. Most of the
/// time we use a ParseNode, it needs to be wrapped so this cuts down on
/// the boilerplate.
pub type PNode<P> = Rc<RefCell<ParseNode<P>>>;

pub struct ParseNode<P> {
    pub inner: P,
    pub span: Span,
}

pub trait ParseConstruct<P> {
    type From: ParseConstruct<P>;
    type Into: SyntaxConstruct;

    fn new(span: Span) -> PNode<P>;

    fn add_child(&mut self, n: Child) -> Result<(), &'static str>;

    fn into_syntax(&self) -> Result<Self::Into, &'static str>;
}

#[macro_export]
macro_rules! impl_parse_construct {
    ($node:ident,$syntax:ident) => {
        impl ParseConstruct<$node> for PNode<$node> {
            type From = PNode<$node>;
            type Into = SNode<$syntax>;

            fn new(span: Span) -> PNode<$node> {
                let inner = $node::new();
                Rc::new(RefCell::new(ParseNode { inner, span }))
            }

            fn add_child(&mut self, n: Child) -> Result<(), &'static str> {
                match n {
                    Child::Node(n) => {
                        let mut node = self.borrow_mut();
                        node.add_node(n)?;
                        node.span = Span::merge(node.span, n.span());
                        Ok(())
                    }
                    Child::Token(t, s) => {
                        let mut node = self.borrow_mut();
                        node.add_token(t)?;
                        node.span = Span::merge(node.span, *s);
                        Ok(())
                    }
                }
            }

            fn into_syntax(&self) -> Result<Self::Into, &'static str> {
                let node = self.borrow();
                node.create_syntax()
            }
        }
    };
}
