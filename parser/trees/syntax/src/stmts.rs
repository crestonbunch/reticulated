use crate::{SNode, Stmt, SyntaxConstruct, SyntaxNode};
use lexer::Span;

pub type Stmts = Vec<SNode<Stmt>>;

impl SyntaxConstruct for SNode<Stmts> {}
