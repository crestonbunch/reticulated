use crate::{SNode, SyntaxConstruct, SyntaxNode};
use lexer::Span;

pub enum Stmt {}

impl SyntaxConstruct for SNode<Stmt> {}
