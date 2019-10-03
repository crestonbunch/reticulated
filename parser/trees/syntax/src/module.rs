use crate::{Expr, SNode, Stmt, SyntaxConstruct, SyntaxNode};
use lexer::Span;

pub enum Module {
    // Module(stmt* body, type_ignore *type_ignores)
    Module(Vec<SNode<Stmt>>),
    // Interactive(stmt* body)
    Interactive(Vec<SNode<Stmt>>),
    // Expression(expr body)
    Expression(SNode<Expr>),
    // FunctionType(expr* argtypes, expr returns)
    FunctionType(Vec<SNode<Expr>>, SNode<Expr>),
}

impl Module {
    pub fn new_module(stmts: Vec<SNode<Stmt>>, span: Span) -> SNode<Module> {
        SyntaxNode::new(Module::Module(stmts), span)
    }
}

impl SyntaxConstruct for SNode<Module> {}
