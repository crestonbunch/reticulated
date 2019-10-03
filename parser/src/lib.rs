extern crate grammar;
extern crate lexer;

use std::cell::RefCell;
use std::convert::From;
use std::io::Read;
use std::rc::Rc;

use grammar::{next_state, Next, NonTerminal};
use lexer::tokens::Token;
use lexer::{Lexer, ParsedToken, Span};
use trees::parse::{Child, NodeProxy};

type StackState = (NodeProxy, NonTerminal, usize, bool);

pub struct Error {
    token: String,
    span: Span,
}

impl Error {
    pub fn new(t: &Token, span: Span) -> Error {
        Error {
            token: t.to_string(),
            span,
        }
    }
}

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Parser<R> {
        Parser { lexer }
    }

    pub fn parse(mut self, start: NonTerminal) -> Result<NodeProxy, Error> {
        let mut stack: Vec<StackState> = vec![];

        let mut state = (NodeProxy::new(start, Span::zero()), start, 0, false);
        let mut token = self.lexer.next();

        while let Some(Ok(ParsedToken(input, span))) = &token {
            let (_, nt, st, accept) = state;
            let next = next_state(nt, st, input);

            if let Some(Next::Terminal(st, accept)) = next {
                let child = Child::Token(input, span);
                state.0.add_child(child);
                // When we encounter a terminal token, we move to a new DFA
                // state, and continue parsing the input.
                state = (state.0, nt, st, accept);
                token = self.lexer.next();
            } else if let Some(Next::NonTerminal(nt, st, accept)) = next {
                let new_node = NodeProxy::new(nt, *span);
                let child = Child::Node(&new_node);
                state.0.add_child(child);
                // When we encounter a non-terminal token, we save the current
                // parser state onto the stack and begin parsing the new
                // non-terminal. The new non-terminal will create a node in our
                // parse tree.
                stack.push(state);
                state = (new_node, nt, st, accept);
                token = self.lexer.next();
            } else {
                // If consuming this input did not result in a state transition,
                // we can still continue parsing if we're in an accept state.
                if !accept {
                    return Err(Error::new(input, *span));
                    // TODO: if we recover here, we can continue parsing
                    // in panic-mode recovery.
                }
                // To continue parsing we return to the parent node by popping
                // the saved state off of the stack and continuing with the
                // current input token.
                if let Some(s) = stack.pop() {
                    state = s;
                } else {
                    // The stack is empty, but we have not yet encountered
                    // an end marker token. This cannot be parsed.
                    return Err(Error::new(input, *span));
                }
            }
        }

        Ok(state.0)
    }
}
