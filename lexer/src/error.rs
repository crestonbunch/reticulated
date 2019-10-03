use std::error;
use std::fmt;

#[derive(Debug, Clone)]
/// An Error raised by the lexer.
pub struct LexerError {
    msg: String,
    line: u64,
    col: u64,
}

impl<'a> fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Parse error: {} on line {} col {}",
            self.msg, self.line, self.col,
        )
    }
}

impl<'a> error::Error for LexerError {
    fn description(&self) -> &str {
        "Parse error"
    }

    fn cause(&self) -> Option<&error::Error> {
        // No underlying cause for lexer errors
        None
    }
}

impl<'a> LexerError {
    pub fn new(msg: &str, line: u64, col: u64) -> LexerError {
        LexerError {
            msg: msg.into(),
            line,
            col,
        }
    }
}
