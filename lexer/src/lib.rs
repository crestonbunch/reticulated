use std::io::Read;

pub mod buffer;
pub mod error;
pub mod tokens;

use buffer::TokenBuffer;
use error::LexerError;
use tokens::{NumberBase, StringFlag, Token};

/// A `Span` describes the start and end points of a token. E.g. for presenting
/// errors to the user, it is helpful to know the location of a token.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Span {
    pub start_line: u64,
    pub start_col: u64,
    pub end_line: u64,
    pub end_col: u64,
}

impl Span {
    /// Create a new span with the desired start and end points.
    pub fn new(
        start_line: u64,
        start_col: u64,
        end_line: u64,
        end_col: u64,
    ) -> Span {
        Span {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }

    /// Create the zero span.
    pub fn zero() -> Span {
        Span::new(0, 0, 0, 0)
    }

    /// Merge two spans together
    pub fn merge(a: Span, b: Span) -> Span {
        let mut start_line = a.start_line;
        let mut start_col = a.start_col;
        let mut end_line = a.end_line;
        let mut end_col = a.end_col;
        if b.start_line < a.start_line {
            start_line = b.start_line;
            start_col = b.start_col;
        } else if b.start_line == a.start_line {
            start_col = a.start_col.min(b.start_col);
        }
        if b.end_line > a.end_line {
            end_line = b.end_line;
            end_col = b.end_col;
        } else if b.end_line == a.end_line {
            end_col = a.end_col.max(b.end_col);
        }
        Span::new(start_line, start_col, end_line, end_col)
    }
}

pub struct ParsedToken(pub Token, pub Span);

impl ParsedToken {
    fn new(token: Token, span: Span) -> ParsedToken {
        ParsedToken(token, span)
    }
}

type LexerResult = Result<Option<ParsedToken>, LexerError>;
type TokenResult = Result<Option<Token>, LexerError>;

pub struct Lexer<R: Read> {
    buffer: TokenBuffer<R>,
    // A stack of indendation levels if tabs were 8 spaces and 1 space,
    // respectively. Tracking both allows us to catch errors when tab levels
    // depend on the size of a tab in spaces.
    indent_stack: Vec<(usize, usize)>,
    indent_depth: usize,
    paren_level: usize,
    // Current line number
    line_num: u64,
    col_num: u64,
    // Line and column index of the current scanning token
    token_start: (u64, u64),
    // Whether or not we are handling a line continuation, if we are then we
    // need to know so we can skip parsing indentation levels.
    cont: bool,
    // True when there are no more tokens left to parse.
    done: bool,
}

impl<R: Read> Iterator for Lexer<R> {
    type Item = Result<ParsedToken, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        // consume input until we find a token
        loop {
            match self.next_token() {
                Ok(Some(token)) => return Some(Ok(token)),
                Ok(None) if self.done => return None,
                Ok(None) => (),
                Err(err) => return Some(Err(err)),
            }
        }
    }
}

impl<'a, R: Read> Lexer<R> {
    /// Creates a new `Lexer`.
    ///
    /// A lexer reads input from a buffer and parses tokens one at a time.
    pub fn new(buffer: TokenBuffer<R>) -> Lexer<R> {
        Lexer {
            buffer,
            indent_stack: vec![(0, 0)],
            indent_depth: 0,
            paren_level: 0,
            line_num: 0,
            col_num: 0,
            token_start: (0, 0), // start line, col of the current token
            cont: false,
            done: false,
        }
    }

    /// Moves the buffer forward, incrementing the column counter
    fn scan_forward(&mut self) -> Option<char> {
        match self.buffer.scan_forward() {
            Some(x) if x == '\n' => {
                // Don't increment the column counter for new lines, we'll
                // reset the counter to 0 when we consume the new line. If we
                // reset it now, we would have to keep a col_num history
                // to undo a new line with scan_back().
                Some(x)
            }
            Some(x) => {
                // Increment the column count when we discover a new character
                self.col_num += 1;
                Some(x)
            }
            None => None,
        }
    }

    /// Move the buffer backward, decrementing the column counter
    fn scan_back(&mut self) {
        let c = self.buffer.scan_back();
        match c {
            // Don't decrement the column when we scan back to a new line since
            // we didn't increment the column in scan_forward.
            '\n' => (),
            _ => self.col_num -= 1,
        }
    }

    /// Helper function to wrap the token with the current line and column
    /// number.
    fn wrap_token(&self, token: Token) -> ParsedToken {
        let (start_line, start_col) = self.token_start;
        let (end_line, end_col) = (self.line_num, self.col_num);
        let span = Span::new(start_line, start_col, end_line, end_col);
        ParsedToken::new(token, span)
    }

    /// Gets the next token result. If the wrapped Option is None, that means
    /// we handled something that wasn't a token (e.g. whitespace, a line
    /// continuation, etc.). The wrapped option will be an EndMarker when
    /// the input buffer reaches EOF.
    fn next_token(&mut self) -> LexerResult {
        if self.done {
            return Ok(None);
        }
        self.token_start = (self.line_num, self.col_num);
        // Look for indents/dedents at the start of every line
        if self.col_num == 0 && !self.cont {
            let result = self.handle_indent();
            match result {
                Ok(Some(token)) => return Ok(Some(self.wrap_token(token))),
                Err(err) => return Err(err),
                _ => (),
            }
        }
        // Predict token handlers by looking at the next character
        let c = self.scan_forward();
        if c.is_none() {
            let token = Token::EndMarker;
            let (line, col) = (self.line_num, self.col_num);
            let span = Span::new(line, col, line, col);
            let token = ParsedToken::new(token, span);
            self.done = true;
            return Ok(Some(token));
        }
        let c = c.unwrap();
        // TODO: support non-ascii identifiers (PEP 3131)
        let token = match c {
            '\t' | ' ' => self.handle_whitespace(),
            '\n' => self.handle_newline(),
            '#' => self.handle_comment(),
            'a'...'z' | 'A'...'Z' | '_' => self.maybe_handle_identifier(c),
            '"' | '\'' => self.handle_string(c, None),
            '.' => self.maybe_handle_number(c),
            '0'...'9' => self.handle_number(c),
            '\\' => self.handle_line_continuation(),
            _ => self.handle_short_tokens(c),
        };
        // Track parenthesis level to decide if whitespace should count toward
        // the indendation level.
        match c {
            '(' | '[' | '{' => self.paren_level += 1,
            ')' | ']' | '}' => self.paren_level -= 1,
            _ => (),
        };
        match token {
            Ok(Some(token)) => Ok(Some(self.wrap_token(token))),
            Ok(None) => Ok(None),
            Err(err) => Err(err),
        }
    }

    /// Handles indents at the beginning of a line, tracking the current
    /// indentation level and ensuring consistent indentation. Returns an
    /// Indent token when the indentation level increases and a Dedent token
    /// when the indentation level decreases.
    fn handle_indent(&mut self) -> TokenResult {
        let mut c = self.scan_forward();
        // Measure the depth of indentations in spaces using tabs of 8 spaces
        // in one variable and tabs of 1 space in another variable.
        // When they disagree about indentation level, the programmer
        // has used tabs and spaces inconsistently.
        let (mut tab_depth, mut space_depth) = (0, 0);
        while match c {
            Some(' ') => {
                tab_depth += 1;
                space_depth += 1;
                true
            }
            Some('\t') => {
                tab_depth = (tab_depth / 8 + 1) * 8;
                space_depth += 1;
                true
            }
            None => return Ok(Some(Token::EndMarker)),
            _ => false,
        } {
            c = self.scan_forward();
        }
        self.scan_back();
        self.buffer.pop(); // consume the whitespace
        if c == Some('#') || c == Some('\n') {
            // Lines with only comments or line breaks do not count toward the
            // indentation level.
            return Ok(None);
        }
        if self.paren_level > 0 {
            // Tabs inside parenthesis do not count toward indentation level.
            return Ok(None);
        }

        let stack = &mut self.indent_stack;
        let (indent_tab_depth, indent_space_depth) =
            *stack.get(self.indent_depth).unwrap();
        if tab_depth == indent_tab_depth {
            if space_depth != indent_space_depth {
                return Err(LexerError::new(
                    "inconsistent use of tabs and spaces in indentation",
                    self.line_num,
                    self.col_num,
                ));
            }
            // No change in indentation level
            Ok(None)
        } else if tab_depth > indent_tab_depth {
            if space_depth <= indent_space_depth {
                return Err(LexerError::new(
                    "inconsistent use of tabs and spaces in indentation",
                    self.line_num,
                    self.col_num,
                ));
            }
            self.indent_depth += 1;
            stack.push((tab_depth, space_depth));
            // Indentation level has increased.
            return Ok(Some(Token::Indent));
        } else {
            while stack.get(self.indent_depth).unwrap().0 > tab_depth {
                self.indent_depth -= 1;
                stack.pop();
            }
            let (indent_tab_depth, indent_space_depth) =
                *stack.get(self.indent_depth).unwrap();
            if tab_depth != indent_tab_depth {
                return Err(LexerError::new(
                    "unindent does not match any outer indentation level",
                    self.line_num,
                    self.col_num,
                ));
            }
            if space_depth != indent_space_depth {
                return Err(LexerError::new(
                    "inconsistent use of tabs and spaces in indentation",
                    self.line_num,
                    self.col_num,
                ));
            }
            // Indentation level has decreased
            return Ok(Some(Token::Dedent));
        }
    }

    /// Consumes whitespace
    fn handle_whitespace(&mut self) -> TokenResult {
        self.buffer.pop(); // consume the whitespace
        Ok(None)
    }

    /// Consumes newlines and increments the line counter and resets the
    /// column counter.
    fn handle_newline(&mut self) -> TokenResult {
        self.line_num += 1;
        self.col_num = 0;
        self.cont = false;
        self.buffer.pop(); // consume the whitespace
        if self.paren_level > 0 {
            // New lines inside of parenthesis are ignored
            return Ok(None);
        }
        Ok(Some(Token::NewLine))
    }

    /// Consumes line continuations and increments the line counter and resets
    /// the column counter.
    fn handle_line_continuation(&mut self) -> TokenResult {
        let c = self.scan_forward();
        if c != Some('\n') {
            return Err(self.handle_unexpected_char());
        }
        self.line_num += 1;
        self.col_num = 0;
        self.cont = true;
        self.buffer.pop(); // consume the line continuation and newline
        Ok(None)
    }

    /// Consumes comments
    fn handle_comment(&mut self) -> TokenResult {
        let mut c = self.scan_forward();
        while match c {
            Some('\n') | None => false,
            _ => true,
        } {
            c = self.scan_forward();
        }
        if c.is_some() {
            // rollback newline characters
            self.scan_back();
        }
        self.buffer.pop(); // consume the comment
        Ok(None)
    }

    /// Helper function to return an unexpected character error
    fn handle_unexpected_char(&self) -> LexerError {
        LexerError::new("unexpected character", self.line_num, self.col_num)
    }

    /// Handles cases where the next token is an identifier.
    fn handle_identifier(&mut self, c: char) -> TokenResult {
        // TODO: support unicode identifiers
        let mut x = Some(c);
        while match x {
            Some('a'...'z') | Some('A'...'Z') | Some('0'...'9') | Some('_') => {
                true
            }
            _ => false,
        } {
            x = self.scan_forward();
        }
        if x.is_some() {
            // The scanned char is not part of the identifier
            self.scan_back();
        }
        let v = self.buffer.pop();
        match &v[..] {
            "if" => Ok(Some(Token::If)),
            "elif" => Ok(Some(Token::Elif)),
            "else" => Ok(Some(Token::Else)),
            "raise" => Ok(Some(Token::Raise)),
            "try" => Ok(Some(Token::Try)),
            "except" => Ok(Some(Token::Except)),
            "finally" => Ok(Some(Token::Finally)),
            "with" => Ok(Some(Token::With)),
            "for" => Ok(Some(Token::For)),
            "while" => Ok(Some(Token::While)),
            "break" => Ok(Some(Token::Break)),
            "continue" => Ok(Some(Token::Continue)),
            "pass" => Ok(Some(Token::Pass)),
            "and" => Ok(Some(Token::And)),
            "or" => Ok(Some(Token::Or)),
            "in" => Ok(Some(Token::In)),
            "is" => Ok(Some(Token::Is)),
            "not" => Ok(Some(Token::Not)),
            "yield" => Ok(Some(Token::Yield)),
            "import" => Ok(Some(Token::Import)),
            "from" => Ok(Some(Token::From)),
            "as" => Ok(Some(Token::As)),
            "lambda" => Ok(Some(Token::Lambda)),
            "True" => Ok(Some(Token::True)),
            "False" => Ok(Some(Token::False)),
            "None" => Ok(Some(Token::None)),
            "assert" => Ok(Some(Token::Assert)),
            "del" => Ok(Some(Token::Del)),
            "return" => Ok(Some(Token::Return)),
            "global" => Ok(Some(Token::Global)),
            "nonlocal" => Ok(Some(Token::Nonlocal)),
            "class" => Ok(Some(Token::Class)),
            "def" => Ok(Some(Token::Def)),
            _ => Ok(Some(Token::Name(v))),
        }
    }

    /// Handles cases where the next token is a string.
    fn handle_string(
        &mut self,
        c: char,
        flag: Option<StringFlag>,
    ) -> TokenResult {
        let quote = c;
        let mut c = self.scan_forward();
        if c == None {
            return Err(LexerError::new(
                "EOF reached while scanning string".into(),
                self.line_num,
                self.col_num,
            ));
        }

        let (quote_size, mut end_quote_size) = {
            let q = self.scan_forward();
            if c == Some(quote) && q == Some(quote) {
                (3, 0) // triple quotes
            } else if c == Some(quote) {
                if !q.is_none() {
                    self.scan_back();
                }
                (1, 1) // empty string
            } else {
                if !q.is_none() {
                    self.scan_back();
                }
                if !c.is_none() {
                    self.scan_back();
                }
                (1, 0) // open quote
            }
        };

        while end_quote_size < quote_size {
            c = self.scan_forward();
            match c {
                None => {
                    return Err(LexerError::new(
                        "EOF reached while scanning string".into(),
                        self.line_num,
                        self.col_num,
                    ));
                }
                Some('\n') if quote_size == 1 => {
                    return Err(LexerError::new(
                        "Unexpected newline in string".into(),
                        self.line_num,
                        self.col_num,
                    ));
                }
                Some('\n') => {
                    // Update line/col counters when encountering a newline
                    self.line_num += 1;
                    self.col_num = 0;
                }
                Some(q) if q == quote => {
                    end_quote_size += 1;
                }
                Some('\\') => {
                    // consume and ignore escaped characters
                    self.scan_forward();
                }
                _ => end_quote_size = 0,
            }
        }

        Ok(Some(Token::String(self.buffer.pop(), flag)))
    }

    /// Helper function to handle numbers in different bases. The passed in
    /// is_digit function should check whether an input character belongs
    /// in the desired base. Moves the buffer pointer forward until the next
    /// character is no longer a digit and returns None. Returns Some(err) if
    /// there is an invalid character while parsing the token.
    fn scan_past_digits<F: Fn(char) -> bool>(
        &mut self,
        is_digit: F,
    ) -> Option<LexerError> {
        let mut c = self.scan_forward();
        // The input number must start with a valid digit
        match c {
            Some(x) if is_digit(x) => (),
            _ => return Some(self.handle_unexpected_char()),
        };
        while {
            match c {
                // Single underscores are skipped
                Some('_') => {
                    c = self.scan_forward();
                    true
                }
                Some(x) => is_digit(x),
                _ => false,
            }
        } {
            // Any non-digit characters after an underscore raise an error
            match c {
                Some(x) if is_digit(x) => (),
                _ => return Some(self.handle_unexpected_char()),
            };
            // Otherwise we consume digits until we find a non-digit
            while match c {
                Some(x) => is_digit(x),
                _ => false,
            } {
                c = self.scan_forward();
            }
        }
        if c.is_some() {
            // roll back the non-digit character
            self.scan_back();
        }
        None
    }

    /// Helper function for parsing decimal numbers including fractions,
    /// imaginary, and exponent numbers.
    fn handle_decimal(&mut self, c: char) -> TokenResult {
        let mut c = Some(c);
        let mut y = self.scan_forward();
        let is_decimal_digit = |x| match x {
            '0'...'9' => true,
            _ => false,
        };

        while match (c, y) {
            (_, Some('.')) => true,
            (_, Some('e')) => true,
            (_, Some('j')) => false,
            (Some('.'), None) => false,
            (Some('j'), None) => false,
            (Some('.'), _) | (Some('e'), _) => {
                self.scan_back();
                true
            }
            (Some('j'), _) => {
                self.scan_back();
                false
            }
            (Some('0'...'9'), Some('_')) => true,
            (Some('0'...'9'), _) => {
                self.scan_back();
                true
            }
            (_, None) => false,
            _ => {
                self.scan_back();
                self.scan_back();
                false
            }
        } {
            let option = self.scan_past_digits(is_decimal_digit);
            match option {
                Some(err) => return Err(err),
                None => (),
            }
            c = self.scan_forward();
            y = self.scan_forward();
        }

        Ok(Some(Token::Number(self.buffer.pop(), NumberBase::Decimal)))
    }

    /// Handles cases where the next token is a number.
    fn handle_number(&mut self, c: char) -> TokenResult {
        let is_hex_digit = |x| match x {
            '0'...'9' | 'a'...'f' | 'A'...'F' => true,
            _ => false,
        };
        let is_octal_digit = |x| match x {
            '0'...'7' => true,
            _ => false,
        };
        let is_binary_digit = |x| match x {
            '0' | '1' => true,
            _ => false,
        };

        let y = self.scan_forward();
        match (c, y) {
            ('0', Some('x')) | ('0', Some('X')) => {
                // Hex
                self.buffer.pop(); // consume the leading 0x
                let option = self.scan_past_digits(is_hex_digit);
                match option {
                    Some(err) => Err(err),
                    None => Ok(Some(Token::Number(
                        self.buffer.pop(),
                        NumberBase::Hexadecimal,
                    ))),
                }
            }
            ('0', Some('o')) | ('0', Some('O')) => {
                // Octal
                self.buffer.pop(); // consume the leading 0o
                let option = self.scan_past_digits(is_octal_digit);
                match option {
                    Some(err) => Err(err),
                    None => Ok(Some(Token::Number(
                        self.buffer.pop(),
                        NumberBase::Octal,
                    ))),
                }
            }
            ('0', Some('b')) | ('0', Some('B')) => {
                // Binary
                self.buffer.pop(); // consume the leading 0b
                let option = self.scan_past_digits(is_binary_digit);
                match option {
                    Some(err) => Err(err),
                    None => Ok(Some(Token::Number(
                        self.buffer.pop(),
                        NumberBase::Binary,
                    ))),
                }
            }
            ('0'...'9', _) => {
                self.scan_back(); // scan back y
                self.handle_decimal(c)
            }
            // TODO: decimal
            _ => Ok(None),
        }
    }

    /// Handles cases where a '.' character might be a decimal number or
    /// it might be the start to a Dot or Ellipsis token.
    fn maybe_handle_number(&mut self, c: char) -> TokenResult {
        let y = self.scan_forward();
        match y {
            Some('0'...'9') => {
                self.scan_back();
                self.handle_number(c)
            }
            _ => {
                self.scan_back();
                // token is Dot or Ellipsis
                self.handle_short_tokens(c)
            }
        }
    }

    /// Helper function to lookahead three characters when matching a Name
    /// token or a String token with leading string flags br, rb, fr, or rf
    fn id_triple_lookahead(
        &mut self,
        x: char,
        y: char,
        z: char,
    ) -> TokenResult {
        let mut handle = |string_flag| {
            self.scan_back(); // rollback quote
            self.buffer.pop(); // consume leading character
            self.scan_forward(); // return to quote
            self.handle_string(z, Some(string_flag))
        };
        match (x, y, z) {
            ('b', 'r', '"')
            | ('r', 'b', '"')
            | ('b', 'r', '\'')
            | ('r', 'b', '\'') => handle(StringFlag::Rb),
            ('r', 'f', '"')
            | ('f', 'r', '"')
            | ('r', 'f', '\'')
            | ('f', 'r', '\'') => handle(StringFlag::Rf),
            (x, y, _) => {
                self.scan_back(); // rollback z
                self.id_double_lookahead(x, y)
            }
        }
    }

    /// Helper function to lookahead two characters when matching a Name token
    /// or a String token with leading string flags b, r, u, or f.
    fn id_double_lookahead(&mut self, x: char, y: char) -> TokenResult {
        let mut handle = |string_flag| {
            self.scan_back(); // rollback quote
            self.buffer.pop(); // consume leading character
            self.scan_forward(); // return to quote
            self.handle_string(y, Some(string_flag))
        };
        match (x, y) {
            ('b', '"') | ('b', '\'') => handle(StringFlag::B),
            ('r', '"') | ('r', '\'') => handle(StringFlag::R),
            ('u', '"') | ('u', '\'') => handle(StringFlag::U),
            ('f', '"') | ('f', '\'') => handle(StringFlag::F),
            (x, _) => {
                self.scan_back(); // rollback y
                self.handle_identifier(x)
            }
        }
    }

    /// Function to match a Name token, or sometimes a String token if it
    /// begins with a flag b, r, u, or f.
    fn maybe_handle_identifier(&mut self, c: char) -> TokenResult {
        let x = c;
        let y = self.scan_forward();
        let z = self.scan_forward();
        match (x, y, z) {
            (x, Some(y), Some(z)) => self.id_triple_lookahead(x, y, z),
            (x, Some(y), None) => self.id_double_lookahead(x, y),
            (x, None, _) => self.handle_identifier(x),
        }
    }

    /// Function to handle tokens that are three characters or less in length
    /// and can be easily looked up in a table by looking ahead up to three
    /// characters.
    fn handle_short_tokens(&mut self, c: char) -> TokenResult {
        let x = c;
        let y = self.scan_forward();
        let z = self.scan_forward();
        match (x, y, z) {
            (x, Some(y), Some(z)) => self.handle_three_char_tokens(x, y, z),
            (x, Some(y), None) => self.handle_two_char_tokens(x, y),
            (x, None, _) => self.handle_one_char_tokens(x),
        }
    }

    fn handle_three_char_tokens(
        &mut self,
        x: char,
        y: char,
        z: char,
    ) -> TokenResult {
        let mut pop_return = |t| {
            self.buffer.pop();
            Ok(Some(t))
        };
        match (x, y, z) {
            ('<', '<', '=') => pop_return(Token::LeftShiftEqual),
            ('>', '>', '=') => pop_return(Token::RightShiftEqual),
            ('*', '*', '=') => pop_return(Token::DoubleStarEqual),
            ('/', '/', '=') => pop_return(Token::DoubleSlashEqual),
            ('.', '.', '.') => pop_return(Token::Ellipsis),
            (_, _, _) => {
                self.scan_back(); // rollback z
                self.handle_two_char_tokens(x, y)
            }
        }
    }

    fn handle_two_char_tokens(&mut self, x: char, y: char) -> TokenResult {
        let mut pop_return = |t| {
            self.buffer.pop();
            Ok(Some(t))
        };
        match (x, y) {
            ('=', '=') => pop_return(Token::EqEqual),
            ('!', '=') => pop_return(Token::NotEqual),
            ('<', '>') => pop_return(Token::NotEqual),
            ('<', '=') => pop_return(Token::LessEqual),
            ('<', '<') => pop_return(Token::LeftShift),
            ('>', '=') => pop_return(Token::GreaterEqual),
            ('>', '>') => pop_return(Token::RightShift),
            ('+', '=') => pop_return(Token::PlusEqual),
            ('-', '=') => pop_return(Token::MinEqual),
            ('-', '>') => pop_return(Token::RArrow),
            ('*', '*') => pop_return(Token::DoubleStar),
            ('*', '=') => pop_return(Token::StarEqual),
            ('/', '/') => pop_return(Token::DoubleSlash),
            ('/', '=') => pop_return(Token::SlashEqual),
            ('|', '=') => pop_return(Token::VBarEqual),
            ('%', '=') => pop_return(Token::PercentEqual),
            ('&', '=') => pop_return(Token::AmperEqual),
            ('^', '=') => pop_return(Token::CircumFlexEqual),
            ('@', '=') => pop_return(Token::AtEqual),
            (':', '=') => pop_return(Token::ColonEqual),
            (x, _) => {
                self.scan_back(); // rollback y
                self.handle_one_char_tokens(x)
            }
        }
    }

    fn handle_one_char_tokens(&mut self, x: char) -> TokenResult {
        let mut pop_return = |t| {
            self.buffer.pop();
            Ok(Some(t))
        };
        match x {
            '(' => pop_return(Token::LParen),
            ')' => pop_return(Token::RParen),
            '[' => pop_return(Token::LBracket),
            ']' => pop_return(Token::RBracket),
            ':' => pop_return(Token::Colon),
            ',' => pop_return(Token::Comma),
            ';' => pop_return(Token::Semi),
            '+' => pop_return(Token::Plus),
            '-' => pop_return(Token::Minus),
            '*' => pop_return(Token::Star),
            '/' => pop_return(Token::Slash),
            '|' => pop_return(Token::VBar),
            '&' => pop_return(Token::Amper),
            '<' => pop_return(Token::Less),
            '>' => pop_return(Token::Greater),
            '=' => pop_return(Token::Equal),
            '.' => pop_return(Token::Dot),
            '%' => pop_return(Token::Percent),
            '{' => pop_return(Token::LBrace),
            '}' => pop_return(Token::RBrace),
            '^' => pop_return(Token::Circumflex),
            '~' => pop_return(Token::Tilde),
            '@' => pop_return(Token::At),
            _ => Err(self.handle_unexpected_char()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_handles_whitespace() {
        let inputs = vec![" ", "\t", "   \t ", "\t  \t  "];
        for input in inputs {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert_eq!(lexer.next().unwrap().unwrap().0, Token::EndMarker);
        }
    }

    #[test]
    fn it_handles_unexpected_chars() {
        let inputs = vec!["!"];
        for input in inputs {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert!(lexer.next().unwrap().is_err());
        }
    }

    #[test]
    fn it_counts_lines() {
        let inputs = vec![
            "foo\nbar\nbaz buzz\n\n\n",
            "foo '''multiline\nstring''' bar",
            "foo. \\\n  bar",
        ];
        let outputs = vec![
            vec![
                (0, 0, 0, 3),
                (0, 3, 1, 0),
                (1, 0, 1, 3),
                (1, 3, 2, 0),
                (2, 0, 2, 3),
                (2, 4, 2, 8),
            ],
            vec![(0, 0, 0, 3), (0, 4, 1, 9), (1, 10, 1, 13)],
            vec![(0, 0, 0, 3), (0, 3, 0, 4), (1, 2, 1, 5)],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for e in expect {
                let actual = lexer.next().unwrap().unwrap();
                let Span {
                    start_line,
                    start_col,
                    end_line,
                    end_col,
                } = actual.1;
                assert_eq!(
                    (start_line, start_col, end_line, end_col),
                    (e.0, e.1, e.2, e.3)
                );
            }
        }
    }

    #[test]
    fn it_handles_line_continuations() {
        let inputs = vec!["foo. \\\n  bar"];
        let outputs = vec![vec![
            Token::Name("foo".into()),
            Token::Dot,
            Token::Name("bar".into()),
            Token::EndMarker,
        ]];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_handles_line_continuation_errors() {
        let inputs = vec!["foo. \\bar"];
        let outputs = vec![vec![Token::Name("foo".into()), Token::Dot]];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
            assert!(lexer.next().unwrap().is_err());
        }
    }

    #[test]
    fn it_parses_indents() {
        let inputs = vec![
            "\t\tfoo\n\t\tbar\nbaz",
            "  foo\n   bar\nbaz",
            "foo\n   \t# bar\n  \t   \n    baz",
            "foo(\n bar\n  baz)",
        ];
        let outputs = vec![
            vec![
                Token::Indent,
                Token::Name("foo".to_owned()),
                Token::NewLine,
                Token::Name("bar".to_owned()),
                Token::NewLine,
                Token::Dedent,
                Token::Name("baz".to_owned()),
                Token::EndMarker,
            ],
            vec![
                Token::Indent,
                Token::Name("foo".to_owned()),
                Token::NewLine,
                Token::Indent,
                Token::Name("bar".to_owned()),
                Token::NewLine,
                Token::Dedent,
                Token::Name("baz".to_owned()),
                Token::EndMarker,
            ],
            vec![
                Token::Name("foo".to_owned()),
                Token::NewLine,
                Token::NewLine,
                Token::NewLine,
                Token::Indent,
                Token::Name("baz".to_owned()),
                Token::EndMarker,
            ],
            vec![
                Token::Name("foo".to_owned()),
                Token::LParen,
                Token::Name("bar".to_owned()),
                Token::Name("baz".to_owned()),
                Token::RParen,
                Token::EndMarker,
            ],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_handles_indent_errors() {
        let inputs = vec![
            "    foo\n        bar\n  baz",
            "    foo\n        bar\n     baz",
            "  \tfoo\n  \tbar\n\t baz",
            "  \tfoo\n\t  bar",
            "  foo\n   bar\n\tbaz",
        ];
        let expect = vec![
            vec![
                Token::Indent,
                Token::Name("foo".into()),
                Token::NewLine,
                Token::Indent,
                Token::Name("bar".into()),
                Token::NewLine,
            ],
            vec![
                Token::Indent,
                Token::Name("foo".into()),
                Token::NewLine,
                Token::Indent,
                Token::Name("bar".into()),
                Token::NewLine,
            ],
            vec![
                Token::Indent,
                Token::Name("foo".into()),
                Token::NewLine,
                Token::Name("bar".into()),
                Token::NewLine,
            ],
            vec![Token::Indent, Token::Name("foo".into()), Token::NewLine],
            vec![
                Token::Indent,
                Token::Name("foo".into()),
                Token::NewLine,
                Token::Indent,
                Token::Name("bar".into()),
                Token::NewLine,
            ],
        ];
        for (input, expect_tokens) in inputs.iter().zip(expect) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            for expect_token in expect_tokens {
                assert_eq!(lexer.next().unwrap().unwrap().0, expect_token);
            }
            assert!(lexer.next().unwrap().is_err());
        }
    }

    #[test]
    fn it_parses_identifiers() {
        let inputs = vec!["FooBar", "Foo Bar Baz"];
        let outputs = vec![
            vec![Token::Name("FooBar".to_owned()), Token::EndMarker],
            vec![
                Token::Name("Foo".to_owned()),
                Token::Name("Bar".to_owned()),
                Token::Name("Baz".to_owned()),
                Token::EndMarker,
            ],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_parses_numbers() {
        let inputs = vec![
            "0xDEADBEEF123",
            "0XFEED_ED",
            "0x12 + 0x3",
            "0o777",
            "0O123",
            "0b110 + 0B10110",
            "123",
            ".123",
            "0.123",
            "123.",
            "123.456",
            "123j",
            "123+4j",
            "12e3",
            "1_1",
            "1.23e4",
            "123.e4",
            "123.4e5j",
            // For some reason leading zeroes are alowed for these types
            // of numbers, but not plain integers. Hopefully no one actually
            // does this?
            "000.",
            "0123.",
            "0123j",
            "0123e1",
            // CPython considers this an error. If handling leading zeros turns
            // out to be a problem we can revisit it.
            "01234",
        ];
        let outputs = vec![
            vec![Token::Number(
                "DEADBEEF123".to_owned(),
                NumberBase::Hexadecimal,
            )],
            vec![Token::Number("FEED_ED".to_owned(), NumberBase::Hexadecimal)],
            vec![
                Token::Number("12".to_owned(), NumberBase::Hexadecimal),
                Token::Plus,
                Token::Number("3".to_owned(), NumberBase::Hexadecimal),
            ],
            vec![Token::Number("777".to_owned(), NumberBase::Octal)],
            vec![Token::Number("123".to_owned(), NumberBase::Octal)],
            vec![
                Token::Number("110".to_owned(), NumberBase::Binary),
                Token::Plus,
                Token::Number("10110".to_owned(), NumberBase::Binary),
            ],
            vec![Token::Number("123".to_owned(), NumberBase::Decimal)],
            vec![Token::Number(".123".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("0.123".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("123.".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("123.456".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("123j".to_owned(), NumberBase::Decimal)],
            vec![
                Token::Number("123".to_owned(), NumberBase::Decimal),
                Token::Plus,
                Token::Number("4j".to_owned(), NumberBase::Decimal),
            ],
            vec![Token::Number("12e3".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("1_1".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("1.23e4".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("123.e4".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("123.4e5j".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("000.".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("0123.".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("0123j".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("0123e1".to_owned(), NumberBase::Decimal)],
            vec![Token::Number("01234".to_owned(), NumberBase::Decimal)],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_handles_number_errors() {
        let inputs = vec![
            "0xDEAD_", "0x", "0xG", "0x__F", "0xA__B", "0o9", "0b2", "3e", "1_",
        ];
        for input in inputs {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert!(lexer.next().unwrap().is_err());
        }
    }

    #[test]
    fn it_parses_three_char_tokens() {
        let inputs = vec!["<<=", ">>=", "**=", "//=", "..."];
        let outputs = vec![
            Token::LeftShiftEqual,
            Token::RightShiftEqual,
            Token::DoubleStarEqual,
            Token::DoubleSlashEqual,
            Token::Ellipsis,
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            let actual = lexer.next().unwrap().unwrap();
            assert_eq!(actual.0, expect);
        }
    }

    #[test]
    fn it_parses_two_char_tokens() {
        let inputs = vec![
            "==", "!=", "<>", "<=", "<<", ">=", ">>", "+=", "-=", "->", "**",
            "*=", "//", "/=", "|=", "%=", "&=", "^=", "@=",
        ];
        let outputs = vec![
            Token::EqEqual,
            Token::NotEqual,
            Token::NotEqual,
            Token::LessEqual,
            Token::LeftShift,
            Token::GreaterEqual,
            Token::RightShift,
            Token::PlusEqual,
            Token::MinEqual,
            Token::RArrow,
            Token::DoubleStar,
            Token::StarEqual,
            Token::DoubleSlash,
            Token::SlashEqual,
            Token::VBarEqual,
            Token::PercentEqual,
            Token::AmperEqual,
            Token::CircumFlexEqual,
            Token::AtEqual,
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            let actual = lexer.next().unwrap().unwrap();
            assert_eq!(actual.0, expect);
        }
    }

    #[test]
    fn it_parses_one_char_tokens() {
        let inputs = vec![
            ":", ",", ";", "+", "-", "*", "/", "|", "&", "<", ">", "=", ".",
            "%", "^", "~", "@",
        ];
        let outputs = vec![
            Token::Colon,
            Token::Comma,
            Token::Semi,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::VBar,
            Token::Amper,
            Token::Less,
            Token::Greater,
            Token::Equal,
            Token::Dot,
            Token::Percent,
            Token::Circumflex,
            Token::Tilde,
            Token::At,
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            let actual = lexer.next().unwrap().unwrap();
            assert_eq!(actual.0, expect);
        }
    }

    #[test]
    fn it_parses_parens() {
        let inputs = vec!["()", "{}", "[]"];
        let outputs = vec![
            vec![Token::LParen, Token::RParen],
            vec![Token::LBrace, Token::RBrace],
            vec![Token::LBracket, Token::RBracket],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_parses_tricky_identifiers() {
        let inputs = vec!["foo", "bar", "rfoo froo rbar brar"];
        let outputs = vec![
            vec![Token::Name("foo".to_owned()), Token::EndMarker],
            vec![Token::Name("bar".to_owned()), Token::EndMarker],
            vec![
                Token::Name("rfoo".to_owned()),
                Token::Name("froo".to_owned()),
                Token::Name("rbar".to_owned()),
                Token::Name("brar".to_owned()),
                Token::EndMarker,
            ],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_parses_keywords() {
        let inputs =
            vec!["if", "else", "elif", "continue", "pass", "yield", "break"];
        let outputs = vec![
            Token::If,
            Token::Else,
            Token::Elif,
            Token::Continue,
            Token::Pass,
            Token::Yield,
            Token::Break,
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert_eq!(lexer.next().unwrap().unwrap().0, expect,);
            assert_eq!(lexer.next().unwrap().unwrap().0, Token::EndMarker);
        }
    }

    #[test]
    fn it_parses_strings() {
        let inputs = vec![
            "''",
            "\"\"",
            "''''''",
            "\"\"\"\"\"\"",
            "\"foo\"",
            "'foo'",
            "'''foo'''",
            "\"\"\"foo\"\"\"",
            "\"\\\"\"",
        ];
        let outputs = vec![
            "''",
            "\"\"",
            "''''''",
            "\"\"\"\"\"\"",
            "\"foo\"",
            "'foo'",
            "'''foo'''",
            "\"\"\"foo\"\"\"",
            "\"\\\"\"",
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert_eq!(
                lexer.next().unwrap().unwrap().0,
                Token::String(expect.into(), None)
            );
            assert_eq!(lexer.next().unwrap().unwrap().0, Token::EndMarker);
        }
    }

    #[test]
    fn it_parses_tricky_strings() {
        let inputs = vec![
            "f\"foo\"",
            "r'foo'",
            "b'''foo'''",
            "u\"\"\"foo\"\"\"",
            "rf'foo'",
            "fr'foo'",
            "rb'foo'",
            "br'foo'",
            "br''",
            "b''",
        ];
        let outputs = vec![
            Token::String("\"foo\"".into(), Some(StringFlag::F)),
            Token::String("'foo'".into(), Some(StringFlag::R)),
            Token::String("'''foo'''".into(), Some(StringFlag::B)),
            Token::String("\"\"\"foo\"\"\"".into(), Some(StringFlag::U)),
            Token::String("'foo'".into(), Some(StringFlag::Rf)),
            Token::String("'foo'".into(), Some(StringFlag::Rf)),
            Token::String("'foo'".into(), Some(StringFlag::Rb)),
            Token::String("'foo'".into(), Some(StringFlag::Rb)),
            Token::String("''".into(), Some(StringFlag::Rb)),
            Token::String("''".into(), Some(StringFlag::B)),
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert_eq!(lexer.next().unwrap().unwrap().0, expect);
            assert_eq!(lexer.next().unwrap().unwrap().0, Token::EndMarker);
        }
    }

    #[test]
    fn it_handles_string_errors() {
        let inputs = vec![
            "\"foobar baz",
            "'foobar baz",
            "\"foobar baz\n\"",
            "'foobar baz\n'",
            "\"\"\"foobar baz'",
            "'''foobar baz'",
            "br'",
            "rf\"",
            "r'",
            "b\"",
            "'",
            "\"",
            "\"\"\"",
            "'''",
            "rb\"\"\"",
            "u'''",
        ];
        for input in inputs {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            assert!(lexer.next().unwrap().is_err());
        }
    }

    #[test]
    fn it_parses_comments() {
        let inputs =
            vec!["# hello world", "# hello world\nfoo", "foo # hello world"];
        let outputs = vec![
            vec![Token::EndMarker],
            vec![Token::NewLine, Token::Name("foo".into()), Token::EndMarker],
            vec![Token::Name("foo".into()), Token::EndMarker],
        ];
        for (input, expect) in inputs.iter().zip(outputs) {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);

            for expect_token in expect {
                let actual = lexer.next().unwrap().unwrap();
                assert_eq!(actual.0, expect_token);
            }
        }
    }

    #[test]
    fn it_parses_entire_buffers() {
        let input = "+";
        let input = TokenBuffer::new(input.as_bytes());
        let lexer = &mut Lexer::new(input);
        assert_eq!(lexer.next().unwrap().unwrap().0, Token::Plus);
        assert_eq!(lexer.next().unwrap().unwrap().0, Token::EndMarker);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn it_parses_simple_functions() {
        let input = "def test_func(param: int, **kwargs) -> int: \
                     \n\treturn param**2";
        let input = TokenBuffer::new(input.as_bytes());
        let lexer = &mut Lexer::new(input);
        let mut i = 0;
        for _ in lexer {
            i += 1;
        }
        assert_eq!(20, i);
    }
}
