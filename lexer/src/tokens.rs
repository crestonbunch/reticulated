use std::fmt;

/// String literals can be preceeded by any of b, r, u, f, rb, rf in Python
/// so this enum tracks which flag (if any) is present on the string.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StringFlag {
    B,
    R,
    U,
    F,
    Rb,
    Rf,
}

impl fmt::Display for StringFlag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StringFlag::B => write!(f, "b"),
            StringFlag::R => write!(f, "r"),
            StringFlag::U => write!(f, "u"),
            StringFlag::F => write!(f, "f"),
            StringFlag::Rb => write!(f, "rb"),
            StringFlag::Rf => write!(f, "rf"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NumberBase {
    Decimal,
    Hexadecimal,
    Octal,
    Binary,
}

impl fmt::Display for NumberBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NumberBase::Decimal => write!(f, "10"),
            NumberBase::Hexadecimal => write!(f, "16"),
            NumberBase::Octal => write!(f, "8"),
            NumberBase::Binary => write!(f, "2"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    EndMarker,
    Name(String),
    Number(String, NumberBase),
    String(String, Option<StringFlag>),
    NewLine,
    Indent,
    Dedent,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Colon,
    Comma,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    VBar,
    Amper,
    Less,
    Greater,
    Equal,
    Dot,
    Percent,
    LBrace,
    RBrace,
    EqEqual,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Tilde,
    Circumflex,
    LeftShift,
    RightShift,
    DoubleStar,
    PlusEqual,
    MinEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    AmperEqual,
    VBarEqual,
    CircumFlexEqual,
    LeftShiftEqual,
    RightShiftEqual,
    DoubleStarEqual,
    DoubleSlash,
    DoubleSlashEqual,
    At,
    AtEqual,
    RArrow,
    Ellipsis,
    ColonEqual,

    // Reserved keywords
    If,
    Elif,
    Else,
    Raise,
    Try,
    Except,
    Finally,
    With,
    For,
    While,
    Break,
    Continue,
    Pass,
    And,
    Or,
    In,
    Is,
    Not,
    Yield,
    Import,
    From,
    As,
    Lambda,
    True,
    False,
    None,
    Assert,
    Del,
    Return,
    Global,
    Nonlocal,
    Class,
    Def,
    Async,
    Await,

    ErrorToken,
}

impl<'a> fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EndMarker => write!(f, "EndMarker"),
            Token::Name(v) => write!(f, "{}", v),
            Token::Number(v, _) => write!(f, "{}", v),
            Token::String(v, _) => write!(f, "{}", v),
            Token::NewLine => write!(f, "NewLine"),
            Token::Indent => write!(f, "Indent"),
            Token::Dedent => write!(f, "Dedent"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Semi => write!(f, ";"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::VBar => write!(f, "|"),
            Token::Amper => write!(f, "&"),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::Equal => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::Percent => write!(f, "%"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::EqEqual => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::Tilde => write!(f, "~"),
            Token::Circumflex => write!(f, "^"),
            Token::LeftShift => write!(f, "<<"),
            Token::RightShift => write!(f, ">>"),
            Token::DoubleStar => write!(f, "**"),
            Token::PlusEqual => write!(f, "+="),
            Token::MinEqual => write!(f, "-="),
            Token::StarEqual => write!(f, "*="),
            Token::SlashEqual => write!(f, "/="),
            Token::PercentEqual => write!(f, "%="),
            Token::AmperEqual => write!(f, "&="),
            Token::VBarEqual => write!(f, "|="),
            Token::CircumFlexEqual => write!(f, "^="),
            Token::LeftShiftEqual => write!(f, "<<="),
            Token::RightShiftEqual => write!(f, ">>="),
            Token::DoubleStarEqual => write!(f, "**="),
            Token::DoubleSlash => write!(f, "//"),
            Token::DoubleSlashEqual => write!(f, "//="),
            Token::At => write!(f, "@"),
            Token::AtEqual => write!(f, "@="),
            Token::RArrow => write!(f, "->"),
            Token::Ellipsis => write!(f, "..."),
            Token::ColonEqual => write!(f, ":="),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::ErrorToken => write!(f, ""),
            Token::If => write!(f, "if"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::Raise => write!(f, "raise"),
            Token::Try => write!(f, "try"),
            Token::Except => write!(f, "except"),
            Token::Finally => write!(f, "finally"),
            Token::With => write!(f, "with"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Pass => write!(f, "pass"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::In => write!(f, "in"),
            Token::Is => write!(f, "is"),
            Token::Not => write!(f, "not"),
            Token::Yield => write!(f, "yield"),
            Token::Import => write!(f, "import"),
            Token::From => write!(f, "from"),
            Token::As => write!(f, "as"),
            Token::Lambda => write!(f, "lambda"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
            Token::None => write!(f, "None"),
            Token::Assert => write!(f, "assert"),
            Token::Del => write!(f, "del"),
            Token::Return => write!(f, "return"),
            Token::Global => write!(f, "global"),
            Token::Nonlocal => write!(f, "nonlocal"),
            Token::Class => write!(f, "class"),
            Token::Def => write!(f, "def"),
        }
    }
}

#[macro_export]
macro_rules! tok {
    ("ENDMARKER") => {
        Token::EndMarker
    };
    ("\n") => {
        Token::NewLine
    };
    ("NEWLINE") => {
        Token::NewLine
    };
    ("\t") => {
        Token::Indent
    };
    ("INDENT") => {
        Token::Indent
    };
    ("DEDENT") => {
        Token::Dedent
    };
    ("(") => {
        Token::LParen
    };
    (")") => {
        Token::RParen
    };
    ("[") => {
        Token::LBracket
    };
    ("]") => {
        Token::RBracket
    };
    (":") => {
        Token::Colon
    };
    (",") => {
        Token::Comma
    };
    (";") => {
        Token::Semi
    };
    ("+") => {
        Token::Plus
    };
    ("-") => {
        Token::Minus
    };
    ("*") => {
        Token::Star
    };
    ("/") => {
        Token::Slash
    };
    ("|") => {
        Token::VBar
    };
    ("&") => {
        Token::Amper
    };
    ("<") => {
        Token::Less
    };
    (">") => {
        Token::Greater
    };
    ("=") => {
        Token::Equal
    };
    (".") => {
        Token::Dot
    };
    ("%") => {
        Token::Percent
    };
    ("{") => {
        Token::LBrace
    };
    ("}") => {
        Token::RBrace
    };
    ("==") => {
        Token::EqEqual
    };
    ("!=") => {
        Token::NotEqual
    };
    ("<>") => {
        Token::NotEqual
    };
    ("<=") => {
        Token::LessEqual
    };
    (">=") => {
        Token::GreaterEqual
    };
    ("~") => {
        Token::Tilde
    };
    ("^") => {
        Token::Circumflex
    };
    ("<<") => {
        Token::LeftShift
    };
    (">>") => {
        Token::RightShift
    };
    ("**") => {
        Token::DoubleStar
    };
    ("+=") => {
        Token::PlusEqual
    };
    ("-=") => {
        Token::MinEqual
    };
    ("*=") => {
        Token::StarEqual
    };
    ("/=") => {
        Token::SlashEqual
    };
    ("%=") => {
        Token::PercentEqual
    };
    ("&=") => {
        Token::AmperEqual
    };
    ("|=") => {
        Token::VBarEqual
    };
    ("^=") => {
        Token::CircumFlexEqual
    };
    ("<<=") => {
        Token::LeftShiftEqual
    };
    (">>=") => {
        Token::RightShiftEqual
    };
    ("**=") => {
        Token::DoubleStarEqual
    };
    ("//") => {
        Token::DoubleSlash
    };
    ("//=") => {
        Token::DoubleSlashEqual
    };
    ("@") => {
        Token::At
    };
    ("@=") => {
        Token::AtEqual
    };
    ("->") => {
        Token::RArrow
    };
    ("...") => {
        Token::Ellipsis
    };
    (":=") => {
        Token::ColonEqual
    };
    ("if") => {
        Token::If
    };
    ("elif") => {
        Token::Elif
    };
    ("else") => {
        Token::Else
    };
    ("raise") => {
        Token::Raise
    };
    ("try") => {
        Token::Try
    };
    ("except") => {
        Token::Except
    };
    ("finally") => {
        Token::Finally
    };
    ("with") => {
        Token::With
    };
    ("for") => {
        Token::For
    };
    ("while") => {
        Token::While
    };
    ("break") => {
        Token::Break
    };
    ("continue") => {
        Token::Continue
    };
    ("pass") => {
        Token::Pass
    };
    ("and") => {
        Token::And
    };
    ("or") => {
        Token::Or
    };
    ("in") => {
        Token::In
    };
    ("is") => {
        Token::Is
    };
    ("not") => {
        Token::Not
    };
    ("yield") => {
        Token::Yield
    };
    ("import") => {
        Token::Import
    };
    ("from") => {
        Token::From
    };
    ("as") => {
        Token::As
    };
    ("lambda") => {
        Token::Lambda
    };
    ("True") => {
        Token::True
    };
    ("False") => {
        Token::False
    };
    ("None") => {
        Token::None
    };
    ("assert") => {
        Token::Assert
    };
    ("del") => {
        Token::Del
    };
    ("return") => {
        Token::Return
    };
    ("global") => {
        Token::Global
    };
    ("nonlocal") => {
        Token::Nonlocal
    };
    ("class") => {
        Token::Class
    };
    ("def") => {
        Token::Def
    };
    ("async") => {
        Token::Async
    };
    ("await") => {
        Token::Await
    };
}

#[macro_export]
macro_rules! tok_name {
    ($name:literal) => {
        Token::Name($name.into())
    };
}
