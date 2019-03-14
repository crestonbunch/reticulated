struct Terminal<'a>(&'a str);

struct NonTerminal<'a>(&'a str);

struct Production<'a> {
    symbol: NonTerminal<'a>,
    choices: [Box<Production<'a>>],
}

pub struct Grammar {}
