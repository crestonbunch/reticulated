extern crate proc_macro;

use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{bracketed, parenthesized};
use syn::{token, Ident, Lit, Token};

pub struct MetaGrammar {
    pub productions: Punctuated<Production, Token![;]>,
}

impl Parse for MetaGrammar {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(MetaGrammar {
            productions: Punctuated::parse_terminated(input)?,
        })
    }
}

// PRODUCITON: NAME ':' ALT ('|' ALT)
pub struct Production {
    pub name: Ident,
    pub alts: Punctuated<Alt, Token![|]>,
}

impl Parse for Production {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let alts = Punctuated::parse_separated_nonempty(input)?;
        Ok(Production { name, alts })
    }
}

// ALT: ITEM+
pub struct Alt {
    pub items: Vec<Item>,
}

impl Parse for Alt {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut items = vec![];
        let mut lookahead = input.lookahead1();
        while lookahead.peek(Ident)
            || lookahead.peek(Lit)
            || lookahead.peek(token::Paren)
            || lookahead.peek(token::Bracket)
        {
            let item = input.parse()?;
            items.push(item);
            lookahead = input.lookahead1();
        }
        Ok(Alt { items })
    }
}

pub enum ItemRepeat {
    One,
    OneOrMore,
    ZeroOrOne,
    ZeroOrMore,
}

// ITEM: '[' RHS ']' | ATOM ['+' | '*']
// ATOM: '(' RHS ')' | NAME | STRING
pub enum Item {
    Rhs(Punctuated<Alt, Token![|]>, ItemRepeat),
    Name(Ident, ItemRepeat),
    Str(Lit, ItemRepeat),
}

impl Parse for Item {
    fn parse(input: ParseStream) -> Result<Self> {
        // Helper closure to lookup a trailing * or + symbol
        let op = || {
            let lookahead = input.lookahead1();
            let mut op = ItemRepeat::One;
            if lookahead.peek(Token![*]) {
                op = ItemRepeat::ZeroOrMore;
                input.parse::<Token![*]>().unwrap();
            } else if lookahead.peek(Token![+]) {
                op = ItemRepeat::OneOrMore;
                input.parse::<Token![+]>().unwrap();
            }
            op
        };

        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            Ok(Item::Name(input.parse()?, op()))
        } else if lookahead.peek(Lit) {
            Ok(Item::Str(input.parse()?, op()))
        } else if lookahead.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let items = Punctuated::parse_separated_nonempty(&content)?;
            Ok(Item::Rhs(items, op()))
        } else if lookahead.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            let items = Punctuated::parse_separated_nonempty(&content)?;
            Ok(Item::Rhs(items, ItemRepeat::ZeroOrOne))
        } else {
            Err(input.error("Invalid item token"))
        }
    }
}
