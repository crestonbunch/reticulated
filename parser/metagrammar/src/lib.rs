//! The metagrammar parser.
//!
//! This package contains a procedural rust macro [`build_grammar!`] which
//! is capable of parsing a Python grammar defined using a psuedo-rust syntax,
//! and outputting a function which reads input tokens from a `lexer::Lexer`
//! to parse Python syntax.
//!
//! Mainly this is used in the `grammar` module to build the Python grammar.
//!
//! The metagrammar parser works by reading the input token stream of the
//! grammar definition, and buliding DFAs that accept for each non-terminal.
//! Note that the grammar is assumed to be an LL(1) grammar (no checks or
//! transformations are performed automatically), and it is assumed that the
//! empty string is not a valid transition in any production. These assumptions
//! help make the parser logic simpler to write.
//!
//! The output of the [`build_grammar!`] macro is a function which takes a
//! parser state (the current non-terminal being parsed, the DFA state
//! we're in for that non-terminal, and an input token) then outputs the
//! transition from this state (a new DFA state, whether the state accepts,
//! and optionally a new non-terminal to recursively parse.)
//!
//! [`build_grammar!`]: macro.build_grammar.html

extern crate lexer;
extern crate proc_macro;
mod metagrammar;
mod nfa;

use proc_macro::TokenStream;
use std::collections::HashMap;
use std::iter::repeat;

use quote::{quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, Ident, Token};

use crate::metagrammar::{Alt, Item, ItemRepeat, MetaGrammar};
use nfa::{Dfa, Nfa};

#[derive(Eq, PartialEq, Hash, Clone)]
enum Edge {
    NonTerminal(Ident),
    Terminal(String),
}

fn build_dfa(alts: Punctuated<Alt, Token![|]>) -> Dfa<Edge> {
    let mut nfa = Nfa::new();
    let (a, z) = build_rhs(alts, &mut nfa);
    nfa.set_start(a);
    nfa.set_final(z);
    nfa.into_dfa().minimize()
}

fn build_rhs(
    alts: Punctuated<Alt, Token![|]>,
    nfa: &mut Nfa<Edge>,
) -> (usize, usize) {
    let (a, z) = (nfa.new_state(), nfa.new_state());
    for alt in alts {
        let (b, y) = build_alt(alt, nfa);
        nfa.transition(a, b, None).unwrap();
        nfa.transition(y, z, None).unwrap();
    }
    (a, z)
}

fn build_alt(alt: Alt, nfa: &mut Nfa<Edge>) -> (usize, usize) {
    let (a, mut b) = (nfa.new_state(), nfa.new_state());
    nfa.transition(a, b, None).unwrap();
    for item in alt.items {
        let (c, d) = build_item(item, nfa);
        nfa.transition(b, c, None).unwrap();
        b = d;
    }
    (a, b)
}

fn build_item(item: Item, nfa: &mut Nfa<Edge>) -> (usize, usize) {
    match item {
        Item::Name(v, r) => {
            let (a, z) = (nfa.new_state(), nfa.new_state());
            let token = Edge::NonTerminal(v);
            nfa.transition(a, z, Some(token)).unwrap();
            repeat_item((a, z), r, nfa)
        }
        Item::Str(v, r) => {
            let (a, z) = (nfa.new_state(), nfa.new_state());
            let st = v.into_token_stream().to_string();
            let st = &st[1..st.len() - 1]; // strip surrounding quotes
            let token = Edge::Terminal(st.into());
            nfa.transition(a, z, Some(token)).unwrap();
            repeat_item((a, z), r, nfa)
        }
        Item::Rhs(v, r) => {
            let (a, z) = build_rhs(v, nfa);
            repeat_item((a, z), r, nfa)
        }
    }
}

fn repeat_item(
    (a, z): (usize, usize),
    repeat: ItemRepeat,
    nfa: &mut Nfa<Edge>,
) -> (usize, usize) {
    match repeat {
        ItemRepeat::One => (a, z),
        ItemRepeat::OneOrMore => {
            nfa.transition(z, a, None).unwrap();
            (a, z)
        }
        ItemRepeat::ZeroOrOne => {
            nfa.transition(a, z, None).unwrap();
            (a, z)
        }
        ItemRepeat::ZeroOrMore => {
            nfa.transition(z, a, None).unwrap();
            (a, a)
        }
    }
}

fn build_first_set(
    edge: &Edge,
    dfas: &HashMap<Ident, Dfa<Edge>>,
) -> Vec<String> {
    match edge {
        Edge::Terminal(v) => vec![v.clone()],
        Edge::NonTerminal(v) => {
            let dfa = dfas.get(&v).expect(&format!("no dfa for {}", v));
            let start_state = dfa.start_state.unwrap();
            let first_transitions = dfa.transitions.get(start_state).unwrap();
            first_transitions
                .iter()
                .flat_map(|(e, _)| build_first_set(e, dfas))
                .collect()
        }
    }
}

fn map_token(t: &str) -> proc_macro2::TokenStream {
    match t {
        "NAME" => quote! { Token::Name(..) },
        "NUMBER" => quote! { Token::Number(..) },
        "STRING" => quote! { Token::String(..) },
        "ERROR_TOKEN" => quote! { Token::ErrorToken },
        _ => {
            quote! { tok!(#t) }
        }
    }
}

/// Unroll a set of dfas into a flat vector of each non terminal and the
/// transitions between states. This helper function is used to build a
/// state machine of sorts by creating a `match` block for every possible
/// dfa/state/input token combination and its dfa/state/accept output.
fn unroll_dfas<'a>(
    dfas: &'a HashMap<Ident, Dfa<Edge>>,
) -> Vec<(&'a Ident, &'a Dfa<Edge>, &'a Edge, usize, usize)> {
    dfas.iter()
        .flat_map(|(nt, dfa)| {
            let z = repeat((nt, dfa));
            dfa.transitions.iter().enumerate().zip(z).flat_map(
                |((source_state, edges), (nt, dfa))| {
                    let z = repeat((source_state, nt, dfa));
                    edges.iter().zip(z).map(
                        |((edge, target_state), (source_state, nt, dfa))| {
                            (nt, dfa, edge, source_state, *target_state)
                        },
                    )
                },
            )
        })
        .collect()
}

/// Build the Python grammar from a metagrammar input.
///
/// # Examples
///
/// This is an example showing the metagrammar syntax.
/// ```
/// use metagrammar::build_grammar;
/// use lexer::tokens::Token;
///
/// build_grammar! {
///     IfStmt: "if" Condition ":" Body "else" ":" Body;
///     Condition: "True" | "False";
///     Body: "NAME" "(" (Expr ",")* ")";
///     Expr: "STRING" | "NUMBER";
/// }
///
/// fn main() {
///     let input = Token::Name("if".into());
///     let result = next_state(NonTerminal::IfStmt, 0, &input);
/// }
/// ```
///
#[proc_macro]
pub fn build_grammar(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as MetaGrammar);

    let dfas: HashMap<Ident, Dfa<Edge>> = input
        .productions
        .into_iter()
        .map(|production| (production.name, build_dfa(production.alts)))
        .collect();

    let first_sets: HashMap<&Ident, Vec<String>> = dfas
        .iter()
        .map(|(nt, _)| {
            (
                nt,
                build_first_set(&Edge::NonTerminal(Ident::clone(nt)), &dfas),
            )
        })
        .collect();

    let enum_def = {
        // Enums for every non-terminal in the grammar, corresponding to
        // DFAs we generated from the grammar.
        let nt_enums = dfas.iter().map(|(name, _)| {
            quote! {
                #name
            }
        });
        quote! {
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            pub enum NonTerminal {
                #(#nt_enums),*
            }
        }
    };

    let transition_def = quote! {
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Next {
            // A transition along a terminal means we're still parsing the
            // same DFA, so we return a new state and whether or not the
            // new state accepts.
            Terminal(usize, bool),
            // A transition along a non-terminal means we need to descend into
            // the DFA for the non-terminal and return when it accepts.
            NonTerminal(NonTerminal, usize, bool),
        }
    };

    let dfa_def = {
        // Unwrap DFAs into conditions for a match statement, where each
        // condition checks (NonTerminal, dfa_state, input_token) and returns
        // a Next::Terminal or Next::NonTerminal to decide the
        // next action of the parser.
        let unrolled = unroll_dfas(&dfas);
        let cases = unrolled.iter().map(|(nt, dfa, edge, source, target)| {
            match edge {
                Edge::Terminal(v) => {
                    let accept = dfa.final_states.contains(&target);
                    let token = map_token(&v[..]);
                    // For each terminal `v`, we output a pattern that matches
                    // the Python `token`, the current non-terminal `nt`,
                    // and the current DFA state. We output the next `target`
                    // DFA state after consuming the input token and whether
                    // `target` is an `accept` state or not.
                    quote! {
                        (NonTerminal::#nt, #source, #token) => {
                            Some(Next::Terminal(#target, #accept))
                        }
                    }
                }
                Edge::NonTerminal(v) => {
                    let first = first_sets
                        .get(v)
                        .expect(&format!("missing first set for {}", v));
                    let accept = dfa.final_states.contains(&target);
                    let first = first.iter().map(|x| map_token(&x));

                    // Repeat vars for the quote! macro
                    let nt = repeat(nt);
                    let source = repeat(source);
                    let v = repeat(v);
                    let accept = repeat(accept);
                    let target = repeat(target);
                    // For each terminal `first` in FIRST(v), we output a
                    // pattern that matches `first`, the current DFA
                    // state `source`, and the current non-terminal `nt`. We
                    // output the new non-terminal `v` after consuming the
                    // input `first` token, the new DFA state `target` and  
                    // whether `target` is an `accept` state or not.
                    quote! {
                        #((NonTerminal::#nt, #source, #first) => {
                            Some(Next::NonTerminal(NonTerminal::#v, #target, #accept))
                        })*
                    }
                }
            }
        });
        quote! {
            pub fn next_state(
                nt: NonTerminal,
                state: usize,
                token: &Token,
            ) -> Option<Next> {
                match (nt, state, token) {
                    #(#cases)*
                    _ => None,
                }
            }
        }
    };
    let q = quote!(
        #enum_def
        #transition_def
        #dfa_def
    );

    TokenStream::from(q)
}
