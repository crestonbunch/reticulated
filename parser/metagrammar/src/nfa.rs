use std::collections::{HashMap, HashSet};
use std::fmt;

/// A transition is an arc along the NFA from one state to another. Transitions
/// may follow an input token, or they may be 'epsilon' transitions which
/// are applied immediately.
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Transition<T>(Option<T>, usize);

/// An NFA is a set of state transitions given an optional token. Any state
/// can have any number of transitions and epsilon-transitions to other states.
/// It is easy to build NFAs, but it is difficult to use them. Most of the time
/// you want to build an NFA then convert it to a DFA, which is easily done
/// using the [`into_dfa`] method.
///
/// [`into_dfa`]: #method.into_dfa
#[derive(Debug, PartialEq, Eq)]
pub struct Nfa<T> {
    // The transition table. Each state has a vector of transitions to other
    // states. Each state is just an index into this table, so we can represent
    // states in the Nfa as usize types.
    transitions: Vec<Vec<Transition<T>>>,
    start_state: Option<usize>,
    final_states: HashSet<usize>,
}

impl<T> Nfa<T>
where
    T: std::hash::Hash,
    T: std::cmp::Eq,
    T: std::cmp::PartialEq,
    T: std::clone::Clone,
{
    /// Creates a new empty Nfa
    pub fn new() -> Nfa<T> {
        Nfa {
            transitions: vec![],
            start_state: None,
            final_states: HashSet::new(),
        }
    }

    /// Inserts a new state into the Nfa and returns the state label
    pub fn new_state(&mut self) -> usize {
        self.transitions.push(vec![]);
        self.transitions.len() - 1
    }

    /// Mark a state as the start state. An NFA can only have one of these.
    pub fn set_start(&mut self, state: usize) {
        self.start_state = Some(state);
    }

    /// Mark a state as final. An NFA can have many of these.
    pub fn set_final(&mut self, state: usize) {
        self.final_states.insert(state);
    }

    /// Inserts a transition between two states into the `Nfa`. Epsilon
    /// transitions are represented as a `None` transition token.
    pub fn transition(
        &mut self,
        a: usize,
        b: usize,
        t: Option<T>,
    ) -> Result<(), String> {
        if b >= self.transitions.len() {
            return Err(format!("State {} does not exist in the NFA", b));
        }
        match self.transitions.get_mut(a) {
            Some(a) => Ok(a.push(Transition(t, b))),
            _ => Err(format!("State {} does not exist in the NFA", a)),
        }
    }

    /// Converts a valid NFA into a valid DFA. It is usually easier to build
    /// an NFA and convert it into a DFA than it is to build a DFA directly.
    /// Conversely, it is usually easier to use a DFA than it is to use an
    /// NFA directly. In other words you will normally want to build an NFA
    /// first and then convert it into a DFA to use using `into_dfa`.
    ///
    /// For specifics on how an NFA is converted to a DFA:
    /// https://en.wikipedia.org/wiki/Powerset_construction
    ///
    /// Panics if the NFA does not have a start state.
    pub fn into_dfa(&self) -> Dfa<T> {
        let mut dfa = Dfa::<T>::new();

        let start = self.start_state.expect("Nfa is missing a start state");

        let mut stack = vec![self.closure(start)];
        let mut dfa_map = vec![];

        // Build the DFA nodes
        while let Some(nfa_set) = stack.pop() {
            // Find a pre-existing DFA state corresponding to the same set
            // of NFA states. Otherwise create a new one.
            if dfa_map.iter().find(|(_, y, _)| *y == nfa_set).is_some() {
                continue;
            }

            let state = dfa.new_state();

            // Build arcs from the NFA state set to other NFA state sets. The
            // connected state sets will be pushed onto the stack to become
            // DFA states. Once all the DFA states are constructed, we can
            // go back over the arcs to connect them all.
            let mut arcs = HashMap::new();
            for Transition(token, next) in nfa_set
                .iter()
                .flat_map(|x| self.transitions.get(*x))
                .flatten()
            {
                if let Some(token) = token {
                    let closure = &self.closure(*next);
                    arcs.entry(token)
                        .or_insert_with(HashSet::new)
                        .extend(closure);
                }
            }

            for (_, set) in arcs.iter() {
                stack.push(HashSet::clone(set));
            }

            // The start state stays the same
            if nfa_set.contains(&start) {
                dfa.set_start(state);
            }

            // The final states stay the same
            if !self.final_states.is_disjoint(&nfa_set) {
                dfa.set_final(state);
            }

            dfa_map.push((state, nfa_set, arcs));
        }

        // Build the edges between the DFA nodes
        for (a, _, nfa_arcs) in dfa_map.iter() {
            for (t, nfa_set) in nfa_arcs {
                let b = dfa_map
                    .iter()
                    .find(|(_, y, _)| y == nfa_set)
                    .map(|(x, ..)| x)
                    .unwrap();
                dfa.transition(*a, *b, Clone::clone(t)).unwrap();
            }
        }

        dfa
    }

    /// Follows epsilon transitions using DFS to build a 'closure' of states
    /// that are all reachable from the start state using only epsilon
    /// transitions.
    fn closure(&self, state: usize) -> HashSet<usize> {
        let mut stack = vec![state];
        let mut states = HashSet::new();
        while let Some(state) = stack.pop() {
            if states.contains(&state) {
                continue;
            }
            states.insert(state);
            if let Some(transitions) = self.transitions.get(state) {
                for Transition(token, next) in transitions {
                    if token.is_none() {
                        stack.push(*next);
                    }
                }
            }
        }
        states
    }
}

impl<T> fmt::Display for Nfa<T>
where
    T: std::string::ToString,
    T: std::clone::Clone,
{
    // Formats the NFA using GraphViz notation https://graphviz.gitlab.io
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let final_states: Vec<String> =
            self.final_states.iter().map(|x| x.to_string()).collect();
        let final_states = final_states.join(" ");
        write!(f, "digraph nfa {{\n")?;
        write!(f, "\tnode [shape = doublecircle]; {};\n", final_states)?;
        write!(f, "\tnode [shape = circle];\n")?;
        for (i, transitions) in self.transitions.iter().enumerate() {
            for Transition(t, n) in transitions {
                let default = String::from("None");
                let t = Clone::clone(t).map_or(default, |x| x.to_string());
                write!(f, "\t{} -> {} [ label = \"{}\" ];\n", i, n, t)?;
            }
        }
        write!(f, "}}\n")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Dfa<T>
where
    T: std::hash::Hash,
    T: std::cmp::Eq,
    T: std::cmp::PartialEq,
    T: std::clone::Clone,
{
    // The transition table. Each state can map to any other state, but unlike
    // an NFA, DFAs cannot transition to multiple states using the same token.
    // They are also not allowed to have epsilon transitions.
    pub transitions: Vec<HashMap<T, usize>>,
    pub start_state: Option<usize>,
    pub final_states: HashSet<usize>,
}

impl<T> Dfa<T>
where
    T: std::hash::Hash,
    T: std::cmp::Eq,
    T: std::cmp::PartialEq,
    T: std::clone::Clone,
{
    /// Creates a new empty `Dfa`.
    pub fn new() -> Dfa<T> {
        Dfa {
            transitions: vec![],
            start_state: None,
            final_states: HashSet::new(),
        }
    }

    /// Inserts a new state in the Dfa and returns the state label.
    pub fn new_state(&mut self) -> usize {
        self.transitions.push(HashMap::new());
        self.transitions.len() - 1
    }

    /// Mark a state as the start state. A DFA can only have one of these.
    pub fn set_start(&mut self, state: usize) {
        self.start_state = Some(state);
    }

    /// Mark a state as final. A DFA can have many of these.
    pub fn set_final(&mut self, state: usize) {
        self.final_states.insert(state);
    }

    /// Inserts a transition between two states into the `Dfa`.
    pub fn transition(
        &mut self,
        a: usize,
        b: usize,
        t: T,
    ) -> Result<(), String> {
        if b >= self.transitions.len() {
            return Err(format!("State {} does not exist in the DFA", b));
        }
        match self.transitions.get_mut(a) {
            Some(a) => {
                a.insert(t, b);
                Ok(())
            }
            _ => Err(format!("State {} does not exist in the DFA", a)),
        }
    }

    /// Build a minimal equivalent DFA from this DFA. This implementation uses
    /// Brzozowski's algorithm due to its simplicity, but has a worst case
    /// exponential runtime. In general it will perform much better than this.
    pub fn minimize(&self) -> Dfa<T> {
        self.reverse().into_dfa().reverse().into_dfa()
    }

    /// Builds an NFA by reversing the edges of this DFA.
    fn reverse(&self) -> Nfa<T> {
        let mut nfa = Nfa::<T>::new();
        // Create an equal set of states in the NFA
        for _ in &self.transitions {
            nfa.new_state();
        }
        // Add edges in reverse to the NFA
        for (n, transitions) in self.transitions.iter().enumerate() {
            for (token, target) in transitions {
                nfa.transition(*target, n, Some(token.clone())).unwrap();
            }
        }
        // Swap the start and final states. Since we can only have one start
        // state, we create a proxy state with epsilon transitions to the
        // multiple start states in the reverse NFA.
        let proxy_state = nfa.new_state();
        nfa.set_start(proxy_state);
        for state in self.final_states.iter() {
            nfa.transition(proxy_state, *state, None).unwrap();
        }
        if let Some(final_state) = self.start_state {
            nfa.set_final(final_state);
        }

        nfa
    }
}

impl<T> fmt::Display for Dfa<T>
where
    T: std::fmt::Display,
    T: std::hash::Hash,
    T: std::cmp::Eq,
    T: std::cmp::PartialEq,
    T: std::clone::Clone,
{
    // Formats the NFA using GraphViz notation https://graphviz.gitlab.io
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let final_states: Vec<String> =
            self.final_states.iter().map(|x| x.to_string()).collect();
        let final_states = final_states.join(" ");
        write!(f, "digraph dfa {{\n")?;
        write!(f, "\tnode [shape = doublecircle]; {};\n", final_states)?;
        write!(f, "\tnode [shape = circle];\n")?;
        for (i, transitions) in self.transitions.iter().enumerate() {
            for (t, next) in transitions {
                write!(f, "\t{} -> {} [ label = \"{}\" ];\n", i, next, t)?;
            }
        }
        write!(f, "}}\n")
    }
}

#[cfg(test)]
mod tests {
    extern crate lexer;
    use super::*;
    use lexer::tokens::Token;
    use std::iter::FromIterator;

    #[test]
    fn it_builds_nfas() {
        let mut nfa = Nfa::new();
        let (a, b) = (nfa.new_state(), nfa.new_state());
        nfa.transition(a, b, Some(Token::LParen)).unwrap();
        nfa.set_start(a);
        nfa.set_final(b);

        assert_eq!(
            nfa.transitions,
            vec![vec![Transition(Some(Token::LParen), 1)], vec![]]
        );
        assert_eq!(nfa.start_state, Some(a));
        assert_eq!(nfa.final_states, HashSet::from_iter(vec![b]));
    }

    #[test]
    fn it_converts_nfas() {
        let mut nfa = Nfa::new();
        let (a, b, c, d) = (
            nfa.new_state(),
            nfa.new_state(),
            nfa.new_state(),
            nfa.new_state(),
        );
        nfa.transition(a, b, Some(Token::Name("0".into()))).unwrap();
        nfa.transition(b, b, Some(Token::Name("1".into()))).unwrap();
        nfa.transition(b, d, Some(Token::Name("1".into()))).unwrap();
        nfa.transition(d, c, Some(Token::Name("0".into()))).unwrap();
        nfa.transition(c, d, Some(Token::Name("0".into()))).unwrap();
        nfa.transition(a, c, None).unwrap();
        nfa.transition(c, b, None).unwrap();
        nfa.set_start(a);
        nfa.set_final(c);
        nfa.set_final(d);

        let actual = nfa.into_dfa();
        let mut expect = Dfa::new();
        let (abc, cd, bc, d) = (
            expect.new_state(),
            expect.new_state(),
            expect.new_state(),
            expect.new_state(),
        );
        expect.transition(abc, cd, Token::Name("0".into())).unwrap();
        expect.transition(abc, cd, Token::Name("1".into())).unwrap();
        expect.transition(cd, cd, Token::Name("1".into())).unwrap();
        expect.transition(cd, bc, Token::Name("0".into())).unwrap();
        expect.transition(bc, cd, Token::Name("1".into())).unwrap();
        expect.transition(bc, d, Token::Name("0".into())).unwrap();
        expect.transition(d, bc, Token::Name("0".into())).unwrap();
        expect.set_start(abc);
        expect.set_final(abc);
        expect.set_final(cd);
        expect.set_final(bc);
        expect.set_final(d);

        assert_eq!(expect, actual);
    }

    #[test]
    fn it_minimizes_dfas() {
        let mut dfa = Dfa::new();
        let (a, b, c, d) = (
            dfa.new_state(),
            dfa.new_state(),
            dfa.new_state(),
            dfa.new_state(),
        );

        dfa.transition(a, b, Token::Name("0".into())).unwrap();
        dfa.transition(b, d, Token::Name("1".into())).unwrap();
        dfa.transition(a, c, Token::Name("1".into())).unwrap();
        dfa.transition(c, d, Token::Name("1".into())).unwrap();
        dfa.set_start(a);
        dfa.set_final(d);

        let actual = dfa.minimize();
        let mut expect = Dfa::new();
        let (a, b, c) =
            (expect.new_state(), expect.new_state(), expect.new_state());
        expect.transition(a, b, Token::Name("0".into())).unwrap();
        expect.transition(a, b, Token::Name("1".into())).unwrap();
        expect.transition(b, c, Token::Name("1".into())).unwrap();
        expect.set_start(a);
        expect.set_final(c);

        assert_eq!(expect, actual);
    }
}
