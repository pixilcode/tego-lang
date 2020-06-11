use std::fmt;
use crate::MatchOutput;

#[derive(Debug, PartialEq, Clone)]
pub enum Match {
    Ident(String),
    Tuple(Vec<Match>),
    Boxed(Box<Match>),
    Value(MatchVal),
    Unit, // Special value for matching '()' (this has special behavior)
    Ignore,
}

impl MatchOutput for Match {
	fn tuple(a: Self, b: Self) -> Self {
        match (a, b) {
            (Match::Tuple(mut a), Match::Tuple(mut b)) => {
                a.append(&mut b);
                Match::Tuple(a)
            }
            (Match::Tuple(mut a), b) => {
                a.push(b);
                Match::Tuple(a)
            }
            (a, Match::Tuple(mut b)) => {
                b.insert(0, a);
                Match::Tuple(b)
            }
            (a, b) => Match::Tuple(vec![a, b]),
        }
    }

	fn unit() -> Self {
        Match::Unit
    }

	fn boxed(a: Self) -> Self {
        Match::Boxed(Box::new(a))
    }

	fn ignore() -> Self {
        Match::Ignore
    }

	fn bool(b: bool) -> Self {
        Match::Value(MatchVal::Bool(b))
    }

	fn int(i: i32) -> Self {
        Match::Value(MatchVal::Int(i))
    }

	fn ident(lexeme: &str) -> Match {
        Match::Ident(lexeme.into())
    }

	fn string(s: &str) -> Self {
        Match::Boxed(Box::new(Match::Value(MatchVal::String(s.into()))))
    }

	fn char(c: char) -> Self {
        Match::Value(MatchVal::Char(c))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchVal {
    Int(i32),
    Bool(bool),
    Char(char),
    String(String)
}

impl fmt::Display for MatchVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                MatchVal::Int(i) => write!(f, "{}", i),
                MatchVal::Bool(b) => write!(f, "{}", b),
                MatchVal::Char(c) => write!(f, "'{}'", c),
                MatchVal::String(s) => write!(f, "\"{}\"", s)
            }
    }
}
