use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Match {
    Ident(String),
    Tuple(Vec<Match>),
    Value(MatchVal),
    Ignore,
}

impl Match {
    pub fn ident(lexeme: &str) -> Match {
        Match::Ident(lexeme.into())
    }

    pub fn tuple(a: Match, b: Match) -> Match {
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

    pub fn int(i: i32) -> Self {
        Match::Value(MatchVal::Int(i))
    }

    pub fn bool(b: bool) -> Self {
        Match::Value(MatchVal::Bool(b))
    }

    pub fn unit() -> Self {
        Match::Tuple(vec![])
    }

    pub fn ignore() -> Self {
        Match::Ignore
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchVal {
    Int(i32),
    Bool(bool),
}

impl fmt::Display for MatchVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MatchVal::Int(i) => i32::to_string(i),
                MatchVal::Bool(b) => bool::to_string(b),
            }
        )
    }
}
