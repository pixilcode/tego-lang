use std::fmt::Debug;
use std::rc::Rc;
use crate::ast::Match;

#[derive(Debug)]
pub enum Env<V>
where V: EnvVal {
    Empty,
    Entry {
        ident: String,
        value: V,
        parent: Rc<Env<V>>
    }
}

impl<V> Env<V> 
where V: EnvVal {
    pub fn empty() -> Rc<Self> {
        Rc::new(Env::Empty)
    }
    
    pub fn associate(pattern: Match, value: V, parent: &Rc<Env<V>>) -> Result<Rc<Self>, String> {
        let parent = Rc::clone(parent);
        value.unwrap_matches(&pattern).map(
            |matches|
            matches.into_iter().fold(
                parent,
                |parent, (ident, value)|
                    Rc::new(Env::Entry { ident, value, parent }),
            )
        )
    }
    
    pub fn get(&self, ident: &str) -> Option<V> {
        match self {
            Env::Empty => None,
            Env::Entry { ident: id, value, parent } =>
                if ident == id {
                    Some(value.clone())
                } else {
                    parent.get(ident)
                }
        }
    }
}

pub trait EnvVal: Clone + Debug {
    fn unwrap_matches(&self, pattern: &Match) -> Result<Vec<(String, Self)>, String>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;
    
    basic_test! {
        no_parent_test
        Env::associate(
            Match::ident("a"),
            Value::Int(1),
            &Env::empty()
        ).unwrap().get("a") => Some(Value::Int(1));
        Env::associate(
            Match::ident("a"),
            Value::Int(1),
            &Env::empty()
        ).unwrap().get("b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::associate(Match::ident("a"), Value::Int(1),
            &Env::associate(Match::ident("b"), Value::Int(2), &Env::empty()).unwrap())
            .unwrap().get("a") => Some(Value::Int(1));
        Env::associate(Match::ident("a"), Value::Int(1),
            &Env::associate(Match::ident("b"), Value::Int(2), &Env::empty()).unwrap())
            .unwrap().get("b") => Some(Value::Int(2));
        Env::associate(Match::ident("a"), Value::Int(1),
            &Env::associate(Match::ident("b"), Value::Int(2), &Env::empty()).unwrap())
            .unwrap().get("c") => None
    }
    
    basic_test! {
        with_tuple_test
        Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            Value::Tuple(vec![
                Value::Int(1),
                Value::Int(2)
            ]),
            &Env::empty()
        ).unwrap().get("a") => Some(Value::Int(1));
        Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            Value::Tuple(vec![
                Value::Int(1),
                Value::Int(2)
            ]),
            &Env::empty()
        ).unwrap().get("b") => Some(Value::Int(2))
    }
}