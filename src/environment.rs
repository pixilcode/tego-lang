use std::fmt::Debug;
use std::rc::Rc;
use crate::ast::match_::Match;

#[derive(Debug, PartialEq, Clone)]
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
    
    basic_test! {
        no_parent_test
        Env::associate(
            Match::ident("a"),
            DummyValue::int("a", 1),
            &Env::empty()
        ).unwrap().get("a") => Some(DummyValue::Int(1));
        Env::associate(
            Match::ident("a"),
            DummyValue::int("a", 1),
            &Env::empty()
        ).unwrap().get("b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap().get("a") => Some(DummyValue::Int(1));
        Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap().get("b") => Some(DummyValue::Int(2));
        Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap().get("c") => None
    }
    
    basic_test! {
        with_tuple_test
        Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            DummyValue::new(vec![
                ("a", 1),
                ("b", 2)
            ]),
            &Env::empty()
        ).unwrap().get("a") => Some(DummyValue::Int(1));
        Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            DummyValue::new(vec![
                ("a", 1),
                ("b", 2)
            ]),
            &Env::empty()
        ).unwrap().get("b") => Some(DummyValue::Int(2))
    }
    
    #[derive(Debug, Clone, PartialEq)]
    enum DummyValue {
        Int(u32),
        Container(Vec<(String, Self)>)
    }
    
    impl DummyValue {
        fn new(contains: Vec<(&str, u32)>) -> Self {
            DummyValue::Container(
                contains.into_iter()
                .map(
                    |(ident, val)|
                    (ident.to_string(), DummyValue::Int(val))
                ).collect()
            )
        }
        
        fn int(ident: &str, val: u32) -> Self {
            DummyValue::Container(vec![(ident.to_string(), DummyValue::Int(val))])
        }
    }
    
    impl EnvVal for DummyValue {
        fn unwrap_matches(&self, _pattern: &Match) -> Result<Vec<(String, Self)>, String> {
            match self {
                DummyValue::Container(a) => Ok(a.clone()),
                DummyValue::Int(_) => Err("Int value can't be unwrapped!".to_string())
            }
        }
    }
}