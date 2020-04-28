use std::cell::RefCell;
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
        parent: Rc<RefCell<Env<V>>>
    }
}

pub type EnvWrapper<E> = Rc<RefCell<E>>;

impl<V> Env<V> 
where V: EnvVal {
    pub fn empty() -> EnvWrapper<Self> {
        Rc::new(RefCell::new(Env::Empty))
    }
    
    pub fn associate(pattern: Match, value: V, parent: &EnvWrapper<Self>) -> Result<EnvWrapper<Self>, String> {
        let parent = Rc::clone(parent);
        value.unwrap_matches(&pattern).map(
            |matches|
            matches.into_iter().fold(
                parent,
                |parent, (ident, value)|
                    Env::associate_ident(ident, value, parent),
            )
        )
    }
    
    pub fn associate_ident(ident: String, value: V, parent: EnvWrapper<Self>) -> EnvWrapper<Self> {
        Rc::new(RefCell::new(Env::Entry { ident, value, parent }))
    }
    
    pub fn get(env: &EnvWrapper<Self>, ident: &str) -> Option<V> {
        match *env.borrow() {
            Env::Empty => None,
            Env::Entry { ident: ref id, ref value, ref parent } =>
                if ident == id {
                    Some(value.clone())
                } else {
                    Env::get(parent, ident)
                }
        }
    }
    
    // Only should be used with declarations to solve recursive problem
    // I strongly dislike the fact that I have to do this
    // Also, write tests!!!
    // This needs to work!!
    pub fn set_value(env: &EnvWrapper<Self>, value: V) {
        let mut inner_env = env.borrow_mut();
        let new_inner_env = match *inner_env {
            Env::Empty => Env::Empty,
            Env::Entry {ref ident, ref parent, ..} =>
                Env::Entry {
                    ident: ident.to_string(),
                    value,
                    parent: Rc::clone(parent)
                }
        };
        *inner_env = new_inner_env;
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
        Env::get(&Env::associate(
            Match::ident("a"),
            DummyValue::int("a", 1),
            &Env::empty()
        ).unwrap(), "a") => Some(DummyValue::Int(1));
        Env::get(&Env::associate(
            Match::ident("a"),
            DummyValue::int("a", 1),
            &Env::empty()
        ).unwrap(), "b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::get(&Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap(), "a") => Some(DummyValue::Int(1));
        Env::get(&Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap(), "b") => Some(DummyValue::Int(2));
        Env::get(&Env::associate(Match::ident("a"), DummyValue::int("a", 1),
            &Env::associate(Match::ident("b"), DummyValue::int("b", 2), &Env::empty()).unwrap())
            .unwrap(), "c") => None
    }
    
    basic_test! {
        with_tuple_test
        Env::get(&Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            DummyValue::new(vec![
                ("a", 1),
                ("b", 2)
            ]),
            &Env::empty()
        ).unwrap(), "a") => Some(DummyValue::Int(1));
        Env::get(&Env::associate(
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            ),
            DummyValue::new(vec![
                ("a", 1),
                ("b", 2)
            ]),
            &Env::empty()
        ).unwrap(), "b") => Some(DummyValue::Int(2))
    }
    
    #[test]
    fn setter_test() {
        let expected = Some(DummyValue::Int(2));
        let env = Env::associate_ident(
                "a".to_string(),
                DummyValue::Int(1),
                Env::empty()
        );
        Env::set_value(&env, DummyValue::Int(2));
        let actual = Env::get(&env, "a");
        
        assert_eq!(expected, actual);
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