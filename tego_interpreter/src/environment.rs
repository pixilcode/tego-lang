use tego_parser::ast::Match;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Env<V>
where
    V: EnvVal,
{
    Empty,
    Entry {
        ident: String,
        value: V,
        parent: Rc<RefCell<Env<V>>>,
    },
}

pub type EnvWrapper<E> = Rc<RefCell<E>>;

impl<V> Env<V>
where
    V: EnvVal,
{
    pub fn empty() -> EnvWrapper<Self> {
        Rc::new(RefCell::new(Env::Empty))
    }

    pub fn associate(
        pattern: Match,
        value: V,
        parent: &EnvWrapper<Self>,
    ) -> Result<EnvWrapper<Self>, String> {
        let parent = Rc::clone(parent);
        value.unwrap_matches(&pattern).map(|matches| {
            matches.into_iter().fold(parent, |parent, (ident, value)| {
                Env::associate_ident(ident, value, parent)
            })
        })
    }
    pub fn associate_ident(ident: String, value: V, parent: EnvWrapper<Self>) -> EnvWrapper<Self> {
        Rc::new(RefCell::new(Env::Entry {
            ident,
            value,
            parent,
        }))
    }
    pub fn get(env: &EnvWrapper<Self>, ident: &str) -> Option<V> {
        match *env.borrow() {
            Env::Empty => None,
            Env::Entry {
                ident: ref id,
                ref value,
                ref parent,
            } => {
                if ident == id {
                    Some(value.clone())
                } else {
                    Env::get(parent, ident)
                }
            }
        }
    }
    pub fn get_evaluated_value(env: &EnvWrapper<Self>) -> Result<V, String> {
        match *env.borrow() {
            Env::Empty => Err("No variables are declared".into()),
            Env::Entry { ref value, .. } if value.is_evaluated() => Ok(value.clone()),
            Env::Entry { .. } => Err("Variable is not evaluated".into()),
        }
    }
    pub fn with_parent(env: &EnvWrapper<Self>, env_parent: &EnvWrapper<Self>) -> EnvWrapper<Self> {
        match *env.borrow() {
            Env::Empty => Rc::clone(env_parent),
            Env::Entry {
                ref ident,
                ref value,
                ref parent,
            } => Rc::new(RefCell::new(Env::Entry {
                ident: ident.into(),
                value: value.clone(),
                parent: Env::with_parent(parent, env_parent),
            })),
        }
    }
    // Only should be used with declarations and delayed values to solve recursive problem
    // Also used with evaluation of lazy values
    // I strongly dislike the fact that I have to do this
    pub fn set_value(env: &EnvWrapper<Self>, value: V) {
        let mut inner_env = env.borrow_mut();
        let new_inner_env = match *inner_env {
            Env::Empty => Env::Empty,
            Env::Entry {
                ref ident,
                ref parent,
                ..
            } => Env::Entry {
                ident: ident.into(),
                value,
                parent: Rc::clone(parent),
            },
        };
        *inner_env = new_inner_env;
    }
}

pub trait EnvVal: Clone + Debug {
    fn unwrap_matches(&self, pattern: &Match) -> Result<Vec<(String, Self)>, String>;
    fn is_evaluated(&self) -> bool;
}

#[cfg(test)]
mod tests {
    use super::*;
    use tego_parser::MatchOutput;

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
        let env = Env::associate_ident("a".into(), DummyValue::Int(1), Env::empty());
        Env::set_value(&env, DummyValue::Int(2));
        let actual = Env::get(&env, "a");
        assert_eq!(expected, actual);
    }
    basic_test! {
        get_value_test
        Env::<DummyValue>::get_evaluated_value(&Env::empty())
            => Err("No variables are declared".into());
        Env::get_evaluated_value(&Env::associate_ident(
            "a".into(),
            DummyValue::Int(1),
            Env::empty()
        )) => Ok(DummyValue::Int(1));
        Env::get_evaluated_value(&Env::associate_ident(
            "a".into(),
            DummyValue::Delayed(1),
            Env::empty()
        )) => Err("Variable is not evaluated".into())
    }
    // Basically checking the (Haskell) monoidal properties
    // of `Env`, with `EnvWrapper<Env>` as the monoid,
    // `Env::with_parent` as the `++` operator,
    // and `Env::empty` as `unit`
    basic_test! {
        with_parent_fn_test
        Env::with_parent(&Env::empty(), &Env::empty()) =>
            Env::<DummyValue>::empty();
        Env::with_parent(
            &Env::empty(),
            &Env::associate_ident(
                "a".into(),
                DummyValue::Int(1),
                Env::empty()
            )
        ) => Env::associate_ident(
            "a".into(),
            DummyValue::Int(1),
            Env::empty()
        );
        Env::with_parent(
            &Env::associate_ident(
                "a".into(),
                DummyValue::Int(1),
                Env::empty()
            ),
            &Env::empty()
        ) => Env::associate_ident(
            "a".into(),
            DummyValue::Int(1),
            Env::empty()
        );
        Env::with_parent(
            &Env::associate_ident(
                "a".into(),
                DummyValue::Int(1),
                Env::empty()
            ),
            &Env::associate_ident(
                "b".into(),
                DummyValue::Int(2),
                Env::empty()
            )
        ) => Env::associate_ident(
            "a".into(),
            DummyValue::Int(1),
            Env::associate_ident(
                "b".into(),
                DummyValue::Int(2),
                Env::empty()
            )
        )
    }
    #[derive(Debug, Clone, PartialEq)]
    enum DummyValue {
        Int(u32),
        Container(Vec<(String, Self)>),
        Delayed(u32),
    }
    impl DummyValue {
        fn new(contains: Vec<(&str, u32)>) -> Self {
            DummyValue::Container(
                contains
                    .into_iter()
                    .map(|(ident, val)| (ident.into(), DummyValue::Int(val)))
                    .collect(),
            )
        }
        fn int(ident: &str, val: u32) -> Self {
            DummyValue::Container(vec![(ident.into(), DummyValue::Int(val))])
        }
    }
    impl EnvVal for DummyValue {
        fn unwrap_matches(&self, _pattern: &Match) -> Result<Vec<(String, Self)>, String> {
            match self {
                DummyValue::Container(a) => Ok(a.clone()),
                DummyValue::Int(_) => Err("Int value can't be unwrapped!".into()),
                DummyValue::Delayed(_) => Err("Int value can't be unwrapped!".into()),
            }
        }
        fn is_evaluated(&self) -> bool {
            match self {
                DummyValue::Delayed(_) => false,
                _ => true,
            }
        }
    }
}
