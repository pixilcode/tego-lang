use std::fmt::Debug;
use std::rc::Rc;
use crate::ast::Match;

#[derive(Debug)]
pub enum Env<V>
where V: Clone + Debug {
    Empty,
    Entry {
        ident: Match,
        value: V,
        parent: Rc<Env<V>>
    }
}

impl<V> Env<V> 
where V: Clone + Debug {
    pub fn empty() -> Rc<Self> {
        Rc::new(Env::Empty)
    }
    
    pub fn associate(ident: Match, value: V, parent: &Rc<Env<V>>) -> Rc<Self> {
        let parent = Rc::clone(parent);
        Rc::new(Env::Entry { ident, value, parent })
    }
    
    pub fn get(&self, ident: &str) -> Option<V> {
        match self {
            Env::Empty => None,
            Env::Entry { ident: id, value, parent } =>
                match id {
                    Match::Ident(id) =>
                        if ident == id {
                            Some(value.clone())
                        } else {
                            parent.get(ident)
                        }
                }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    basic_test! {
        no_parent_test
        Env::associate(Match::ident("a"), 1, &Env::empty()).get("a") => Some(1);
        Env::associate(Match::ident("a"), 1, &Env::empty()).get("b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::associate(Match::ident("a"), 1,
            &Env::associate(Match::ident("b"), 2, &Env::empty()))
            .get("a") => Some(1);
        Env::associate(Match::ident("a"), 1,
            &Env::associate(Match::ident("b"), 2, &Env::empty()))
            .get("b") => Some(2);
        Env::associate(Match::ident("a"), 1,
            &Env::associate(Match::ident("b"), 2, &Env::empty()))
            .get("c") => None
    }
}