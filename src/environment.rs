use std::rc::Rc;

pub enum Env<V> {
    Empty,
    Entry {
        ident: String,
        value: V,
        parent: Rc<Env<V>>
    }
}

impl<V> Env<V> {
    pub fn empty() -> Self {
        Env::Empty
    }
    
    pub fn associate(ident: &str, value: V, parent: Env<V>) -> Self {
        let ident = ident.to_string();
        let parent = Rc::new(parent);
        Env::Entry { ident, value, parent }
    }
    
    pub fn get(&self, ident: &str) -> Option<&V> {
        match self {
            Env::Empty => None,
            Env::Entry { ident: id, value, parent } =>
                if ident == id {
                    Some(value)
                } else {
                    parent.get(ident)
                }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    basic_test! {
        no_parent_test
        Env::associate("a", 1, Env::empty()).get("a") => Some(&1);
        Env::associate("a", 1, Env::empty()).get("b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::associate("a", 1,
            Env::associate("b", 2, Env::empty()))
            .get("a") => Some(&1);
        Env::associate("a", 1,
            Env::associate("b", 2, Env::empty()))
            .get("b") => Some(&2);
        Env::associate("a", 1,
            Env::associate("b", 2, Env::empty()))
            .get("c") => None
    }
}