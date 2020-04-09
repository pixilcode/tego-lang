use std::rc::Rc;

pub struct Env<V> {
    ident: String,
    value: V,
    parent: Option<Rc<Env<V>>>
}

impl<V> Env<V> {
    fn associate(ident: &str, value: V, parent: Option<Env<V>>) -> Self {
        let ident = ident.to_string();
        let parent = parent.map(Rc::new);
        Env { ident, value, parent }
    }
    
    pub fn get(&self, ident: &str) -> Option<&V> {
        if ident == self.ident {
            Some(&self.value)
        } else {
            match &self.parent {
                Some(env) => env.get(ident),
                None => None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    basic_test! {
        no_parent_test
        Env::associate("a", 1, None).get("a") => Some(&1);
        Env::associate("a", 1, None).get("b") => None
    }
    
    basic_test! {
        with_parent_test
        Env::associate("a", 1,
            Some(Env::associate("b", 2, None)))
            .get("a") => Some(&1);
        Env::associate("a", 1,
            Some(Env::associate("b", 2, None)))
            .get("b") => Some(&2);
        Env::associate("a", 1,
            Some(Env::associate("b", 2, None)))
            .get("c") => None
    }
}