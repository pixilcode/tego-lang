use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            match self {
                Type::Int => "Int",
                Type::Bool => "Bool",
                Type::Unit => "Unit",
                Type::Error => "Error"
            })
    }
}