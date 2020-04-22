use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Tuple(Vec<Type>),
    Fn_,
    Error
}

impl Type {
    pub fn unit() -> Self {
        Type::Tuple(vec![])
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
            match self {
                Type::Int => "Int".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::Tuple(types) => {
                    let result = types.iter()
                        .map(|t| format!("{}", t))
                        .fold(String::new(), |a, s| a + &s + ", ");
                    let result = &result[..result.len()-2]; // Get rid of the last ", "
                    format!("({})", result)
                },
                Type::Fn_ => "Fn".to_string(),
                Type::Error => "Error".to_string()
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    basic_test!(
        int
        &format!("{}", Type::Int) => "Int"
    );
    
    basic_test!(
        boolean
        &format!("{}", Type::Bool) => "Bool"
    );
    
    basic_test!(
        tuple
        &format!("{}", Type::Tuple(vec![
                Type::Int,
                Type::unit(),
                Type::Bool
            ])
        ) => "(Int, (), Bool)"
    );
    
    basic_test!(
        fn_
        &format!("{}", Type::Fn_) => "Fn"
    );
    
    basic_test!(
        error
        &format!("{}", Type::Error) => "Error"
    );
}