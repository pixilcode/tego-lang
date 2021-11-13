use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Char,
    Tuple(Vec<Type>),
    Fn_,
    Boxed(Box<Type>),
    Command,
    Error,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Int => "Int".into(),
                Type::Bool => "Bool".into(),
                Type::Char => "Char".into(),
                Type::Tuple(types) => {
                    if types.iter().all(|t| matches!(t, Type::Char)) {
                        "String".into()
                    } else {
                        let result = types
                            .iter()
                            .map(|t| format!("{}", t))
                            .fold(String::new(), |a, s| a + &s + ", ");
                        let result = if result.len() >= 2 {
                            &result[..result.len() - 2] // Get rid of the last ", "
                        } else {
                            &result
                        };
                        format!("({})", result)
                    }
                }
                Type::Fn_ => "Fn".into(),
                Type::Boxed(type_) => format!("Boxed<{}>", type_),
                Type::Command => "Command".into(),
                Type::Error => "Error".into(),
            }
        )
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
                Type::Bool
            ])
        ) => "(Int, Bool)"
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
