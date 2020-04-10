use crate::ast::Expr;
use crate::type_::Type;
use crate::environment::Env;

type TypeEnv = Env<Type>;

pub fn check_expr(expr: &Expr, expected: &Type, env: &TypeEnv) -> Result<(), String> {
    match expr {
        Expr::Literal(val) =>
            if val.is_type(expected) {
                Ok(())
            } else {
                type_error(expected, &val.type_())
            }
        Expr::Binary(a, _, b) =>
            check_expr(a, expected, env)
                .and_then(|_| check_expr(b, expected, env)),
        Expr::Unary(_, a) =>
            check_expr(a, expected, env), // TODO Write tests!!!
        Expr::If(cond, t, f) =>
            check_expr(cond, &Type::Bool, env)
                .and_then(|_| check_expr(t, expected, env))
                .and_then(|_| check_expr(f, expected, env)),
        Expr::Variable(ident) =>
            match env.get(ident) {
                Some(type_) => if type_ == expected {
                    Ok(())
                } else {
                    type_error(expected, type_)
                },
                None => Err(format!("Variable '{}' is not declared", ident))
            }
    }
}

fn type_error(expected: &Type, actual: &Type) -> Result<(), String> {
    Err(format!("Expected type '{}', got type '{}'", expected, actual))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, UnaryOp};
    
    #[test]
    fn literal_check() {
        let expected = Ok(());
        let actual = check_expr(&Expr::int(0), &Type::Int, &Env::empty());
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(&Expr::unit(), &Type::Int, &Env::empty());
        assert_eq!(expected, actual);
    }
    
    #[test]
    fn binary_check() {
        let expected = Ok(());
        let actual = check_expr(
            &Expr::binary(
                Expr::int(0),
                BinaryOp::Plus,
                Expr::int(0)
            ),
            &Type::Int,
            &Env::empty()
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::unit(),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int,
            &Env::empty()
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::int(0),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int,
            &Env::empty()
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::unit(),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int,
            &Env::empty()
        );
        assert_eq!(expected, actual);
    }
    
    #[test]
    fn unary_check() {
        let expected = Ok(());
        let actual = check_expr(
            &Expr::unary(
                UnaryOp::Not,
                Expr::bool(true)
            ),
            &Type::Bool,
            &Env::empty()
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Bool, Type::Unit);
        let actual = check_expr(
            &Expr::unary(
                UnaryOp::Not,
                Expr::unit()
            ),
            &Type::Bool,
            &Env::empty()
        );
        assert_eq!(expected, actual);
    }
    
    basic_test! {
        variable_check
        check_expr(
            &Expr::variable("a"),
            &Type::Int,
            &TypeEnv::associate("a", Type::Int, Env::empty())
        ) => Ok(());
        check_expr(
            &Expr::variable("a"),
            &Type::Bool,
            &TypeEnv::associate("a", Type::Int, Env::empty())
        ) => error(Type::Bool, Type::Int);
        check_expr(
            &Expr::variable("b"),
            &Type::Int,
            &TypeEnv::associate("a", Type::Int, Env::empty())
        ) => Err(format!("Variable '{}' is not declared", "b"))
    }
        
    fn error(expected: Type, actual: Type) -> Result<(), String> {
        Err(format!("Expected type '{}', got type '{}'",
            expected,
            actual
        ))
    }
}