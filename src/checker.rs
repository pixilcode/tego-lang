use crate::ast::Expr;
use crate::type_::Type;

pub fn check_expr(expr: &Expr, expected: &Type) -> Result<(), String> {
    match expr {
        Expr::Literal(val) =>
            if val.is_type(expected) {
                Ok(())
            } else {
                Err(format!("Expected type '{}', got type '{}'", expected, val.type_()))
            }
        Expr::Binary(a, _, b) =>
            check_expr(a, expected)
                .and_then(|_| check_expr(b, expected)),
        Expr::Unary(_, a) =>
            check_expr(a, expected), // TODO Write tests!!!
        Expr::If(cond, t, f) =>
            check_expr(cond, &Type::Bool)
                .and_then(|_| check_expr(t, expected))
                .and_then(|_| check_expr(f, expected)),
        Expr::Variable(ident) =>
            unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, UnaryOp};
    
    #[test]
    fn literal_check() {
        let expected = Ok(());
        let actual = check_expr(&Expr::int(0), &Type::Int);
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(&Expr::unit(), &Type::Int);
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
            &Type::Int
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::unit(),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::int(0),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Int, Type::Unit);
        let actual = check_expr(
            &Expr::binary(
                Expr::unit(),
                BinaryOp::Plus,
                Expr::unit()
            ),
            &Type::Int
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
            &Type::Bool
        );
        assert_eq!(expected, actual);
        
        let expected = error(Type::Bool, Type::Unit);
        let actual = check_expr(
            &Expr::unary(
                UnaryOp::Not,
                Expr::unit()
            ),
            &Type::Bool
        );
        assert_eq!(expected, actual);
    }
    
    fn error(expected: Type, actual: Type) -> Result<(), String> {
        Err(format!("Expected type '{}', got type '{}'",
            expected,
            actual
        ))
    }
}