use tego_lang::ast::Expr;
use tego_lang::value::Value;

pub fn eval_expr(expr: Expr) -> Value {
    match expr {
        Expr::Unary(op, a) => op.eval(eval_expr(*a)),
        Expr::Binary(a, op, b) => op.eval(eval_expr(*a), eval_expr(*b)),
        Expr::Literal(val) => val,
        Expr::If(cond, a, b) => match eval_expr(*cond) {
            Value::Bool(true) => eval_expr(*a),
            Value::Bool(false) => eval_expr(*b),
            _ => error("If condition must return a boolean")
        }
    }
}

fn error(message: &str) -> Value {
    Value::Error(message.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tego_lang::ast::{BinaryOp, UnaryOp};
    
    #[test]
    fn eval_literal() {
        let expected = Value::Int(1);
        let actual = eval_expr(Expr::int(1));
        assert_eq!(expected, actual);
    }
    
    #[test]
    fn eval_binary() {
        let expected = Value::Int(3);
        let actual = eval_expr(
            Expr::binary(
                Expr::int(1),
                BinaryOp::Plus,
                Expr::int(2)
            )
        );
        assert_eq!(expected, actual);
    }
    
    #[test]
    fn eval_unary() {
        let expected = Value::Int(-3);
        let actual = eval_expr(
            Expr::unary(
                UnaryOp::Negate,
                Expr::int(3)
            )
        );
        assert_eq!(expected, actual);
    }
    
    #[test]
    fn eval_if_expr() {
        let expected = Value::Int(1);
        let actual = eval_expr(
            Expr::if_expr(
                Expr::bool(true),
                Expr::int(1),
                Expr::int(2)
            )
        );
        assert_eq!(expected, actual);
        
        let expected = Value::Int(2);
        let actual = eval_expr(
            Expr::if_expr(
                Expr::bool(false),
                Expr::int(1),
                Expr::int(2)
            )
        );
        assert_eq!(expected, actual);
    }
}