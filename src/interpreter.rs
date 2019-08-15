use jest_lang::ast::Expr;
use jest_lang::value::Value;

pub fn eval_expr(expr: Expr) -> Value {
    match expr {
        Expr::Unary(op, a) => op.eval(eval_expr(*a)),
        Expr::Binary(a, op, b) => op.eval(eval_expr(*a), eval_expr(*b)),
        Expr::Literal(val) => val
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jest_lang::ast::{BinaryOp, UnaryOp};
    
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
}