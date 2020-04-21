use crate::ast::Expr;
use crate::environment::Env;
use crate::interpreter::value::Value;
use std::rc::Rc;

pub mod value;

type VarEnv = Env<Value>;

pub fn new_env() -> Rc<VarEnv> {
    VarEnv::empty()
}

pub fn eval_expr(expr: Expr, env: &Rc<VarEnv>) -> Value {
    match expr {
        Expr::Unary(op, a) => op.eval(eval_expr(*a, env)),
        Expr::Binary(a, op, b) =>
            op.eval(
                eval_expr(*a, env),
                eval_expr(*b, env)
            ),
        Expr::Literal(val) => val,
        Expr::If(cond, a, b) => match eval_expr(*cond, env) {
            Value::Bool(true) => eval_expr(*a, env),
            Value::Bool(false) => eval_expr(*b, env),
            _ => error("If condition must return a boolean")
        },
        Expr::Variable(ident) => match env.get(&ident) {
            Some(val) => val.clone(),
            None =>
                error(&format!("Variable '{}' is not declared", ident))
        },
        Expr::Let(ident, value, inner) =>
            match VarEnv::associate(
                ident,
                eval_expr(*value, env),
                env
            ) {
                Ok(env) => eval_expr(*inner, &env),
                Err(error) => Value::Error(error)
            },
        Expr::Fn_(param, body) =>
            Value::fn_(param, body, Rc::clone(env)),
        Expr::FnApp(function, arg) => {
                let function = eval_expr(*function, env);
                match function {
                    Value::Fn_(param, body, fn_env) =>
                        match VarEnv::associate(
                            param,
                            eval_expr(*arg, env),
                            &fn_env
                        ) {
                            Ok(fn_env) => eval_expr(*body, &fn_env),
                            Err(error) => Value::Error(error),
                        },
                    _ => error(&format!(
                            "Can't apply argument to type '{}'",
                            function.type_()
                         ))
                }
            }
    }
}

fn error(message: &str) -> Value {
    Value::Error(message.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOp, UnaryOp, Match};
    
    #[test]
    fn eval_literal() {
        let expected = Value::Int(1);
        let actual = eval_expr(Expr::int(1), &VarEnv::empty());
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
            ),
            &VarEnv::empty()
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
            ),
            &VarEnv::empty()
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
            ),
            &VarEnv::empty()
        );
        assert_eq!(expected, actual);
        
        let expected = Value::Int(2);
        let actual = eval_expr(
            Expr::if_expr(
                Expr::bool(false),
                Expr::int(1),
                Expr::int(2)
            ),
            &VarEnv::empty()
        );
        assert_eq!(expected, actual);
    }
    
    basic_test! {
        eval_variable
        eval_expr(
            Expr::variable("a"),
            &VarEnv::associate(
                Match::ident("a"),
                Value::Int(1),
                &VarEnv::empty()
            ).unwrap()
        ) => Value::Int(1);
        eval_expr(
            Expr::variable("b"),
            &VarEnv::associate(
                Match::ident("a"),
                Value::Int(1),
                &VarEnv::empty()
            ).unwrap()
        ) => Value::Error("Variable 'b' is not declared".to_string())
    }
    
    basic_test! {
        eval_let_expr
        eval_expr(
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::int(2)
            ),
            &VarEnv::empty()
        ) => Value::Int(2);
        eval_expr(
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::variable("a")
            ),
            &VarEnv::empty()
        ) => Value::Int(1)
    }
    
    basic_test! {
        eval_fn_application
        eval_expr(
            Expr::fn_app(
                Expr::fn_expr(
                    Match::ident("a"),
                    Expr::variable("a")
                ),
                Expr::int(1)
            ),
            &VarEnv::empty()
        ) => Value::Int(1)
    }
}