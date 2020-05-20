use crate::ast::{Decl, Expr, Prog};
use crate::execute::environment::{Env, EnvWrapper};
use crate::execute::value::Value;
use std::rc::Rc;

pub type VarEnv<'a> = Env<'a, Value<'a>>;
pub type WrappedEnv<'a> = EnvWrapper<VarEnv<'a>>;

pub fn run_prog<'a>(prog: Prog<'a>) -> Result<Value<'a>, String> {
    match prog {
        Prog::Binary(main, decls) => Ok(eval_expr(main, &env_from_decls(&decls))),
        Prog::Library(_) => Err("No 'main' found in file".into()),
    }
}

pub fn new_env<'a>() -> WrappedEnv<'a> {
    VarEnv::empty()
}

pub fn env_from_decls<'a>(decls: &'a [Decl<'a>]) -> WrappedEnv<'a> {
    let (env, decl_ptrs) = unfilled_env(decls);
    fill_decl_env(decls, &decl_ptrs, env)
}

fn unfilled_env<'a>(decls: &'a [Decl<'a>]) -> (WrappedEnv<'a>, Vec<WrappedEnv<'a>>) {
    decls
        .iter()
        .map(|decl| match decl {
            Decl::Expression(ident, _) => (
                ident,
                Value::Error(format!("'{}' has not been initialized", ident)),
            ),
        })
        .fold(
            (new_env(), Vec::with_capacity(decls.len())),
            |(parent, mut decl_ptrs), (ident, val)| {
                let new_env = Env::associate_ident(ident, val, parent);
                decl_ptrs.push(Rc::clone(&new_env));
                (new_env, decl_ptrs)
            },
        )
}

fn fill_decl_env<'a>(
    decls: &[Decl<'a>],
    decl_ptrs: &[WrappedEnv<'a>],
    env: WrappedEnv<'a>,
) -> WrappedEnv<'a> {
    decls
        .iter()
        .zip(decl_ptrs.iter())
        .for_each(|(decl, decl_ptr)| match decl {
            Decl::Expression(_, Expr::Fn_(param, body)) => Env::set_value(
                decl_ptr,
                Value::decl_function(param.clone(), body.clone(), Rc::downgrade(&env)),
            ),
            Decl::Expression(_, expr) => Env::set_value(
                decl_ptr,
                Value::delayed_decl(
                    expr.clone(), // Could probably fix this so it doesn't clone...
                    Rc::downgrade(&decl_ptr),
                    Rc::downgrade(&env),
                ),
            ),
        });
    env
}

pub fn eval_expr<'a>(expr: Expr<'a>, env: &'a WrappedEnv<'a>) -> Value<'a> {
    let mut result = eval_expr_thunk(&expr, env);
    loop {
        result = match result {
            EvalResult::Value(v) => return v.clone(),
            EvalResult::Thunk(next) => next(),
        }
    }
}

fn eval_expr_thunk<'a>(expr: &'a Expr<'a>, env: &'a WrappedEnv<'a>) -> EvalResult<'a> {
    match expr {
        Expr::Unary(op, a) => {
            eval_expr_thunk(&*a, env).and_then(|a| EvalResult::Value(op.eval(*a)))
        }
        Expr::Binary(a, op, b) => eval_expr_thunk(&*a, env).and_then(|a| {
            eval_expr_thunk(&*b, env).and_then(|b| EvalResult::Value(op.eval(*a, *b)))
        }),
        Expr::Literal(val) => EvalResult::Value(val.clone()),
        Expr::If(cond, a, b) => eval_expr_thunk(&*cond, env).and_then(|cond| match cond {
            Value::Bool(true) => eval_expr_thunk(&*a, env),
            Value::Bool(false) => eval_expr_thunk(&*b, env),
            _ => error("If condition must return a boolean"),
        }),
        Expr::Variable(ident) => match Env::get(&env, &ident) {
            Some(val) => EvalResult::Value(val.eval(Some(Rc::clone(env)))),
            None => error(&format!("Variable '{}' is not declared", ident)),
        },
        Expr::Let(ident, value, inner) => eval_expr_thunk(&*value, env).and_then(|value| {
            match VarEnv::associate(*ident, *value, &env) {
                Ok(env) => eval_expr_thunk(&*inner, &env),
                Err(error) => EvalResult::Value(Value::Error(error)),
            }
        }),
        Expr::Fn_(param, body) => EvalResult::Value(Value::function(*param, *body, Rc::clone(env))),
        Expr::FnApp(function, arg) => {
            eval_expr_thunk(&*function, env).and_then(|function| match function {
                Value::Function(param, body, fn_env) => {
                    eval_expr_thunk(&*arg, env).and_then(|arg| {
                        match VarEnv::associate(*param, *arg, &fn_env.unwrap()) {
                            Ok(fn_env) => {
                                EvalResult::Thunk(Box::new(|| eval_expr_thunk(&*body, &fn_env)))
                            }
                            Err(error) => EvalResult::Value(Value::Error(error)),
                        }
                    })
                }
                _ => error(&format!(
                    "Can't apply argument to type '{}'",
                    function.type_()
                )),
            })
        }
        Expr::Match(val, patterns) => eval_expr_thunk(&*val, env).and_then(|val| {
            match patterns.into_iter().find_map(|(pattern, expr)| {
                VarEnv::associate(*pattern, val.clone(), &env)
                    .map(|env| Some((env, expr)))
                    .unwrap_or(None)
            }) {
                Some((env, expr)) => eval_expr_thunk(expr, &env),
                None => error("Value didn't match any patterns"),
            }
        }),
        Expr::Delayed(ident, value, inner) => {
            let new_env = VarEnv::associate(
                *ident,
                Value::Error("Value not yet initialized".into()),
                &env,
            )
            .unwrap(); // This will never fail because the ident is always a variable identifier
            VarEnv::set_value(
                &new_env,
                Value::delayed(*value, Rc::downgrade(&new_env), Rc::clone(&env)),
            );
            eval_expr_thunk(&*inner, &new_env)
        }
    }
}

fn error(message: &str) -> EvalResult {
    // Make error a seperate enum, which can help with adding in error processing
    // Also will have to check for errors in the 'Expr::Literal' branch above
    // Although after this, maybe Value::Error won't be necessary
    // Also, see 'Expr::Let', 'Expr::FnApp'
    EvalResult::Value(Value::Error(message.into()))
}

enum EvalResult<'a> {
    Thunk(Box<dyn FnOnce() -> EvalResult<'a> + 'a>),
    Value(Value<'a>),
}

impl<'a> EvalResult<'a> {
    fn and_then<F>(self, f: F) -> Self
    where
        F: Clone + FnOnce(&Value<'a>) -> EvalResult<'a> + 'a,
    {
        match self {
            EvalResult::Thunk(thunk) => EvalResult::thunk(move || thunk().and_then(f.clone())),
            EvalResult::Value(val) => f(&val),
        }
    }

    fn thunk<F>(f: F) -> Self
    where
        F: FnOnce() -> EvalResult<'a> + 'a,
    {
        EvalResult::Thunk(Box::new(f))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Match;
    #[test]
    fn eval_literal() {
        let expected = Value::Int(1);
        let actual = eval_expr(Expr::int(1), &VarEnv::empty());
        assert_eq!(expected, actual);
    }
    #[test]
    fn eval_binary() {
        let expected = Value::Int(3);
        let actual = eval_expr(Expr::plus(Expr::int(1), Expr::int(2)), &VarEnv::empty());
        assert_eq!(expected, actual);
    }
    #[test]
    fn eval_unary() {
        let expected = Value::Int(-3);
        let actual = eval_expr(Expr::negate(Expr::int(3)), &VarEnv::empty());
        assert_eq!(expected, actual);
    }
    #[test]
    fn eval_if_expr() {
        let expected = Value::Int(1);
        let actual = eval_expr(
            Expr::if_expr(Expr::bool(true), Expr::int(1), Expr::int(2)),
            &VarEnv::empty(),
        );
        assert_eq!(expected, actual);
        let expected = Value::Int(2);
        let actual = eval_expr(
            Expr::if_expr(Expr::bool(false), Expr::int(1), Expr::int(2)),
            &VarEnv::empty(),
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
        ) => Value::Error("Variable 'b' is not declared".into())
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
    basic_test! {
        match_expr
        eval_expr(
            Expr::match_(Expr::int(1), vec![
                (Match::int(0), Expr::int(0)),
                (Match::int(1), Expr::int(1)),
                (Match::ident("a"), Expr::int(2))
            ]),
            &VarEnv::empty()
        ) => Value::Int(1);
        eval_expr(
            Expr::match_(Expr::int(3), vec![
                (Match::int(0), Expr::int(0)),
                (Match::int(1), Expr::int(1)),
                (Match::int(2), Expr::int(2))
            ]),
            &VarEnv::empty()
        ) => Value::Error("Value didn't match any patterns".into())
    }
    basic_test! {
        decl_eval
        {
            let decls = vec![
                Decl::Expression(
                    "add1".into(),
                    Expr::fn_expr(
                        Match::ident("a"),
                        Expr::plus(Expr::variable("a"), Expr::int(1))
                    )
                )
            ];
            eval_expr(
                Expr::fn_app(
                    Expr::variable("add1"),
                    Expr::int(1)
                ),
                &env_from_decls(&decls)
            )
        } => Value::Int(2);
        {
            let decls = vec![
                Decl::Expression(
                    "a".into(),
                    Expr::int(1)
                ),
                Decl::Expression(
                    "b".into(),
                    Expr::plus(Expr::variable("a"), Expr::int(1))
                )
            ];
            eval_expr(
                Expr::variable("b"),
                &env_from_decls(&decls)
            )
        } => Value::Int(2)
    }
    basic_test! {
        orderless_decl_eval
        {
            let decls = vec![
                Decl::Expression(
                    "a".into(),
                    Expr::plus(Expr::variable("b"), Expr::int(1))
                ),
                Decl::Expression(
                    "b".into(),
                    Expr::int(2)
                )
            ];
            let env = env_from_decls(&decls);
            eval_expr(
                Expr::variable("a"),
                &env
            )
        } => Value::Int(3)
    }
    basic_test! {
        delayed_test
        eval_expr(
            Expr::delayed(
                Match::ident("a"),
                Expr::variable("b"),
                Expr::let_expr(
                    Match::ident("b"),
                    Expr::int(1),
                    Expr::plus(Expr::variable("a"), Expr::variable("b"))
                )
            ),
            &VarEnv::empty()
        ) => Value::Int(2)
    }
}
