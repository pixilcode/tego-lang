use tego_parser::ast::{Match, Expr};
use crate::value::{StoredEnv, Value, VarEnv};
use crate::interpreter::eval_expr;
use std::rc::Rc;
use std::fmt;

#[derive(Clone)]
pub enum Function {
	UserDef(Match, Box<Expr>, StoredEnv),
	Internal(Rc<dyn Fn(Value) -> Value>)
}

impl Function {
	pub fn eval(self, arg: Value) -> Value {
		match self {
			Function::UserDef(param, body, fn_env) =>
				match VarEnv::associate(param, arg, &fn_env.unwrap()) {
					Ok(fn_env) => eval_expr(*body, &fn_env),
					Err(error) => Value::Error(error),
				},
			Function::Internal(f) => f(arg)
		}
	}
}

impl fmt::Debug for Function {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Function::UserDef(match_, body, env) =>
				write!(f, "UserDef{:?}", (match_, body, env)),
			Function::Internal(_) =>
				write!(f, "Internal"),
		}
	}
}

impl PartialEq for Function {
	fn eq(&self, _: &Self) -> bool {
		false // functions cannot be tested for equality
	}
}