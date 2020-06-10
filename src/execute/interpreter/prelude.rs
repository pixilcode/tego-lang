use crate::execute::interpreter::{VarEnv, WrappedEnv};
use crate::execute::value::Value;
use crate::execute::value::command::Command;

pub fn prelude() -> WrappedEnv {
	let prelude_decls =
		vec![
			("return", return_fn()),
			("println", println_fn())
		];
	prelude_decls.into_iter().fold(VarEnv::empty(), |parent, (ident, val)| VarEnv::associate_ident(ident.into(), val, parent))
}

fn return_fn() -> Value {
	Value::internal_fn(
		|val| Value::Command(val.into())
	)
}

fn println_fn() -> Value {
	Value::internal_fn(
		|val| Value::Command(Command::println(val))
	)
}