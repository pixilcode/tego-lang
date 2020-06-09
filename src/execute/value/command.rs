use crate::execute::value::Value;
use crate::execute::value::tuple::TupleWrapper;
use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone)]
pub struct CommandWrapper(Rc<dyn Command>);

impl CommandWrapper {
	fn unit(value: Value) -> impl Command {
		Unit(value)
	}

	fn bind<F>(&self, f: F) -> impl Command
	where F: Fn(Value) -> Rc<dyn Command> + 'static {
		Compound(Rc::clone(&self.0), Rc::new(f))
	}
}

impl PartialEq for CommandWrapper {
	// Commands can't be tested for equality
	fn eq(&self, _: &Self) -> bool {
		false
	}
}

pub trait Command: std::fmt::Debug {
	fn run(&self) -> Value;
}

#[derive(Debug)]
struct Unit(Value);

impl Command for Unit {
	fn run(&self) -> Value {
		self.0.clone()
	}
}

struct Compound(Rc<dyn Command>, Rc<dyn Fn(Value) -> Rc<dyn Command>>);

impl Command for Compound {
	fn run(&self) -> Value {
		let result = self.0.run();
		self.1(result).run()
	}
}

impl std::fmt::Debug for Compound {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Compound({:?}, <fn>)", self.0)
	}
}

#[derive(Debug)]
struct PrintlnCmd(Value);

impl Command for PrintlnCmd {
	fn run(&self) -> Value {
		match self.0 {
			Value::Tuple(TupleWrapper::String(ref s)) => println!("{}", s),
			ref v => println!("{}", v)
		}
		Value::unit()
	}
}