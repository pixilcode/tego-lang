use crate::value::tuple::Tuple;
use crate::value::Value;
use std::fmt;
use std::rc::Rc;
use std::io::{self, BufRead};

#[derive(Clone)]
pub enum Command {
    Unit(Rc<Value>),
    Compound(Rc<Command>, Rc<dyn Fn(Value) -> Result<Command, Value>>),
    Println(Rc<Value>),
    ReadLine,
    ReadInt,
}

impl Command {
    pub fn unit(value: Value) -> Self {
        Command::Unit(Rc::new(value))
    }

    pub fn bind<F>(&self, f: F) -> Self
    where
        F: Fn(Value) -> Result<Command, Value> + 'static,
    {
        Command::Compound(Rc::new(self.clone()), Rc::new(f))
    }
    
    pub fn run(&self) -> Value {
        match self {
            Command::Unit(val) => run_unit(val),
            Command::Compound(first, next) => run_compound(first, next),
            Command::Println(val) => run_println(val),
            Command::ReadLine => run_readline(),
            Command::ReadInt => run_readint()
        }
    }
    
    pub fn println(value: Value) -> Self {
        Command::Println(Rc::new(value))
    }

    pub fn readline() -> Self {
        Command::ReadLine
    }

    pub fn readint() -> Self {
        Command::ReadInt
    }
}

impl PartialEq for Command {
    // Commands can't be tested for equality
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

conversion!(Command[value: Value] => Command::unit(value));

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Command::Unit(val) => write!(f, "Command({:?})", val),
            Command::Compound(first, _) => write!(f, "Command({:?}, Next(<fn>))", first),
            Command::Println(val) => write!(f, "Command(Println({:?}))", val),
            Command::ReadLine => write!(f, "Command(ReadLine)"),
            Command::ReadInt => write!(f, "Command(ReadInt)")
        }
    }
}

fn run_unit(value: &Value) -> Value {
    value.clone()
}

fn run_compound(first: &Command, next: &Rc<dyn Fn(Value) -> Result<Command, Value>>) -> Value {
    let result = first.run();
    match next(result) {
        Ok(command) => command.run(),
        Err(value) => value
    }
}

fn run_println(value: &Value) -> Value {
    match value {
        Value::Tuple(Tuple::String(ref s)) => println!("{}", s),
        ref v => println!("{}", v),
    }
    Value::unit()
}

fn run_readline() -> Value {
    let mut string = String::new();
    let result = io::stdin().read_line(&mut string);
    match result {
        Ok(_) => string.into(),
        Err(error) => Value::Error(error.to_string())
    }
}

fn run_readint() -> Value {
    let stdin = io::stdin();
    let mut lock = stdin.lock();
    let mut input = vec![];
    let string = lock.read_until(b' ', &mut input).or_else(|_| lock.read_until(b'\n', &mut input));
    let string = match string {
        Ok(_) => String::from_utf8(input),
        Err(error) => return Value::Error(error.to_string())
    };
    let int = match string {
        Ok(string) => string.trim().parse::<i32>(),
        Err(error) => return Value::Error(error.to_string())
    };
    match int {
        Ok(i) => Value::Int(i),
        Err(error) => Value::Error(error.to_string())
    }
}
