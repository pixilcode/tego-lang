use crate::ast::match_::Match;
use crate::execute::value::Value;
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    If(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    Let(Match<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    Fn_(Match<'a>, Box<Expr<'a>>),
    FnApp(Box<Expr<'a>>, Box<Expr<'a>>),
    Match(Box<Expr<'a>>, Vec<(Match<'a>, Expr<'a>)>),
    Delayed(Match<'a>, Box<Expr<'a>>, Box<Expr<'a>>),
    Variable(String),
    Unary(UnaryOp<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, BinaryOp<'a>, Box<Expr<'a>>),
    Literal(Value<'a>),
}

impl<'a> Expr<'a> {
    pub fn let_expr(ident: Match<'a>, value: Self, inner: Self) -> Self {
        Expr::Let(ident, Box::new(value), Box::new(inner))
    }

    pub fn if_expr(cond: Self, t: Self, f: Self) -> Self {
        Expr::If(Box::new(cond), Box::new(t), Box::new(f))
    }

    pub fn fn_expr(param: Match<'a>, body: Self) -> Self {
        Expr::Fn_(param, Box::new(body))
    }

    pub fn fn_app(function: Self, arg: Self) -> Self {
        Expr::FnApp(Box::new(function), Box::new(arg))
    }

    pub fn match_(val: Self, patterns: Vec<(Match<'a>, Self)>) -> Self {
        Expr::Match(Box::new(val), patterns)
    }

    pub fn delayed(ident: Match<'a>, value: Self, inner: Self) -> Self {
        Expr::Delayed(ident, Box::new(value), Box::new(inner))
    }

    pub fn unary(op: UnaryOp<'a>, a: Self) -> Self {
        Expr::Unary(op, Box::new(a))
    }

    pub fn unary_from_str(op: &str, a: Self) -> Self {
        Expr::unary(UnaryOp::from(op), a)
    }

    pub fn negate(a: Self) -> Self {
        Expr::unary(UnaryOp::negate(), a)
    }

    pub fn not(a: Self) -> Self {
        Expr::unary(UnaryOp::not(), a)
    }

    pub fn binary(a: Self, op: BinaryOp<'a>, b: Self) -> Self {
        Expr::Binary(Box::new(a), op, Box::new(b))
    }

    pub fn binary_from_str(a: Self, op: &str, b: Self) -> Self {
        Expr::binary(a, BinaryOp::from(op), b)
    }

    pub fn plus(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::plus(), b)
    }

    pub fn minus(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::minus(), b)
    }

    pub fn multiply(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::multiply(), b)
    }

    pub fn divide(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::divide(), b)
    }

    pub fn modulo(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::modulo(), b)
    }

    pub fn and(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::and(), b)
    }

    pub fn or(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::or(), b)
    }

    pub fn xor(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::xor(), b)
    }

    pub fn join(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::join(), b)
    }

    pub fn equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::equal(), b)
    }

    pub fn not_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::notEqual(), b)
    }

    pub fn less_than(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::lessThan(), b)
    }

    pub fn greater_than(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::greaterThan(), b)
    }

    pub fn less_than_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::lessThanEqual(), b)
    }

    pub fn greater_than_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::greaterThanEqual(), b)
    }

    pub fn int(i: i32) -> Self {
        Expr::Literal(Value::Int(i))
    }

    pub fn bool(b: bool) -> Self {
        Expr::Literal(Value::Bool(b))
    }

    pub fn unit() -> Self {
        Expr::Literal(Value::unit())
    }

    pub fn tuple(vals: Vec<Value<'a>>) -> Self {
        Expr::Literal(Value::Tuple(vals))
    }

    pub fn string(s: &str) -> Self {
        Expr::Literal(Value::string(s))
    }

    pub fn char(c: char) -> Self {
        Expr::Literal(Value::Char(c))
    }

    pub fn variable(ident: &str) -> Self {
        Expr::Variable(ident.into())
    }

    pub fn error(err: &str) -> Self {
        Expr::Literal(Value::Error(err.into()))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp<'a> {
    Negate(PhantomData<Value<'a>>),
    Not(PhantomData<Value<'a>>),
}

impl<'a> UnaryOp<'a> {
    pub fn eval(&self, a: Value<'a>) -> Value<'a> {
        match self {
            UnaryOp::Negate(_) => -a,
            UnaryOp::Not(_) => !a,
        }
    }

    fn negate() -> Self {
        UnaryOp::Negate(PhantomData)
    }

    fn not() -> Self {
        UnaryOp::Not(PhantomData)
    }
}

impl<'a> From<&str> for UnaryOp<'a> {
    fn from(s: &str) -> Self {
        match s {
            "-" => UnaryOp::negate(),
            "not" => UnaryOp::not(),
            other => panic!("Unary op {} has not been defined!", other),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp<'a> {
    Plus(PhantomData<Value<'a>>),
    Minus(PhantomData<Value<'a>>),
    Multiply(PhantomData<Value<'a>>),
    Divide(PhantomData<Value<'a>>),
    Modulo(PhantomData<Value<'a>>),
    And(PhantomData<Value<'a>>),
    Or(PhantomData<Value<'a>>),
    Xor(PhantomData<Value<'a>>),
    Join(PhantomData<Value<'a>>), // ',' operator, creates a tuple
    Equal(PhantomData<Value<'a>>),
    NotEqual(PhantomData<Value<'a>>),
    LessThan(PhantomData<Value<'a>>),
    GreaterThan(PhantomData<Value<'a>>),
    LessThanEqual(PhantomData<Value<'a>>),
    GreaterThanEqual(PhantomData<Value<'a>>),
}

impl<'a> BinaryOp<'a> {
    pub fn eval(&self, a: Value<'a>, b: Value<'a>) -> Value<'a> {
        match self {
            BinaryOp::Plus(_) => a + b,
            BinaryOp::Minus(_) => a - b,
            BinaryOp::Multiply(_) => a * b,
            BinaryOp::Divide(_) => a / b,
            BinaryOp::Modulo(_) => a % b,
            BinaryOp::And(_) => a & b,
            BinaryOp::Or(_) => a | b,
            BinaryOp::Xor(_) => a ^ b,
            BinaryOp::Join(_) => Value::join(a, b),
            BinaryOp::Equal(_) => Value::Bool(a == b),
            BinaryOp::NotEqual(_) => Value::Bool(a != b),
            BinaryOp::LessThan(_) => a.less_than(b),
            BinaryOp::GreaterThan(_) => a.greater_than(b),
            BinaryOp::LessThanEqual(_) => a.less_than_equal(b),
            BinaryOp::GreaterThanEqual(_) => a.greater_than_equal(b),
        }
    }

    fn plus() -> Self {
        BinaryOp::Plus(PhantomData)
    }

    fn minus() -> Self {
        BinaryOp::Minus(PhantomData)
    }

    fn multiply() -> Self {
        BinaryOp::Multiply(PhantomData)
    }

    fn divide() -> Self {
        BinaryOp::Divide(PhantomData)
    }

    fn modulo() -> Self {
        BinaryOp::Modulo(PhantomData)
    }

    fn and() -> Self {
        BinaryOp::And(PhantomData)
    }

    fn or() -> Self {
        BinaryOp::Or(PhantomData)
    }

    fn xor() -> Self {
        BinaryOp::Xor(PhantomData)
    }

    fn join() -> Self {
        BinaryOp::Join(PhantomData)
    }

    fn equal() -> Self {
        BinaryOp::Equal(PhantomData)
    }

    fn notEqual() -> Self {
        BinaryOp::NotEqual(PhantomData)
    }

    fn lessThan() -> Self {
        BinaryOp::LessThan(PhantomData)
    }

    fn greaterThan() -> Self {
        BinaryOp::GreaterThan(PhantomData)
    }

    fn lessThanEqual() -> Self {
        BinaryOp::LessThanEqual(PhantomData)
    }

    fn greaterThanEqual() -> Self {
        BinaryOp::GreaterThanEqual(PhantomData)
    }
}

impl<'a> From<&str> for BinaryOp<'a> {
    fn from(s: &str) -> Self {
        match s {
            "+" => BinaryOp::plus(),
            "-" => BinaryOp::minus(),
            "*" => BinaryOp::multiply(),
            "/" => BinaryOp::divide(),
            "%" => BinaryOp::modulo(),
            "and" => BinaryOp::and(),
            "or" => BinaryOp::or(),
            "xor" => BinaryOp::xor(),
            "," => BinaryOp::join(),
            "==" => BinaryOp::equal(),
            "/=" => BinaryOp::notEqual(),
            "<" => BinaryOp::lessThan(),
            ">" => BinaryOp::greaterThan(),
            "<=" => BinaryOp::lessThanEqual(),
            ">=" => BinaryOp::greaterThanEqual(),
            other => panic!("Binary op {} has not been defined!", other),
        }
    }
}
