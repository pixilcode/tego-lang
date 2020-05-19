use crate::ast::match_::Match;
use crate::execute::value::Value;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Match, Box<Expr>, Box<Expr>),
    Fn_(Match, Box<Expr>),
    FnApp(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Match, Expr)>),
    Delayed(Match, Box<Expr>, Box<Expr>),
    Variable(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Literal(Value),
}

impl Expr {
    pub fn let_expr(ident: Match, value: Self, inner: Self) -> Self {
        Expr::Let(ident, Box::new(value), Box::new(inner))
    }

    pub fn if_expr(cond: Self, t: Self, f: Self) -> Self {
        Expr::If(Box::new(cond), Box::new(t), Box::new(f))
    }

    pub fn fn_expr(param: Match, body: Self) -> Self {
        Expr::Fn_(param, Box::new(body))
    }

    pub fn fn_app(function: Self, arg: Self) -> Self {
        Expr::FnApp(Box::new(function), Box::new(arg))
    }

    pub fn match_(val: Self, patterns: Vec<(Match, Self)>) -> Self {
        Expr::Match(Box::new(val), patterns)
    }

    pub fn delayed(ident: Match, value: Self, inner: Self) -> Self {
        Expr::Delayed(ident, Box::new(value), Box::new(inner))
    }

    pub fn unary(op: UnaryOp, a: Self) -> Self {
        Expr::Unary(op, Box::new(a))
    }

    pub fn unary_from_str(op: &str, a: Self) -> Self {
        Expr::unary(UnaryOp::from(op), a)
    }

    pub fn negate(a: Self) -> Self {
        Expr::unary(UnaryOp::Negate, a)
    }

    pub fn not(a: Self) -> Self {
        Expr::unary(UnaryOp::Not, a)
    }

    pub fn binary(a: Self, op: BinaryOp, b: Self) -> Self {
        Expr::Binary(Box::new(a), op, Box::new(b))
    }

    pub fn binary_from_str(a: Self, op: &str, b: Self) -> Self {
        Expr::binary(a, BinaryOp::from(op), b)
    }

    pub fn plus(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Plus, b)
    }

    pub fn minus(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Minus, b)
    }

    pub fn multiply(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Multiply, b)
    }

    pub fn divide(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Divide, b)
    }

    pub fn modulo(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Modulo, b)
    }

    pub fn and(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::And, b)
    }

    pub fn or(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Or, b)
    }

    pub fn xor(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Xor, b)
    }

    pub fn join(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Join, b)
    }

    pub fn equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::Equal, b)
    }

    pub fn not_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::NotEqual, b)
    }

    pub fn less_than(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::LessThan, b)
    }

    pub fn greater_than(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::GreaterThan, b)
    }

    pub fn less_than_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::LessThanEqual, b)
    }

    pub fn greater_than_equal(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::GreaterThanEqual, b)
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

    pub fn tuple(vals: Vec<Value>) -> Self {
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

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl UnaryOp {
    pub fn eval(&self, a: Value) -> Value {
        match self {
            UnaryOp::Negate => -a,
            UnaryOp::Not => !a,
        }
    }
}

impl From<&str> for UnaryOp {
    fn from(s: &str) -> Self {
        match s {
            "-" => UnaryOp::Negate,
            "not" => UnaryOp::Not,
            other => panic!("Unary op {} has not been defined!", other),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Xor,
    Join, // ',' operator, creates a tuple
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}

impl BinaryOp {
    pub fn eval(&self, a: Value, b: Value) -> Value {
        match self {
            BinaryOp::Plus => a + b,
            BinaryOp::Minus => a - b,
            BinaryOp::Multiply => a * b,
            BinaryOp::Divide => a / b,
            BinaryOp::Modulo => a % b,
            BinaryOp::And => a & b,
            BinaryOp::Or => a | b,
            BinaryOp::Xor => a ^ b,
            BinaryOp::Join => Value::join(a, b),
            BinaryOp::Equal => Value::Bool(a == b),
            BinaryOp::NotEqual => Value::Bool(a != b),
            BinaryOp::LessThan => a.less_than(b),
            BinaryOp::GreaterThan => a.greater_than(b),
            BinaryOp::LessThanEqual => a.less_than_equal(b),
            BinaryOp::GreaterThanEqual => a.greater_than_equal(b),
        }
    }
}

impl From<&str> for BinaryOp {
    fn from(s: &str) -> Self {
        match s {
            "+" => BinaryOp::Plus,
            "-" => BinaryOp::Minus,
            "*" => BinaryOp::Multiply,
            "/" => BinaryOp::Divide,
            "%" => BinaryOp::Modulo,
            "and" => BinaryOp::And,
            "or" => BinaryOp::Or,
            "xor" => BinaryOp::Xor,
            "," => BinaryOp::Join,
            "==" => BinaryOp::Equal,
            "/=" => BinaryOp::NotEqual,
            "<" => BinaryOp::LessThan,
            ">" => BinaryOp::GreaterThan,
            "<=" => BinaryOp::LessThanEqual,
            ">=" => BinaryOp::GreaterThanEqual,
            other => panic!("Binary op {} has not been defined!", other),
        }
    }
}
