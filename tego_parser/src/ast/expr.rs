use crate::ast::Match;
use crate::ExprOutput;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Do(Box<Expr>, Match, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Match, Box<Expr>, Box<Expr>),
    Fn_(Match, Box<Expr>),
    FnApp(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Match, Expr)>),
    Delayed(Match, Box<Expr>, Box<Expr>),
    Boxed(Box<Expr>),
    Variable(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Literal(ExprValue),
}

impl Expr {
    fn binary(a: Self, op: BinaryOp, b: Self) -> Self {
        Expr::Binary(Box::new(a), op, Box::new(b))
    }

    fn unary(op: UnaryOp, a: Self) -> Self {
        Expr::Unary(op, Box::new(a))
    }

    pub fn negate(a: Self) -> Self {
        Expr::unary(UnaryOp::Negate, a)
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(a: Self) -> Self {
        Expr::unary(UnaryOp::Not, a)
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

    pub fn flat_join(a: Self, b: Self) -> Self {
        Expr::binary(a, BinaryOp::FlatJoin, b)
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
}

impl ExprOutput for Expr {
    type Match = Match;

    fn binary(a: Self, op: &str, b: Self) -> Self {
        Expr::Binary(Box::new(a), op.into(), Box::new(b))
    }

    fn unary(op: &str, a: Self) -> Self {
        Expr::Unary(op.into(), Box::new(a))
    }

    fn let_expr(ident: Match, value: Self, inner: Self) -> Self {
        Expr::Let(ident, Box::new(value), Box::new(inner))
    }

    fn delayed(ident: Match, value: Self, inner: Self) -> Self {
        Expr::Delayed(ident, Box::new(value), Box::new(inner))
    }

    fn if_expr(cond: Self, t: Self, f: Self) -> Self {
        Expr::If(Box::new(cond), Box::new(t), Box::new(f))
    }

    fn match_(val: Self, patterns: Vec<(Match, Self)>) -> Self {
        Expr::Match(Box::new(val), patterns)
    }

    fn fn_expr(param: Match, body: Self) -> Self {
        Expr::Fn_(param, Box::new(body))
    }

    fn fn_app(function: Self, arg: Self) -> Self {
        Expr::FnApp(Box::new(function), Box::new(arg))
    }

    fn unit() -> Self {
        Expr::Literal(ExprValue::Unit)
    }

    fn boxed(expr: Self) -> Self {
        Expr::Boxed(Box::new(expr))
    }

    fn bool(b: bool) -> Self {
        Expr::Literal(ExprValue::Bool(b))
    }

    fn int(i: i32) -> Self {
        Expr::Literal(ExprValue::Int(i))
    }

    fn variable(ident: &str) -> Self {
        Expr::Variable(ident.into())
    }

    fn string(s: String) -> Self {
        Expr::Literal(ExprValue::String(s))
    }

    fn char(c: char) -> Self {
        Expr::Literal(ExprValue::Char(c))
    }

    fn do_expr(command: Self, result_match: Match, body: Self) -> Self {
        Expr::Do(Box::new(command), result_match, Box::new(body))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprValue {
    Int(i32),
    Bool(bool),
    Unit,
    String(String),
    Char(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
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
    Join,     // ',' operator, creates a tuple
    FlatJoin, // ',,' operator
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
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
            ",," => BinaryOp::FlatJoin,
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
