use crate::value::Value;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Literal(Value)
}

impl Expr {
    pub fn unary(op: UnaryOp, a: Self) -> Self {
        Expr::Unary(op, Box::new(a))
    }
    
    pub fn unary_from_str(op: &str, a: Self) -> Self {
        Expr::unary(UnaryOp::from(op), a)
    }
    
    pub fn binary(a: Self, op: BinaryOp, b: Self) -> Self {
        Expr::Binary(Box::new(a), op, Box::new(b))
    }
    
    pub fn binary_from_str(a: Self, op: &str, b: Self) -> Self {
        Expr::binary(a, BinaryOp::from(op), b)
    }
    
    pub fn int(i: i32) -> Self {
        Expr::Literal(Value::Int(i))
    }
    
    pub fn bool(b: bool) -> Self {
        Expr::Literal(Value::Bool(b))
    }
    
    pub fn unit() -> Self {
        Expr::Literal(Value::Unit)
    }
    
    pub fn tuple(vals: Vec<Value>) -> Self {
        Expr::Literal(Value::Tuple(vals))
    }
    
    pub fn error(err: &str) -> Self {
        Expr::Literal(Value::Error(err.to_string()))
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not
}

impl UnaryOp {
    pub fn eval(&self, a: Value) -> Value {
        match self {
            UnaryOp::Negate => -a,
            UnaryOp::Not => !a
        }
    }
}

impl From<&str> for UnaryOp {
    fn from(s: &str) -> Self {
        match s {
            "-" => UnaryOp::Negate,
            "not" => UnaryOp::Not,
            other => panic!(
                "Unary op {} has not been defined!",
                other
            )
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Xor,
    Tuplate // ',' operator, creates a tuple
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
            BinaryOp::Tuplate => Value::tuplate(a, b)
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
            "," => BinaryOp::Tuplate,
            other => panic!(
                "Binary op {} has not been defined!",
                other
            )
        }
    }
}