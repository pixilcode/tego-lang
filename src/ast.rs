use crate::value::Value;

pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Literal(Value)
}

impl Expr {
    pub fn binary(a: Self, op: BinaryOp, b: Self) -> Self {
        Expr::Binary(Box::new(a), op, Box::new(b))
    }
    
    pub fn int(i: i32) -> Self {
        Expr::Literal(Value::Int(i))
    }
    
    pub fn unit() -> Self {
        Expr::Literal(Value::Unit)
    }
}

pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    // Tuplate // ',' operator, creates a tuple
}

impl BinaryOp {
    pub fn eval(&self, a: Value, b: Value) -> Value {
        match self {
            BinaryOp::Plus => a + b,
            BinaryOp::Minus => a - b,
            BinaryOp::Multiply => a * b,
            BinaryOp::Divide => a / b,
            BinaryOp::Modulo => a % b
        }
    }
}