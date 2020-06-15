use crate::ast::{ExprId, MatchId};
use std::rc::Rc;

pub enum Expr {
	Do(ExprId, MatchId, ExprId),
    If(ExprId, ExprId, ExprId),
    Let(MatchId, ExprId, ExprId),
    Fn_(MatchId, ExprId),
    FnApp(ExprId, ExprId),
    Match(ExprId, Vec<(MatchId, ExprId)>),
    Delayed(MatchId, ExprId, ExprId),
    Boxed(ExprId),
    Variable(String),
    Unary(UnaryOp, ExprId),
    Binary(ExprId, BinaryOp, ExprId),
    Literal(ExprValue),
}

pub enum ExprValue {
	Int(i32),
    Bool(bool),
    Unit,
    String(String),
    Char(char)
}

pub enum UnaryOp {
    Negate,
    Not,
}

pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Xor,
    Join,
    FlatJoin, 
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}