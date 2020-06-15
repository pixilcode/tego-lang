use crate::ast::MatchId;
use std::rc::Rc;

pub enum Match {
	Ident(String),
	Tuple(Vec<MatchId>),
	Boxed(MatchId),
	Value(MatchValue),
	Unit,
	Ignore
}

pub enum MatchValue {
	Int(i32),
    Bool(bool),
    Char(char),
    String(String),
}
