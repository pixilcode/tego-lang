use crate::ast::{DeclId, ExprId};
use std::rc::Rc;

pub enum Decl {
	Expression(String, ExprId)
}