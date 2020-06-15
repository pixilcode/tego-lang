use crate::ast::{ProgId, DeclId, ExprId};
use std::rc::Rc;

pub enum Prog {
	Library(Vec<DeclId>),
    Binary(ExprId, Vec<DeclId>),
}