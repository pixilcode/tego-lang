use crate::ast::{Decl, Expr};

#[derive(PartialEq, Debug, Clone)]
pub enum Prog {
	Library(Vec<Decl>),
	Binary(Expr, Vec<Decl>),
}
