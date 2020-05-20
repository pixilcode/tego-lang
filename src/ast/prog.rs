use crate::ast::{Decl, Expr};

#[derive(PartialEq, Debug, Clone)]
pub enum Prog<'a> {
    Library(Vec<Decl<'a>>),
    Binary(Expr<'a>, Vec<Decl<'a>>),
}
