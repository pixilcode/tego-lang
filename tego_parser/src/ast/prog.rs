use crate::ast::{Decl, Expr};
use crate::ProgOutput;

#[derive(PartialEq, Debug, Clone)]
pub enum Prog {
    Library(Vec<Decl>),
    Binary(Expr, Vec<Decl>),
}

impl ProgOutput for Prog {
    type Decl = Decl;

    fn binary(main: Expr, decls: Vec<Decl>) -> Self {
        Prog::Binary(main, decls)
    }
    fn library(decls: Vec<Decl>) -> Self {
        Prog::Library(decls)
    }
}
