use crate::ast::Expr;
use crate::DeclOutput;

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Expression(String, Expr),
}

impl DeclOutput for Decl {
    type Expr = Expr;

    fn expression(ident: &str, body: Expr) -> Self {
        Decl::Expression(ident.into(), body)
    }

    fn to_main(&self, main_fn_ident: &str) -> Option<Expr> {
        match self {
            Decl::Expression(ref ident, ref body) if ident == main_fn_ident => Some(body.clone()),
            _ => None,
        }
    }
}
