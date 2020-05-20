use crate::ast::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Decl<'a> {
    Expression(String, Expr<'a>),
}

impl<'a> Decl<'a> {
    pub fn expression(ident: &str, body: Expr<'a>) -> Self {
        Decl::Expression(ident.into(), body)
    }
}
