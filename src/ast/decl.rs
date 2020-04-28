use crate::ast::expr::Expr;

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Expression(String, Expr)
}

impl Decl {
    pub fn expression(ident: &str, body: Expr) -> Self {
        Decl::Expression(ident.to_string(), body)
    }
}