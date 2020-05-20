use crate::ast::Decl;
use crate::ast::Expr;
use crate::parser::expr::expr;
use crate::parser::match_::match_;
use crate::parser::tokens::*;

use nom::{multi::many0, sequence::tuple, IResult};

type DeclResult<'a> = IResult<&'a str, Decl<'a>>;

pub fn decl(input: &'_ str) -> DeclResult<'_> {
    req_nl(expression)(input)
}

fn expression(input: &'_ str) -> DeclResult<'_> {
    tuple((identifier, many0(match_), opt_nl(assign), expr))(input).map(
        |(input, (ident, params, _, body))| {
            (
                input,
                Decl::expression(
                    ident,
                    params
                        .into_iter()
                        .rev()
                        .fold(body, |body, param| Expr::fn_expr(param, body)),
                ),
            )
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Match;
    parser_test! {
        expression_test
        (decl): "val = 1\n" =>
            Decl::expression(
                "val",
                Expr::int(1)
            );
        (decl): "id a = a\n" =>
            Decl::expression(
                "id",
                Expr::fn_expr(Match::ident("a"), Expr::variable("a"))
            );
        (decl): "const a _ = a\n" =>
            Decl::expression(
                "const",
                Expr::fn_expr(
                    Match::ident("a"),
                    Expr::fn_expr(
                        Match::Ignore,
                        Expr::variable("a")
                    )
                )
            );
        // If the declaration is the last one, a new line isn't required
        (decl): "val = 1" =>
            Decl::expression(
                "val",
                Expr::int(1)
            )
    }
}
