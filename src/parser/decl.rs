use crate::ast::Decl;
use crate::ast::Expr;
use crate::parser::expr;
use crate::parser::match_;
use crate::parser::tokens::*;
use crate::parser::Input;
use crate::parser::ParseResult;
use crate::parser::error::*;

use nom::{multi::many0, sequence::tuple};

type DeclResult<'a> = ParseResult<'a, Decl>;

pub fn decl(input: Input<'_>) -> DeclResult<'_> {
    req_nl(expression)(input)
}

fn expression(input: Input<'_>) -> DeclResult<'_> {
    tuple((identifier, many0(match_), opt_nl(assign), expr))(input)
    .map_err(decl_expr_error)
    .map(
        |(input, (ident, params, _, body))| {
            (
                input,
                Decl::expression(
                    ident.to_str(),
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
    use crate::parser::test::*;
    use crate::parser::Span;

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
