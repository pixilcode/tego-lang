use crate::DeclOutput;
use crate::ExprOutput;
use crate::error::*;
use crate::expr;
use crate::match_;
use crate::parsers::tokens::*;
use crate::Input;
use crate::ParseResult;

use nom::{multi::many0, sequence::tuple};

type DeclResult<'a, D> = ParseResult<'a, D>;

pub fn decl<D>(input: Input<'_>) -> DeclResult<'_, D>
where D: DeclOutput {
    req_nl(expression)(input)
}

fn expression<D>(input: Input<'_>) -> DeclResult<'_, D>
where D: DeclOutput {
    tuple((identifier, many0(match_), opt_nl(assign), expr))(input)
        .map_err(decl_expr_error)
        .map(|(input, (ident, params, _, body))| {
            (
                input,
                D::expression(
                    ident.to_str(),
                    params
                        .into_iter()
                        .rev()
                        .fold(body, |body, param| D::Expr::fn_expr(param, body)),
                ),
            )
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::*;
    use crate::Span;
    use crate::ast::{Decl, Expr, Match};

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
