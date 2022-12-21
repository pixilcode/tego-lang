use crate::error::{
    ParseErrorKind,
    handlers::{expect_expr, expect_keyword}
};
use crate::expr;
use crate::match_;
use crate::parsers::tokens::*;
use crate::DeclOutput;
use crate::ExprOutput;
use crate::Input;
use crate::ParseResult;

use nom::{
    combinator::map,
    multi::many0,
    sequence::{tuple, preceded}
};

type DeclResult<'a, D> = ParseResult<'a, D>;

pub fn decl<D>(input: Input<'_>) -> DeclResult<'_, D>
where
    D: DeclOutput,
{
    req_nl(expression)(input)
}

fn expression<D>(input: Input<'_>) -> DeclResult<'_, D>
where
    D: DeclOutput,
{
    map(
        tuple((
            identifier,
            many0(match_),
            preceded(
                opt_nl(expect_keyword(assign, ParseErrorKind::DeclAssign)),
                expect_expr(expr)
            )
        )),
        |(ident, params, body)| D::expression(
            ident.to_str(),
            params
                .into_iter()
                .rev()
                .fold(body, |body, param| D::Expr::fn_expr(param, body))
        )
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Decl, Expr, Match};
    use crate::test::*;
    use crate::Span;
    use crate::span::span_at;

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
    
    basic_test! {
        expression_error_test
        expression::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        expression::<()>("*".into()) =>
            parse_error("*".into(), 1, 1, ParseErrorKind::NoMatch);
        expression::<()>("a".into()) =>
            parse_failure(span_at("", 2, 1, 1), 2, 1, ParseErrorKind::DeclAssign);
        expression::<()>("a =".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::ExpectedExpr)
    }
}
