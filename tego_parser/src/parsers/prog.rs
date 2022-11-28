use crate::decl;
use crate::parsers::tokens::newlines;
use crate::Input;
use crate::ParseResult;
use crate::{DeclOutput, ProgOutput};
use nom::combinator::{all_consuming, map};
use nom::sequence::preceded;

type ProgResult<'a, P> = ParseResult<'a, P>;

pub fn prog<P>(input: Input<'_>) -> ProgResult<'_, P>
where
    P: ProgOutput,
{
    map(
        all_consuming(parse_prog),
        |(main, decls)| match main {
            Some(main) => P::binary(main, decls),
            None => P::library(decls),
        }
    )(input)
}

fn parse_prog<D>(input: Input<'_>) -> ParseResult<'_, (Option<<D as DeclOutput>::Expr>, Vec<D>)>
where
    D: DeclOutput,
{
    preceded(newlines(true), decl::<D>)(input)
        .and_then(|(input, decl)| {
            let (input, (main, mut decls)) = if input.to_str().is_empty() {
                (input, (None, vec![]))
            } else {
                parse_prog(input)?
            };

            let main = main.or_else(|| decl.to_main("main"));

            decls.insert(0, decl);
            Ok((input, (main, decls)))
        })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{Decl, Expr, Match, Prog};
    use crate::test::*;

    parser_test! {
        binary_test
        (prog): "
		main = id 1
		
		id a = a
		" => Prog::Binary(
            Expr::fn_app(Expr::variable("id"), Expr::int(1)),
            vec![
                Decl::Expression("main".into(), Expr::fn_app(Expr::variable("id"), Expr::int(1))),
                Decl::Expression("id".into(), Expr::fn_expr(Match::ident("a"), Expr::variable("a")))
            ]
        )
    }

    parser_test! {
        library_test
        (prog): "
		id a = a

		const a b = a
		" => Prog::Library(
            vec![
                Decl::Expression("id".into(), Expr::fn_expr(Match::ident("a"), Expr::variable("a"))),
                Decl::Expression("const".into(), Expr::fn_expr(Match::ident("a"), Expr::fn_expr(Match::ident("b"), Expr::variable("a"))))
            ]
        )
    }
}
