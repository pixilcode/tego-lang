use crate::{ProgOutput, DeclOutput};
use crate::decl;
use crate::parsers::tokens::newlines;
use crate::Input;
use crate::ParseResult;
use nom::combinator::all_consuming;
use nom::sequence::preceded;

type ProgResult<'a, P> = ParseResult<'a, P>;

pub fn prog<P>(input: Input<'_>) -> ProgResult<'_, P>
where P: ProgOutput {
    all_consuming(parse_prog)(input).map(|(input, (main, decl))| match main {
        Some(main) => (input, P::binary(main, decl)),
        None => (input, P::library(decl)),
    })
}

fn parse_prog<D>(input: Input<'_>) -> ParseResult<'_, (Option<<D as DeclOutput>::Expr>, Vec<D>)>
where D: DeclOutput {
    let decl_res = preceded(newlines(false), decl)(input);
    decl_res.and_then(|(input, decl): (Input, D)| {
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
    use crate::test::*;
    use crate::ast::{Prog, Decl, Expr, Match};

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
