use crate::ast::{Decl, Expr, Prog};
use crate::parser::decl;
use crate::parser::Input;
use nom::IResult;

type ProgResult<'a> = IResult<Input<'a>, Prog>;

pub fn prog(input: Input<'_>) -> ProgResult<'_> {
	parse_prog(input).map(|(input, (main, decl))| match main {
		Some(main) => (input, Prog::Binary(main, decl)),
		None => (input, Prog::Library(decl)),
	})
}

fn parse_prog(input: Input<'_>) -> IResult<Input<'_>, (Option<Expr>, Vec<Decl>)> {
	let decl = decl::decl(input.trim());
	decl.and_then(|(input, decl)| {
		let (input, (main, mut decls)) = if input.is_empty() {
			(input, (None, vec![]))
		} else {
			parse_prog(input)?
		};

		let main = main.or_else(|| match decl {
			Decl::Expression(ref ident, ref expr) if ident == "main" => Some(expr.clone()),
			_ => None,
		});

		decls.insert(0, decl.clone());
		Ok((input, (main, decls)))
	})
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::ast::Match;

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
