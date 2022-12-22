mod decl;
mod expr;
mod match_;
mod prog;
mod tokens;

use crate::{
	Input,
	DeclOutput,
	ParseResult,
	ProgOutput,
	ExprOutput, MatchOutput,
};
use crate::error::parse_handlers::complete_parser;
use decl::decl as parse_decl;
use expr::expr as parse_expr;
use match_::match_ as parse_match;
use prog::prog as parse_prog;

pub use tokens::is_valid_char;

/// The external-facing API for parsing programs
pub fn prog<P>(input: Input) -> ParseResult<P>
where
	P: ProgOutput
{
	complete_parser(parse_prog)(input)
}

/// The external-facing API for parsing declarations
pub fn decl<D>(input: Input) -> ParseResult<D>
where
	D: DeclOutput
{
	complete_parser(parse_decl)(input)
}

/// The external-facing API for parsing expressions
pub fn expr<E>(input: Input) -> ParseResult<E>
where
	E: ExprOutput
{
	complete_parser(parse_expr)(input)
}

/// The external-facing API for parsing matches
pub fn match_<M>(input: Input) -> ParseResult<M>
where
	M: MatchOutput
{
	complete_parser(parse_match)(input)
}
