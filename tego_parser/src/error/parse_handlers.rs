use crate::{Input, InternalParseResult, ParseResult};
use crate::error::{ParseError, ParseErrorKind};
use crate::parsers::is_valid_char;

/// Indicates that the parser is supposed to be finished (all input should be consumed,
/// and any errors are now failures)
pub fn complete_parser<F, O>(parser: F) -> impl Fn(Input) -> ParseResult<O>
where
	F: Fn(Input) -> nom::IResult<Input, O, (Input, ParseError)>
{
	move |input|
	parser(input)
		// Make all errors failures
		.map_err(|error| match error {
			nom::Err::Error((_, error)) | nom::Err::Failure((_, error)) =>
				error,
			nom::Err::Incomplete(_) => unreachable!("parser doesn't use 'streaming' parsers")
		})
		// Ensure that all input is consumed
		.and_then(|(input, output)| if input.to_str().len() > 0 {
			Err(ParseError::new_from_kind(input, ParseErrorKind::ExpectedEof))
		} else {
			Ok(output)
		})
}

/// If there is an error, ignore anything that was parsed by the
/// parser and just return the original input
pub fn err_retain_all<'a, F, O, E>(parser: F) -> impl Fn(Input<'a>) -> nom::IResult<Input<'a>, O, (Input<'a>, E)>
where
	F: Fn(Input<'a>) -> nom::IResult<Input<'a>, O, (Input<'a>, E)>,
{
	move |input| parser(input).map_err(|error| match error {
		nom::Err::Error((_, error)) => nom::Err::Error((input, error)),
		nom::Err::Failure((_, error)) => nom::Err::Failure((input, error)),
		error => error
	})
}

/// Indicate that this is the right hand side (rhs) of an operator
/// 
/// If the rhs doesn't match anything, alert that the rhs is missing
pub fn expect_rhs<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::NoMatch | ParseErrorKind::Eof
		) =>
			ParseError::nom_failure(input, ParseErrorKind::MissingRhs),
		error => error
	})
}

/// Indicate that this match pattern is expected
pub fn expect_match<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::NoMatch | ParseErrorKind::Eof
		) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedMatch),
		error => error
	})
}

/// Indicate that this expression is expected
pub fn expect_expr<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::NoMatch | ParseErrorKind::Eof
		) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedExpr),
		error => error
	})
}

/// Indicate that a variable identifier is expected
pub fn expect_variable<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::NoMatch | ParseErrorKind::Eof
		) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedVariable),
		error => error
	})
}

/// Indicate that the given token is expected
pub fn expect_keyword<'a, F, O>(parser: F, kind: ParseErrorKind) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::ExpectedKeyword { .. }
		) =>
			ParseError::nom_failure(input, kind),
		error => error
	})
}

/// Indicate that at least one match arm is expected
pub fn expect_match_arms<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> InternalParseResult<'a, O>
where
	F: Fn(Input<'a>) -> InternalParseResult<'a, O>,
{
	move |input|
	parser(input).map_err(|error| match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::ExpectedKeyword {
				keyword: "|",
				..
			}
		) =>
			ParseError::nom_failure(input, ParseErrorKind::MatchBar),
		error => error
	})
}

// Expr Errors

pub fn missing_rhs_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::MissingRhs),
		error => error
	}
}

pub fn num_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::NumberTooBig
		) => nom::Err::Failure((input, error)),
		error => error
	}
}

pub fn string_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::StringUnclosed |
			ParseErrorKind::InvalidEscapedString
		) => nom::Err::Failure((input, error)),
		error => error
	}
}

pub fn char_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, error)) if matches!(error.kind,
			ParseErrorKind::CharUnclosed |
			ParseErrorKind::InvalidChar |
			ParseErrorKind::InvalidEscapedChar
		) => nom::Err::Failure((input, error)),
		error => error
	}
}

pub fn literal_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) if input.to_str().is_empty() =>
			nom::Err::Error((
				input,
				ParseError::new_from_kind(input, ParseErrorKind::Eof)
			)),
		error => error
	}
}

// Match Errors
pub fn ident_match_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) 
			if input.to_str().is_empty() => nom::Err::Error((
				input, 
				ParseError::new_from_kind(input, ParseErrorKind::Eof)
			)),
		nom::Err::Error((input, error)) if error.kind == ParseErrorKind::KeywordIdentifier =>
			nom::Err::Failure((input, error.into())),
		nom::Err::Error((input, error)) => nom::Err::Error((input, error.into())),
		nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn basic_match_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _))
			if input.to_str().is_empty() => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::Eof))),
		nom::Err::Error((input, error)) if input.to_str().starts_with(char::is_alphabetic) =>
			nom::Err::Failure((input, error)),
		nom::Err::Error((input, _)) => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::NoMatch))),
		nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn tuple_error<'a>(comma: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, ParseError)>) -> nom::Err<(Input<'a>, ParseError)> {
	move |error|
	match error {
		nom::Err::Error((input, _))
			=> nom::Err::Failure((input, ParseError::new_from_kind(input, ParseErrorKind::IncompleteTuple(comma.line(), comma.column())))),
		nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn terminating_paren_error(line: usize, column: usize) -> impl Fn(nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	move |error|
	match error {
		nom::Err::Error((input, _)) =>
			nom::Err::Failure((
				input,
				ParseError::new_from_kind(input, ParseErrorKind::TerminatingParen(line, column))
			)),
		error => error,
	}
}

pub fn terminating_bracket_error(line: usize, column: usize) -> impl Fn(nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	move |error|
	match error {
		nom::Err::Error((input, _)) => // The only possible error is a 'right_bracket' error
			nom::Err::Failure((
				input,
				ParseError::new_from_kind(input, ParseErrorKind::TerminatingBracket(line, column))
			)),
		error => error,
	}
}

// Token Errors

pub fn char_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('\'') =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, _)) => {
			let c = input.to_str().chars().nth(1);
			match c {
				Some(c) if c == '\\' =>
					ParseError::nom_error(input, ParseErrorKind::InvalidEscapedChar),
				Some(c) if !is_valid_char(c) =>
					ParseError::nom_error(input, ParseErrorKind::InvalidChar),
				_ => ParseError::nom_error(input, ParseErrorKind::CharUnclosed)
			}
		},
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn string_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('"') =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, nom::error::ErrorKind::Tag)) => // the last `"` match failed
			ParseError::nom_error(input, ParseErrorKind::StringUnclosed),
		nom::Err::Error((input, _)) => // a 
			ParseError::nom_error(input, ParseErrorKind::InvalidEscapedString),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn number_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::MapRes)) =>
			ParseError::nom_error(input, ParseErrorKind::NumberTooBig),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn ident_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ParseError::nom_error(input, ParseErrorKind::KeywordIdentifier),
		nom::Err::Error((input, _)) if input.to_str().is_empty() =>
			ParseError::nom_error(input, ParseErrorKind::Eof),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn multi_comment_error<'a>(full_input: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, nom::error::ErrorKind)>) -> nom::Err<(Input<'a>, ParseError)> {
	move |error|
	match error {
		nom::Err::Error((_, nom::error::ErrorKind::TakeUntil)) =>
			ParseError::nom_error(full_input, ParseErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn inline_comment_error<'a>(full_input: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, nom::error::ErrorKind)>) -> nom::Err<(Input<'a>, ParseError)> {
	move |error|
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ParseError::nom_error(input, ParseErrorKind::UnexpectedNewline),
		nom::Err::Error((_, nom::error::ErrorKind::TakeUntil)) =>
			ParseError::nom_error(full_input, ParseErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn single_comment_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn newline_error<'a>(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedNewline),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn reserved_error(keyword: &'static str) -> impl Fn(nom::Err<(Input<'_>, nom::error::ErrorKind)>) -> nom::Err<(Input<'_>, ParseError)> {
	move |error|
	match error {
		// `true` and `false` are expressions, not keywords
		nom::Err::Error((input, _)) if keyword == "true" || keyword == "false" =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::ExpectedKeyword {
				keyword,
				line: input.line(),
				column: input.column(),
			}),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}