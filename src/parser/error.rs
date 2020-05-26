use crate::parser::Input;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
	column: usize,
	line: usize,
	kind: ErrorKind,
}

macro_rules! error_type {
	($name:ident, $kind:expr, $error:ty) => {
		pub fn $name<'a>(error: nom::Err<(Input<'a>, $error)>) -> nom::Err<(Input<'a>, ParseError)> {
			match error {
				nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
				nom::Err::Error((input, _)) => ParseError::new(input, $kind),
				nom::Err::Failure((input, _)) => ParseError::new(input, $kind),
			}
		}
	};
}

impl ParseError {
	fn new(input: Input, kind: ErrorKind) -> nom::Err<(Input, Self)> {
		nom::Err::Error((
			input,
			ParseError {
				column: input.column(),
				line: input.line(),
				kind,
			},
		))
	}
}

error_type!(token_error, ErrorKind::Token, ParseError);
error_type!(char_error, ErrorKind::Char, ParseError);

impl<'a> nom::error::ParseError<Input<'a>> for (Input<'a>, ParseError) {
	fn from_error_kind(input: Input<'a>, _: nom::error::ErrorKind) -> Self {
		(
			input,
			ParseError {
				column: input.column(),
				line: input.line(),
				kind: ErrorKind::NomError,
			},
		)
	}

	fn append(_: Input, _: nom::error::ErrorKind, other: Self) -> Self {
		other
	}
}

#[derive(PartialEq, Debug, Clone)]
enum ErrorKind {
	Token,
	Char,
	NomError,
}
