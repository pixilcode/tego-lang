use crate::Input;
use crate::parsers::tokens;

#[derive(Debug, Clone, PartialEq)]
pub struct ScanError {
	column: usize,
	line: usize,
	kind: ScanErrorKind,
}

impl ScanError {
	fn new(location: Input, kind: ScanErrorKind) -> Self {
		Self {
			column: location.column(),
			line: location.line(),
			kind
		}
	}

	fn nom_error(input: Input, kind: ScanErrorKind) -> nom::Err<(Input, ScanError)> {
		nom::Err::Error((input, ScanError::new(input, kind)))
	}

	pub fn new_from(column: usize, line: usize, kind: ScanErrorKind) -> Self {
		Self {column, line, kind}
	}
}

pub fn char_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('\'') =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Error((input, _)) => {
			let c = input.to_str().chars().nth(1);
			match c {
				Some(c) if c == '\\' =>
					ScanError::nom_error(input, ScanErrorKind::InvalidEscapedChar),
				Some(c) if tokens::INVALID_CHARS.contains(&c) =>
					ScanError::nom_error(input, ScanErrorKind::InvalidChar),
				_ => ScanError::nom_error(input, ScanErrorKind::CharUnclosed)
			}
		},
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn string_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('"') =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Error((input, nom::error::ErrorKind::Tag)) => // the last `"` match failed
			ScanError::nom_error(input, ScanErrorKind::StringUnclosed),
		nom::Err::Error((input, _)) => // a 
			ScanError::nom_error(input, ScanErrorKind::InvalidEscapedString),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn number_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::MapRes)) =>
			ScanError::nom_error(input, ScanErrorKind::NumberTooBig),
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn ident_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ScanError::nom_error(input, ScanErrorKind::KeywordIdentifier),
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn multi_comment_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Tag)) if !input.to_str().starts_with("{-") =>
			ScanError::nom_error(input, ScanErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn inline_comment_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ScanError::nom_error(input, ScanErrorKind::UnexpectedNewline),
		nom::Err::Error((input, nom::error::ErrorKind::Tag)) if !input.to_str().starts_with("{-") =>
			ScanError::nom_error(input, ScanErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn single_comment_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn newline_error_scan<'a>(error: nom::Err<(Input, ScanError)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::ExpectedNewline),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn reserved_error_scan(token: &'static str) -> impl Fn(nom::Err<(Input<'_>, nom::error::ErrorKind)>) -> nom::Err<(Input<'_>, ScanError)> {
	move |error|
	match error {
		// `true` and `false` are expressions, not keywords
		nom::Err::Error((input, _)) if token == "true" || token == "false" =>
			ScanError::nom_error(input, ScanErrorKind::NoMatch),
		nom::Err::Error((input, _)) =>
			ScanError::nom_error(input, ScanErrorKind::ExpectedKeyword(token)),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

impl<'a> nom::error::ParseError<Input<'a>> for (Input<'a>, ScanError) {
    fn from_error_kind(input: Input<'a>, kind: nom::error::ErrorKind) -> Self {
        (
            input,
            ScanError {
                column: input.column(),
                line: input.line(),
                kind: ScanErrorKind::UnhandledNomError,
            },
        )
    }

    fn append(_: Input, _: nom::error::ErrorKind, other: Self) -> Self {
        // All errors should already be handled...
		// I need to figure out how to use this
		// effectively
		other
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScanErrorKind {
	// CHAR ERRORS
	CharUnclosed,
	InvalidChar,
	InvalidEscapedChar,

	// STRING ERRORS
	StringUnclosed,
	InvalidEscapedString,

	// NUMBER ERRORS
	NumberTooBig,

	// IDENTIFIER ERRORS
	KeywordIdentifier,

	// COMMENT ERRORS
	UnexpectedNewline,
	UnclosedComment,

	// WHITESPACE ERRORS
	ExpectedNewline,

	// NO MATCH ERRORS
	NoMatch,
	ExpectedKeyword(&'static str),

	// NOM ERRORS
	UnknownFailure,
	UnhandledNomError,
}