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
		nom::Err::Error((input, _)) => // 
			ScanError::nom_error(input, ScanErrorKind::InvalidEscapedString),
		nom::Err::Failure((input, _)) =>
			nom::Err::Failure((input, ScanError::new(input, ScanErrorKind::UnknownFailure))),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
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

	// NO MATCH ERROR
	NoMatch,

	// FAILURE ERROR
	UnknownFailure,
}