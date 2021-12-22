use crate::Input;
use crate::parsers::tokens;
use nom::error::ErrorKind as NomErrorKind;

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

	pub fn new_from(column: usize, line: usize, kind: ScanErrorKind) -> Self {
		Self {column, line, kind}
	}
}

pub fn char_error_scan(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ScanError)> {
	match error {
		nom::Err::Error((input, _)) => {

			if !input.to_str().starts_with('\'') {
				return nom::Err::Error((input, ScanError::new(input, ScanErrorKind::NoMatch)));
			}

			let c = input.to_str().chars().nth(1);
			match c {
				Some(c) if c == '\\' =>
					nom::Err::Error((input, ScanError::new(input, ScanErrorKind::InvalidEscapedChar))),
				Some(c) if tokens::INVALID_CHARS.contains(&c) =>
					nom::Err::Error((input, ScanError::new(input, ScanErrorKind::InvalidChar))),
				_ => nom::Err::Error((input, ScanError::new(input, ScanErrorKind::CharUnclosed)))
			}
		},
		nom::Err::Failure((input, error)) => todo!("fill this in"),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScanErrorKind {
	// CHAR ERRORS
	CharUnclosed,
	InvalidChar,
	InvalidEscapedChar,

	// NO MATCH ERROR
	NoMatch,
}