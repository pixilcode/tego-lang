use crate::Input;
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
	println!("=> YAY {:?}", error);
	match error {
		nom::Err::Error((input, error)) => {
			if (error == NomErrorKind::Tag || error == NomErrorKind::Eof) && input.to_str().starts_with('\'') {
				nom::Err::Error((input, ScanError::new(input, ScanErrorKind::CharUnclosed)))
			} else if error == NomErrorKind::Tag || error == NomErrorKind::Eof {
				todo!("figure this out")
			} else {
				todo!("figure this out, too")
			}
		},
		nom::Err::Failure((input, error)) => todo!("fill this in"),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScanErrorKind {
	CharUnclosed,
}