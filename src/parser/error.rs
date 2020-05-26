use crate::parser::Input;
use nom::error::ErrorKind as NomErrorKind;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
    column: usize,
    line: usize,
    kind: ErrorKind,
}

macro_rules! error_type {
    ($name:ident, $kind:expr) => {
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
                nom::Err::Error((input, _)) => ParseError::new(input, $kind),
                nom::Err::Failure((input, _)) => ParseError::new(input, $kind),
            }
        }
    };

    ($name:ident, $kind:expr; $param_name:ident : $param_type:ty) => {
        pub fn $name($param_name: $param_type) -> impl Fn(nom::Err<(Input<'_>, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
            move |error|
            match error {
                nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
                nom::Err::Error((input, _)) => ParseError::new(input, $kind),
                nom::Err::Failure((input, _)) => ParseError::new(input, $kind),
            }
        }
    };
}

impl ParseError {
    fn new(input: Input<'_>, kind: ErrorKind) -> nom::Err<(Input<'_>, Self)> {
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

error_type!(reserved_error, ErrorKind::Reserved(token); token: &'static str);
error_type!(char_error, ErrorKind::Char);
error_type!(newline_error, ErrorKind::TerminatingNewline);
error_type!(string_error, ErrorKind::String);
error_type!(number_error, ErrorKind::Number);
error_type!(ident_error, ErrorKind::Identifier);

impl<'a> nom::error::ParseError<Input<'a>> for (Input<'a>, ParseError) {
    fn from_error_kind(input: Input<'a>, kind: NomErrorKind) -> Self {
        (
            input,
            ParseError {
                column: input.column(),
                line: input.line(),
                kind: kind.into(),
            },
        )
    }

    fn append(_input: Input, _kind: NomErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(PartialEq, Debug, Clone)]
enum ErrorKind {
    Reserved(&'static str),
    Char,
    String,
    Number,
    Identifier,
    TerminatingNewline,
    Eof,
    UnknownNomError,
}

impl From<NomErrorKind> for ErrorKind {
    fn from(e: NomErrorKind) -> Self {
        match e {
            NomErrorKind::Eof => ErrorKind::Eof,
            _ => ErrorKind::UnknownNomError
        }
    }
}
