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
error_type!(literal_error, ErrorKind::Literal);
error_type!(match_arm_error, ErrorKind::MatchArm);
error_type!(match_head_error, ErrorKind::MatchHead);
error_type!(if_cond_error, ErrorKind::IfCond);
error_type!(if_body_error, ErrorKind::IfBody);
error_type!(let_assign_error, ErrorKind::LetAssign);
error_type!(delay_assign_error, ErrorKind::DelayAssign);
error_type!(ident_match_error, ErrorKind::IdentMatch);
error_type!(basic_match_error, ErrorKind::BasicMatch);
error_type!(grouping_match_error, ErrorKind::GroupingMatch);
error_type!(decl_expr_error, ErrorKind::DeclExpr);

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
    // Token Errors
    Reserved(&'static str),
    Char,
    String,
    Number,
    Identifier,

    // Expr Errors
    Literal,
    MatchArm,
    MatchHead,
    IfCond,
    IfBody,
    LetAssign,
    DelayAssign,

    // Match Errors
    IdentMatch,
    BasicMatch,
    GroupingMatch,

    // Decl Errors
    DeclExpr,

    // Other Errors
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
