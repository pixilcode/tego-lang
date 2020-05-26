use crate::parser::Input;
use nom::error::ErrorKind as NomErrorKind;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
    column: usize,
    line: usize,
    kind: ErrorKind,
}

macro_rules! error_type {
    ( $name:ident, $kind:expr ) => {
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, _)) => ParseError::new(input, $kind),
                e @ nom::Err::Incomplete(_) => e,
                e @ nom::Err::Failure((_, _)) => e,
            }
        }
    };

    ( $name:ident, $kind:expr; $param_name:ident : $param_type:ty ) => {
        pub fn $name($param_name: $param_type) -> impl Fn(nom::Err<(Input<'_>, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
            move |error|
            match error {
                nom::Err::Error((input, _)) => ParseError::new(input, $kind),
                e @ nom::Err::Incomplete(_) => e,
                e @ nom::Err::Failure((_, _)) => e,
            }
        }
    };

    
    ( [ $name:ident, $kind:expr ] $( $error:pat => $result:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                $( $error => $result, )+
                nom::Err::Error((input, _)) => ParseError::new(input, $kind),
                e @ nom::Err::Incomplete(_) => e,
                e @ nom::Err::Failure((_, _)) => e,
            }
        }
    };

    ( token [ $name:ident ] $( $reserved:literal => $kind:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, error)) =>
                    match error.kind {
                        $( ErrorKind::Reserved($reserved) => ParseError::new(input, $kind), )+
                        _ => ParseError::new_with(input, error)
                    }
                e @ nom::Err::Incomplete(_) => e,
                e @ nom::Err::Failure((_, _)) => e,
            }
        }
    }
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

    fn new_with(input: Input<'_>, error: Self) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, error))
    }
}

// Token Errors
error_type!(reserved_error, ErrorKind::Reserved(token); token: &'static str);
error_type!(char_error, ErrorKind::Char);
error_type!(string_error, ErrorKind::String);
error_type!(number_error, ErrorKind::Number);
error_type!(ident_error, ErrorKind::Identifier);

// Expr Errors
error_type! {
    [literal_error, ErrorKind::Literal]
    nom::Err::Error((input, _)) =>
        if input.to_str().starts_with('"') {
            ParseError::new(input, ErrorKind::String)
        } else if input.to_str().starts_with('\'') {
            ParseError::new(input, ErrorKind::Char)
        } else if input.to_str().starts_with(|c: char| c.is_digit(10)) {
            ParseError::new(input, ErrorKind::Number)
        } else if input.to_str().starts_with(char::is_alphabetic) {
            ParseError::new(input, ErrorKind::Identifier)
        } else {
            ParseError::new(input, ErrorKind::Literal)
        }
}
error_type!(terminating_paren_error, ErrorKind::TerminatingParen);
error_type! {
    token [fn_expr_error]
    "->" => ErrorKind::FnArrow
}
error_type! {
    token [match_arm_error]
    "|" => ErrorKind::MatchBar,
    "->" => ErrorKind::MatchArrow
}
error_type! {
    token [match_head_error]
    "to" => ErrorKind::MatchTo
}
error_type! {
    token [if_cond_error]
    "then" => ErrorKind::Then,
    "?" => ErrorKind::Then
}
error_type! {
    token [if_body_error]
    "else" => ErrorKind::Else,
    ":" => ErrorKind::Else
}
error_type! {
    token [let_assign_error]
    "=" => ErrorKind::LetAssign,
    "in" => ErrorKind::LetIn
}
error_type! {
    token [delay_assign_error]
    "=" => ErrorKind::DelayAssign,
    "in" => ErrorKind::DelayIn
}

// Match Errors
error_type!(ident_match_error, ErrorKind::IdentifierMatch);
error_type! {
    [basic_match_error, ErrorKind::BasicMatch]
    nom::Err::Error((input, _)) =>
        if input.to_str().starts_with('"') {
            ParseError::new(input, ErrorKind::StringMatch)
        } else if input.to_str().starts_with('\'') {
            ParseError::new(input, ErrorKind::CharMatch)
        } else if input.to_str().starts_with(|c: char| c.is_digit(10)) {
            ParseError::new(input, ErrorKind::NumberMatch)
        } else if input.to_str().starts_with(char::is_alphabetic) {
            ParseError::new(input, ErrorKind::IdentifierMatch)
        } else {
            ParseError::new(input, ErrorKind::BasicMatch)
        }
}
error_type!(grouping_match_error, ErrorKind::TerminatingParenMatch);

// Decl Errors
error_type! {
    token [decl_expr_error]
    "=" => ErrorKind::DeclAssign
}

// Other Errors
error_type!(newline_error, ErrorKind::TerminatingNewline);


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

    fn append(_: Input, _: NomErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum ErrorKind {
    // Token Errors
    Reserved(&'static str),
    Char,
    String,
    Number,
    Identifier,

    // Expr Errors
    Literal,
    TerminatingParen,
    FnArrow,
    MatchBar,
    MatchArrow,
    MatchTo,
    Then,
    Else,
    LetAssign,
    LetIn,
    DelayAssign,
    DelayIn,

    // Match Errors
    BasicMatch,
    StringMatch,
    CharMatch,
    NumberMatch,
    IdentifierMatch,
    TerminatingParenMatch,

    // Decl Errors
    DeclAssign,

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
