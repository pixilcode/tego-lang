use crate::parser::{Input, ParseResult};
use crate::parser::tokens::{newlines, token};
use nom::error::ErrorKind as NomErrorKind;
use std::fmt;
use std::io;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
    column: usize,
    line: usize,
    kind: ErrorKind,
}

impl ParseError {
    fn is_unhandled(&self) -> bool {
        match self.kind {
            ErrorKind::Reserved(_)
            | ErrorKind::Char
            | ErrorKind::String
            | ErrorKind::Number
            | ErrorKind::Keyword
            | ErrorKind::UnknownNomError
            | ErrorKind::UnhandledError => true,
            _ => false,
        }
    }

    fn new_from(input: Input<'_>, error: Self, kind: ErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, ParseError { kind, ..error }))
    }

    fn new_with(input: Input<'_>, error: Self) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, error))
    }

    pub fn verbose_from_source(&self, source: &str, writer: &mut impl io::Write) -> io::Result<()> {
        writeln!(writer, "{}", self)?;
        writeln!(writer)?;
        match self.kind {
            ErrorKind::TerminatingParen(line, column) => {
                writeln!(writer, "    |")?;
                writeln!(
                    writer,
                    "{:>3} | {}",
                    self.line,
                    source
                        .lines()
                        .nth(self.line - 1)
                        .unwrap_or("")
                )?;
                writeln!(
                    writer,
                    "    | {:>1$} error found here",
                    "^",
                    self.column - 1
                )?;
                writeln!(writer)?;
                writeln!(writer, "    |")?;
                writeln!(
                    writer,
                    "{:>3} | {}",
                    line,
                    source
                        .lines()
                        .nth(line - 1)
                        .unwrap_or("")
                )?;
                writeln!(
                    writer,
                    "    | {:>1$} opening parenthesis found here",
                    "^",
                    column - 1
                )?;
            }
            _ => {
                writeln!(writer, "    |")?;
                writeln!(
                    writer,
                    "{:>3} | {}",
                    self.line,
                    source
                        .lines()
                        .nth(self.line - 1)
                        .unwrap_or("")
                )?;
                writeln!(
                    writer,
                    "    | {:>1$} error found here",
                    "^",
                    self.column - 1
                )?;
            }
        }
        writeln!(writer)?;
        Ok(())
    }
}

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

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error = match self.kind {
            // Token Errors
            ErrorKind::Reserved(reserved) => format!("missing '{}' token", reserved),
            ErrorKind::Char => "error parsing character literal".into(),
            ErrorKind::String => "error parsing string literal".into(),
            ErrorKind::Number => "error parsing number literal".into(),
            ErrorKind::Keyword => "found keyword where an identifier was expected".into(),

            // Expr Errors
            ErrorKind::InvalidCharacter => "encountered invalid character".into(),
            ErrorKind::TerminatingParen(_, _) => "missing closing parenthesis".into(),
            ErrorKind::FnArrow => {
                "missing '->' between function parameters and function body".into()
            }
            ErrorKind::MatchBar => "missing '|' before match arm".into(),
            ErrorKind::MatchArrow => "missing '->' between match pattern and match body".into(),
            ErrorKind::MatchTo => "missing 'to' between match head and body".into(),
            ErrorKind::Then => "missing 'then' or '?' after if condition".into(),
            ErrorKind::Else => "missing 'else' in if expression".into(),
            ErrorKind::LetAssign => "missing '=' in let assignment".into(),
            ErrorKind::LetIn => "missing 'in' in let expression".into(),
            ErrorKind::DelayAssign => "missing '=' in delay assignment".into(),
            ErrorKind::DelayIn => "missing 'in' in delay expression".into(),
            ErrorKind::EndOfExpr => "unexpected end of expr".into(),

            // Decl Errors
            ErrorKind::DeclAssign => "missing '=' in expression declaration".into(),

            // Other Errors
            ErrorKind::TerminatingNewline => "missing newline (expected here)".into(),
            ErrorKind::Eof => "reached end of file before parsing was completed".into(),
            ErrorKind::UnknownNomError => "unknown error from parsing".into(),
            ErrorKind::UnhandledError => "unhandled parsing error".into(),
        };
        write!(
            f,
            "error[E{:04}]: {} (found on line {}, column {})",
            u16::from(self.kind),
            error,
            self.line,
            self.column
        )
    }
}

impl<'a> std::error::Error for ParseError {}

macro_rules! error_type {
    ( $name:ident, $kind:expr ) => {
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from(input, error, $kind),
                e => e
            }
        }
    };

    ( $name:ident, $kind:expr; $param_name:ident : $param_type:ty ) => {
        pub fn $name($param_name: $param_type) -> impl Fn(nom::Err<(Input<'_>, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
            move |error|
            match error {
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from(input, error, $kind),
                e => e
            }
        }
    };


    ( [ $name:ident ] $( $error:pat $( if $cond:expr )? => $result:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                $( $error $( if $cond:expr )? => $result, )+
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from(input, error, ErrorKind::UnhandledError),
                e => e
            }
        }
    };

    ( token [ $name:ident ] $( $reserved:literal => $kind:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, error)) if error.is_unhandled() =>
                    match error.kind {
                        $( ErrorKind::Reserved($reserved) => ParseError::new_from(input, error, $kind), )+
                        _ => ParseError::new_with(input, error)
                    },
                e => e
            }
        }
    };

    ( starts_with [ $name:ident $( , $default_kind:expr)? ] $( $pattern:expr => $kind:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, error))
                    if input.to_str().is_empty() || token(newlines(true))(input).is_ok() =>
                        ParseError::new_from(input, error, ErrorKind::EndOfExpr),
                $( nom::Err::Error((input, error))
                    if error.is_unhandled() && input.to_str().starts_with($pattern) =>
                        ParseError::new_from(input, error, $kind), )+
                $( nom::Err::Error((input, error)) if error.is_unhandled() =>
                    ParseError::new_from(input, error, $default_kind), )?
                e => e
            }
        }
    }
}

// Error handlers

// Try a different parser
pub fn try_parser<'a, F, O>(parser: F, input: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, ParseError)>) -> ParseResult<'a, O>
where F: Fn(Input<'a>) -> ParseResult<'a, O> {
    move |error|
    match error {
        nom::Err::Error((input, error)) if !error.is_unhandled() => Err(nom::Err::Error((input, error))),
        _ => parser(input)
    }
}

// Token Errors
error_type!(reserved_error, ErrorKind::Reserved(token); token: &'static str);
error_type!(char_error, ErrorKind::Char);
error_type!(string_error, ErrorKind::String);
error_type!(number_error, ErrorKind::Number);
error_type!(ident_error, ErrorKind::Keyword);

// Expr Errors
error_type! {
    starts_with [literal_error, ErrorKind::InvalidCharacter]
    '"' => ErrorKind::String,
    '\'' => ErrorKind::Char,
    |c: char| c.is_digit(10) => ErrorKind::Number,
    char::is_alphabetic => ErrorKind::Keyword
}
error_type!(terminating_paren_error, ErrorKind::TerminatingParen(open_paren_loc.0, open_paren_loc.1); open_paren_loc: (usize, usize));
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
error_type!(ident_match_error, ErrorKind::Keyword);
error_type! {
    starts_with [basic_match_error, ErrorKind::InvalidCharacter]
    '"' => ErrorKind::String,
    '\'' => ErrorKind::Char,
    |c: char| c.is_digit(10) => ErrorKind::Number,
    char::is_alphabetic => ErrorKind::Keyword
}
error_type!(grouping_match_error, ErrorKind::TerminatingParen(open_paren_loc.0, open_paren_loc.1); open_paren_loc: (usize, usize));

// Decl Errors
error_type! {
    token [decl_expr_error]
    "=" => ErrorKind::DeclAssign
}

// Other Errors
error_type!(newline_error, ErrorKind::TerminatingNewline);

#[derive(PartialEq, Debug, Clone, Copy)]
enum ErrorKind {
    // Token Errors
    Reserved(&'static str),
    Char,
    String,
    Number,
    Keyword,
    InvalidCharacter,

    // Expr Errors
    TerminatingParen(usize, usize),
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
    EndOfExpr,

    // Decl Errors
    DeclAssign,

    // Other Errors
    TerminatingNewline,
    Eof,
    UnknownNomError,
    UnhandledError,
}

impl From<NomErrorKind> for ErrorKind {
    fn from(error: NomErrorKind) -> Self {
        match error {
            NomErrorKind::Eof => ErrorKind::Eof,
            _ => ErrorKind::UnknownNomError,
        }
    }
}

impl From<ErrorKind> for u16 {
    fn from(error: ErrorKind) -> Self {
        u16::from(&error)
    }
}

// Error code (between 1 and 100 for parse errors)
impl From<&ErrorKind> for u16 {
    fn from(error: &ErrorKind) -> Self {
        match error {
            ErrorKind::Reserved(_) => 1,
            ErrorKind::Char => 2,
            ErrorKind::String => 3,
            ErrorKind::Number => 4,
            ErrorKind::Keyword => 5,
            ErrorKind::InvalidCharacter => 6,
            ErrorKind::TerminatingParen(_, _) => 7,
            ErrorKind::FnArrow => 8,
            ErrorKind::MatchBar => 9,
            ErrorKind::MatchArrow => 10,
            ErrorKind::MatchTo => 11,
            ErrorKind::Then => 12,
            ErrorKind::Else => 13,
            ErrorKind::LetAssign => 14,
            ErrorKind::LetIn => 15,
            ErrorKind::DelayAssign => 16,
            ErrorKind::DelayIn => 17,
            ErrorKind::DeclAssign => 18,
            ErrorKind::TerminatingNewline => 19,
            ErrorKind::Eof => 20,
            ErrorKind::UnknownNomError => 21,
            ErrorKind::UnhandledError => 22,
            ErrorKind::EndOfExpr => 23
        }
    }
}
