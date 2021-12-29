use crate::parsers::tokens::{newlines, token};
use crate::{Input, ParseResult};
use crate::error::scanner::{ScanError, ScanErrorKind};
use std::fmt;
use std::io;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
    column: usize,
    line: usize,
    kind: ParseErrorKind,
}

impl ParseError {
    fn is_unhandled(&self) -> bool {
        matches!(self.kind, 
            ParseErrorKind::Reserved(_)
            | ParseErrorKind::Char
            | ParseErrorKind::String
            | ParseErrorKind::Number
            | ParseErrorKind::Keyword
            | ParseErrorKind::UnknownNomError
        )
    }

    pub fn new(column: usize, line: usize, kind: ParseErrorKind) -> Self {
        Self {
            column, line, kind,
        }
    }

    pub fn new_from_kind(input: Input<'_>, kind: ParseErrorKind) -> Self {
        Self {
            column: input.column(),
            line: input.line(),
            kind,
        }
    }

    fn new_from(input: Input<'_>, error: Self, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, ParseError { kind, ..error }))
    }

    fn new_with(input: Input<'_>, error: Self) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, error))
    }

    pub fn verbose_from_source(&self, source: &str, writer: &mut impl io::Write) -> io::Result<()> {
        writeln!(writer, "{}", self)?;
        writeln!(writer)?;
        match self.kind {
            ParseErrorKind::TerminatingParen(line, column) => {
                writeln!(writer, "    |")?;
                writeln!(
                    writer,
                    "{:>3} | {}",
                    self.line,
                    source.lines().nth(self.line - 1).unwrap_or("")
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
                    source.lines().nth(line - 1).unwrap_or("")
                )?;
                writeln!(
                    writer,
                    "    | {:>1$} opening parenthesis found here",
                    "^",
                    column - 1
                )?;
            }
            ParseErrorKind::TerminatingBracket(line, column) => {
                writeln!(writer, "    |")?;
                writeln!(
                    writer,
                    "{:>3} | {}",
                    self.line,
                    source.lines().nth(self.line - 1).unwrap_or("")
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
                    source.lines().nth(line - 1).unwrap_or("")
                )?;
                writeln!(
                    writer,
                    "    | {:>1$} opening bracket found here",
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
                    source.lines().nth(self.line - 1).unwrap_or("")
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
    fn from_error_kind(input: Input<'a>, kind: nom::error::ErrorKind) -> Self {
        (
            input,
            ParseError {
                column: input.column(),
                line: input.line(),
                kind: kind.into(),
            },
        )
    }

    fn append(_: Input, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error = match self.kind {
            // Token Errors
            ParseErrorKind::Reserved(reserved) => format!("missing '{}' token", reserved),
            ParseErrorKind::Char => "error parsing character literal".into(),
            ParseErrorKind::String => "error parsing string literal".into(),
            ParseErrorKind::Number => "error parsing number literal".into(),
            ParseErrorKind::Keyword => "found keyword where an identifier was expected".into(),

            // Expr Errors
            ParseErrorKind::InvalidCharacter => "encountered invalid character".into(),
            ParseErrorKind::TerminatingParen(_, _) => "missing closing parenthesis".into(),
            ParseErrorKind::TerminatingBracket(_, _) => "missing closing bracket".into(),
            ParseErrorKind::FnArrow => {
                "missing '->' between function parameters and function body".into()
            }
            ParseErrorKind::MatchBar => "missing '|' before match arm".into(),
            ParseErrorKind::MatchArrow => "missing '->' between match pattern and match body".into(),
            ParseErrorKind::MatchTo => "missing 'to' between match head and body".into(),
            ParseErrorKind::Then => "missing 'then' or '?' after if condition".into(),
            ParseErrorKind::Else => "missing 'else' in if expression".into(),
            ParseErrorKind::LetAssign => "missing '=' in let assignment".into(),
            ParseErrorKind::LetIn => "missing 'in' in let expression".into(),
            ParseErrorKind::DelayAssign => "missing '=' in delay assignment".into(),
            ParseErrorKind::DelayIn => "missing 'in' in delay expression".into(),
            ParseErrorKind::EndOfExpr => "unexpected end of expr".into(),
            ParseErrorKind::DoIn => "missing 'in' in do expression".into(),
            ParseErrorKind::DoThen => "missing 'then' in do expression".into(),

            // Decl Errors
            ParseErrorKind::DeclAssign => "missing '=' in expression declaration".into(),

            // Other Errors
            ParseErrorKind::TerminatingNewline => "missing newline (expected here)".into(),
            ParseErrorKind::Eof => "reached end of file before parsing was completed".into(),
            ParseErrorKind::Incomplete => "incomplete information found".into(),
            ParseErrorKind::UnknownNomError => "unknown error from parsing".into(),
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

impl std::error::Error for ParseError {}

impl<'a> From<nom::Err<(Input<'a>, ParseError)>> for ParseError {
    fn from(error: nom::Err<(Input, ParseError)>) -> Self {
        match error {
            nom::Err::Incomplete(_) => ParseError {
                column: 1,
                line: 1,
                kind: ParseErrorKind::Incomplete,
            },
            nom::Err::Error((_, error)) | nom::Err::Failure((_, error)) => error,
        }
    }
}

impl From<ScanError> for ParseError {
    fn from(error: ScanError) -> Self {
        Self {
            column: error.column(),
            line: error.line(),
            kind: ParseErrorKind::TokenError(error.kind()),
        }
    }
}

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
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from(input, error, ParseErrorKind::UnhandledError),
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
                        $( ParseErrorKind::Reserved($reserved) => ParseError::new_from(input, error, $kind), )+
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
                        ParseError::new_from(input, error, ParseErrorKind::EndOfExpr),
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
pub fn try_parser<'a, F, O>(
    parser: F,
    input: Input<'a>,
) -> impl Fn(nom::Err<(Input<'a>, ParseError)>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    move |error| match error {
        nom::Err::Error((input, error)) if !error.is_unhandled() => {
            Err(nom::Err::Error((input, error)))
        }
        _ => parser(input),
    }
}

// Token Errors
error_type!(reserved_error, ParseErrorKind::Reserved(token); token: &'static str);
error_type!(char_error, ParseErrorKind::Char);
error_type!(string_error, ParseErrorKind::String);
error_type!(number_error, ParseErrorKind::Number);
error_type!(ident_error, ParseErrorKind::Keyword);

// Expr Errors
error_type! {
    starts_with [literal_error, ParseErrorKind::InvalidCharacter]
    '"' => ParseErrorKind::String,
    '\'' => ParseErrorKind::Char,
    |c: char| c.is_digit(10) => ParseErrorKind::Number,
    char::is_alphabetic => ParseErrorKind::Keyword
}
error_type!(terminating_paren_error, ParseErrorKind::TerminatingParen(open_paren_loc.0, open_paren_loc.1); open_paren_loc: (usize, usize));
error_type!(terminating_bracket_error, ParseErrorKind::TerminatingBracket(open_bracket_loc.0, open_bracket_loc.1); open_bracket_loc: (usize, usize));
error_type! {
    token [fn_expr_error]
    "->" => ParseErrorKind::FnArrow
}
error_type! {
    token [match_arm_error]
    "|" => ParseErrorKind::MatchBar,
    "->" => ParseErrorKind::MatchArrow
}
error_type! {
    token [match_head_error]
    "to" => ParseErrorKind::MatchTo
}
error_type! {
    token [if_cond_error]
    "then" => ParseErrorKind::Then,
    "?" => ParseErrorKind::Then
}
error_type! {
    token [if_body_error]
    "else" => ParseErrorKind::Else,
    ":" => ParseErrorKind::Else
}
error_type! {
    token [let_assign_error]
    "=" => ParseErrorKind::LetAssign,
    "in" => ParseErrorKind::LetIn
}
error_type! {
    token [delay_assign_error]
    "=" => ParseErrorKind::DelayAssign,
    "in" => ParseErrorKind::DelayIn
}
error_type! {
    token [do_error]
    "in" => ParseErrorKind::DoIn,
    "then" => ParseErrorKind::DoThen
}

// Match Errors
pub fn ident_match_error(error: nom::Err<(Input, ScanError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, error))
            if error.is_partial_match() => nom::Err::Error((input, error.into())),
        nom::Err::Error((input, error))
            if input.to_str().is_empty() => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::Eof))),
        nom::Err::Error((input, _)) => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::NoMatch))),
        nom::Err::Failure((input, error)) => nom::Err::Error((input, error.into())),
        nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
    }
}

error_type! {
    starts_with [basic_match_error, ParseErrorKind::InvalidCharacter]
    '"' => ParseErrorKind::String,
    '\'' => ParseErrorKind::Char,
    |c: char| c.is_digit(10) => ParseErrorKind::Number,
    char::is_alphabetic => ParseErrorKind::Keyword
}
error_type!(grouping_match_error, ParseErrorKind::TerminatingParen(open_paren_loc.0, open_paren_loc.1); open_paren_loc: (usize, usize));

// Decl Errors
error_type! {
    token [decl_expr_error]
    "=" => ParseErrorKind::DeclAssign
}

// Other Errors
error_type!(newline_error, ParseErrorKind::TerminatingNewline);

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ParseErrorKind {
    // Token Errors
    TokenError(ScanErrorKind),
    Reserved(&'static str),
    Char,
    String,
    Number,
    Keyword,
    InvalidCharacter,

    // Expr Errors
    TerminatingParen(usize, usize),
    TerminatingBracket(usize, usize),
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
    DoIn,
    DoThen,

    // Decl Errors
    DeclAssign,

    // Other Errors
    TerminatingNewline,
    Eof,
    NoMatch,
    Incomplete,
    UnknownNomError,
}

impl From<nom::error::ErrorKind> for ParseErrorKind {
    fn from(error: nom::error::ErrorKind) -> Self {
        match error {
            nom::error::ErrorKind::Eof => ParseErrorKind::Eof,
            _ => ParseErrorKind::UnknownNomError,
        }
    }
}

impl From<ParseErrorKind> for u16 {
    fn from(error: ParseErrorKind) -> Self {
        u16::from(&error)
    }
}

// Error code (between 1 and 100 for parse errors)
impl From<&ParseErrorKind> for u16 {
    fn from(error: &ParseErrorKind) -> Self {
        match error {
            ParseErrorKind::Reserved(_) => 1,
            ParseErrorKind::Char => 2,
            ParseErrorKind::String => 3,
            ParseErrorKind::Number => 4,
            ParseErrorKind::Keyword => 5,
            ParseErrorKind::InvalidCharacter => 6,
            ParseErrorKind::TerminatingParen(_, _) => 7,
            ParseErrorKind::FnArrow => 8,
            ParseErrorKind::MatchBar => 9,
            ParseErrorKind::MatchArrow => 10,
            ParseErrorKind::MatchTo => 11,
            ParseErrorKind::Then => 12,
            ParseErrorKind::Else => 13,
            ParseErrorKind::LetAssign => 14,
            ParseErrorKind::LetIn => 15,
            ParseErrorKind::DelayAssign => 16,
            ParseErrorKind::DelayIn => 17,
            ParseErrorKind::DeclAssign => 18,
            ParseErrorKind::TerminatingNewline => 19,
            ParseErrorKind::Eof => 20,
            ParseErrorKind::UnknownNomError => 21,
            ParseErrorKind::EndOfExpr => 22,
            ParseErrorKind::TerminatingBracket(_, _) => 23,
            ParseErrorKind::Incomplete => 24,
            ParseErrorKind::DoIn => 25,
            ParseErrorKind::DoThen => 26,
        }
    }
}