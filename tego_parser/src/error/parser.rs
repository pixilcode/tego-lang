use crate::parsers::tokens;
use crate::{Input, ParseResult};
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
            | ParseErrorKind::UnhandledNomError
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

    fn new_from_deprecated(input: Input<'_>, error: Self, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, ParseError { kind, ..error }))
    }

    fn new_with(input: Input<'_>, error: Self) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, error))
    }

    fn nom_error(input: Input<'_>, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, Self::new_from_kind(input, kind)))
    }

    fn nom_failure(input: Input<'_>, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, Self::new_from_kind(input, kind)))
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

            ParseErrorKind::CharUnclosed => todo!("write this"),
            ParseErrorKind::InvalidChar => todo!("write this"),
            ParseErrorKind::InvalidEscapedChar => todo!("write this"),
        
            // STRING ERRORS
            ParseErrorKind::StringUnclosed => todo!("write this"),
            ParseErrorKind::InvalidEscapedString => todo!("write this"),
        
            // NUMBER ERRORS
            ParseErrorKind::NumberTooBig => todo!("write this"),
        
            // IDENTIFIER ERRORS
            ParseErrorKind::KeywordIdentifier => todo!("write this"),
        
            // COMMENT ERRORS
            ParseErrorKind::UnexpectedNewline => todo!("write this"),
            ParseErrorKind::UnclosedComment => todo!("write this"),
        
            // WHITESPACE ERRORS
            ParseErrorKind::ExpectedNewline => todo!("write this"),
        
            // NO MATCH ERRORS
            ParseErrorKind::ExpectedKeyword {
                ..
            } => todo!("write this"),

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

            // Match Errors
            ParseErrorKind::IncompleteTuple(_, _) => todo!("write this"),

            // Decl Errors
            ParseErrorKind::DeclAssign => "missing '=' in expression declaration".into(),

            // Other Errors
            ParseErrorKind::TerminatingNewline => "missing newline (expected here)".into(),
            ParseErrorKind::Eof => "reached end of file before parsing was completed".into(),
            ParseErrorKind::Incomplete => "incomplete information found".into(),
            ParseErrorKind::UnhandledNomError => "unknown error from parsing".into(),
            ParseErrorKind::UnknownFailure => todo!("write this"),
            ParseErrorKind::NoMatch => todo!("write this"),
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

macro_rules! error_type {
    ( $name:ident, $kind:expr ) => {
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from_deprecated(input, error, $kind),
                e => e
            }
        }
    };

    ( $name:ident, $kind:expr; $param_name:ident : $param_type:ty ) => {
        pub fn $name($param_name: $param_type) -> impl Fn(nom::Err<(Input<'_>, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
            move |error|
            match error {
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from_deprecated(input, error, $kind),
                e => e
            }
        }
    };


    ( [ $name:ident ] $( $error:pat $( if $cond:expr )? => $result:expr ),+ ) => {
        #[allow(unreachable_patterns)]
        pub fn $name(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
            match error {
                $( $error $( if $cond:expr )? => $result, )+
                nom::Err::Error((input, error)) if error.is_unhandled() => ParseError::new_from_deprecated(input, error, ParseErrorKind::UnhandledError),
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
                        $( ParseErrorKind::Reserved($reserved) => ParseError::new_from_deprecated(input, error, $kind), )+
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
                    if input.to_str().is_empty() || preceded(space0, newlines(true))(input).is_ok() =>
                        ParseError::new_from_deprecated(input, error, ParseErrorKind::EndOfExpr),
                $( nom::Err::Error((input, error))
                    if error.is_unhandled() && input.to_str().starts_with($pattern) =>
                        ParseError::new_from_deprecated(input, error, $kind), )+
                $( nom::Err::Error((input, error)) if error.is_unhandled() =>
                    ParseError::new_from_deprecated(input, error, $default_kind), )?
                e => e
            }
        }
    }
}

// Error handlers

/// Try a different scanner
pub fn try_parser<'a, P, O>(
    parser: P,
    input: Input<'a>,
) -> impl Fn(nom::Err<(Input<'a>, ParseError)>) -> ParseResult<'a, O>
where
    P: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    move |error| match error {
        failure @ nom::Err::Failure((_, _)) => Err(failure),
        incomplete @ nom::Err::Incomplete(_) => Err(incomplete),
        _ => parser(input),
    }
}

/// Begin a parse phrase (for parse phrases that begin
/// with a keyword).
/// 
/// For example `if 1 == 2 then ...` begins with `if`.
/// 
/// Using the `if_` parser would return a failure
/// if it doesn't match `if`, meaning parsing would terminate
/// if an `if` expression isn't matched.
/// 
/// Normally, we would like to try to parse other expressions
/// such as `match`, so we use `begin_phrase` to turn the failure into an
/// error
pub fn begin_phrase<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
	F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
	move |input|
    parser(input).map_err(|err| match err {
		nom::Err::Failure((input, error))
			if matches!(error.kind, ParseErrorKind::ExpectedKeyword { .. }) =>
			nom::Err::Error((input, ParseError::new_from_kind(
                input, ParseErrorKind::NoMatch,
            ))),
		error => error,
	})
}

// Token Errors

pub fn char_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('\'') =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, _)) => {
			let c = input.to_str().chars().nth(1);
			match c {
				Some(c) if c == '\\' =>
					ParseError::nom_failure(input, ParseErrorKind::InvalidEscapedChar),
				Some(c) if tokens::INVALID_CHARS.contains(&c) =>
					ParseError::nom_failure(input, ParseErrorKind::InvalidChar),
				_ => ParseError::nom_failure(input, ParseErrorKind::CharUnclosed)
			}
		},
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn string_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) if !input.to_str().starts_with('"') =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, nom::error::ErrorKind::Tag)) => // the last `"` match failed
			ParseError::nom_failure(input, ParseErrorKind::StringUnclosed),
		nom::Err::Error((input, _)) => // a 
			ParseError::nom_failure(input, ParseErrorKind::InvalidEscapedString),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn number_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::MapRes)) =>
			ParseError::nom_failure(input, ParseErrorKind::NumberTooBig),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn ident_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ParseError::nom_failure(input, ParseErrorKind::KeywordIdentifier),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn multi_comment_error<'a>(full_input: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, nom::error::ErrorKind)>) -> nom::Err<(Input<'a>, ParseError)> {
    move |error|
	match error {
		nom::Err::Error((_, nom::error::ErrorKind::TakeUntil)) =>
			ParseError::nom_failure(full_input, ParseErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn inline_comment_error<'a>(full_input: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, nom::error::ErrorKind)>) -> nom::Err<(Input<'a>, ParseError)> {
    move |error|
	match error {
		nom::Err::Error((input, nom::error::ErrorKind::Verify)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnexpectedNewline),
		nom::Err::Error((_, nom::error::ErrorKind::TakeUntil)) =>
			ParseError::nom_failure(full_input, ParseErrorKind::UnclosedComment),
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn single_comment_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn newline_error<'a>(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
	match error {
		nom::Err::Error((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedNewline),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

pub fn reserved_error(keyword: &'static str) -> impl Fn(nom::Err<(Input<'_>, nom::error::ErrorKind)>) -> nom::Err<(Input<'_>, ParseError)> {
	move |error|
	match error {
		// `true` and `false` are expressions, not keywords
		nom::Err::Error((input, _)) if keyword == "true" || keyword == "false" =>
			ParseError::nom_error(input, ParseErrorKind::NoMatch),
		nom::Err::Error((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::ExpectedKeyword {
				keyword,
				line: input.line(),
				column: input.column(),
			}),
		nom::Err::Failure((input, _)) =>
			ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
		nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
	}
}

// Expr Errors
pub fn num_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, error)) if matches!(error.kind,
            ParseErrorKind::NumberTooBig
        ) => nom::Err::Failure((input, error)),
        error => error
    }
}

pub fn string_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, error)) if matches!(error.kind,
            ParseErrorKind::StringUnclosed |
            ParseErrorKind::InvalidEscapedString
        ) => nom::Err::Failure((input, error)),
        error => error
    }
}

pub fn char_expr_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, error)) if matches!(error.kind,
            ParseErrorKind::CharUnclosed |
            ParseErrorKind::InvalidChar |
            ParseErrorKind::InvalidEscapedChar
        ) => nom::Err::Failure((input, error)),
        error => error
    }
}

pub fn literal_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, _)) if input.to_str().is_empty() =>
            nom::Err::Error((
                input,
                ParseError::new_from_kind(input, ParseErrorKind::Eof)
            )),
        error => error
    }
}

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
pub fn ident_match_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, _)) 
            if input.to_str().is_empty() => nom::Err::Error((
                input, 
                ParseError::new_from_kind(input, ParseErrorKind::Eof)
            )),
        nom::Err::Error((input, error)) if error.kind == ParseErrorKind::KeywordIdentifier =>
            nom::Err::Failure((input, error.into())),
        nom::Err::Error((input, error)) => nom::Err::Error((input, error.into())),
        nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
        nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
    }
}

pub fn basic_match_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    match error {
        nom::Err::Error((input, _))
            if input.to_str().is_empty() => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::Eof))),
        nom::Err::Error((input, error)) if input.to_str().starts_with(char::is_alphabetic) => // TODO: check if it starts_with any of the keywords
            nom::Err::Failure((input, error.into())),
        nom::Err::Error((input, _)) => nom::Err::Error((input, ParseError::new_from_kind(input, ParseErrorKind::NoMatch))),
        nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
        nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
    }
}

pub fn tuple_error<'a>(comma: Input<'a>) -> impl Fn(nom::Err<(Input<'a>, ParseError)>) -> nom::Err<(Input<'a>, ParseError)> {
    move |error|
    match error {
        nom::Err::Error((input, _))
            => nom::Err::Failure((input, ParseError::new_from_kind(input, ParseErrorKind::IncompleteTuple(comma.line(), comma.column())))),
        nom::Err::Failure((input, error)) => nom::Err::Failure((input, error.into())),
        nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
    }
}

pub fn terminating_paren_error(line: usize, column: usize) -> impl Fn(nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    move |error|
    match error {
        nom::Err::Error((input, _)) =>
            nom::Err::Failure((
                input,
                ParseError::new_from_kind(input, ParseErrorKind::TerminatingParen(line, column))
            )),
        error => error,
    }
}

pub fn terminating_bracket_error(line: usize, column: usize) -> impl Fn(nom::Err<(Input, ParseError)>) -> nom::Err<(Input, ParseError)> {
    move |error|
    match error {
        nom::Err::Error((input, _)) => // The only possible error is a 'right_bracket' error
            nom::Err::Failure((
                input,
                ParseError::new_from_kind(input, ParseErrorKind::TerminatingBracket(line, column))
            )),
        error => error,
    }
}

// Decl Errors
error_type! {
    token [decl_expr_error]
    "=" => ParseErrorKind::DeclAssign
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ParseErrorKind {
    // Token Errors

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
	ExpectedKeyword {
		keyword: &'static str,
		line: usize,
		column: usize,
	},

    // Match Errors

    IncompleteTuple(usize, usize),
    
    // OLD ERRORS

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
    UnknownFailure,
	UnhandledNomError,
    TerminatingNewline,
    Eof,
    NoMatch,
    Incomplete,
}

impl From<nom::error::ErrorKind> for ParseErrorKind {
    fn from(error: nom::error::ErrorKind) -> Self {
        match error {
            nom::error::ErrorKind::Eof => ParseErrorKind::Eof,
            _ => ParseErrorKind::UnhandledNomError,
        }
    }
}

impl From<ParseErrorKind> for u16 {
    fn from(error: ParseErrorKind) -> Self {
        u16::from(&error)
    }
}

// Error code (between 1 and 100 for parse errors)
// TODO: find the trait that can be derived to do this on it's own
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
            ParseErrorKind::UnhandledNomError => 21,
            ParseErrorKind::EndOfExpr => 22,
            ParseErrorKind::TerminatingBracket(_, _) => 23,
            ParseErrorKind::Incomplete => 24,
            ParseErrorKind::DoIn => 25,
            ParseErrorKind::DoThen => 26,
            ParseErrorKind::CharUnclosed => todo!("write this"),
            ParseErrorKind::InvalidChar => todo!("write this"),
            ParseErrorKind::InvalidEscapedChar => todo!("write this"),
            ParseErrorKind::StringUnclosed => todo!("write this"),
            ParseErrorKind::InvalidEscapedString => todo!("write this"),
            ParseErrorKind::NumberTooBig => todo!("write this"),
            ParseErrorKind::KeywordIdentifier => todo!("write this"),
            ParseErrorKind::UnexpectedNewline => todo!("write this"),
            ParseErrorKind::UnclosedComment => todo!("write this"),
            ParseErrorKind::ExpectedNewline => todo!("write this"),
            ParseErrorKind::ExpectedKeyword {
                ..
            } => todo!("write this"),
            ParseErrorKind::UnknownFailure => todo!("write this"),
            ParseErrorKind::NoMatch => todo!("write this"),
            ParseErrorKind::IncompleteTuple(_, _) => todo!("write this"),
        }
    }
}