use crate::parsers::tokens;
use crate::{Input, ParseResult};
use std::fmt;
use std::io;
use std::ops::Deref;

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError {
    column: usize,
    line: usize,
    kind: ParseErrorKind,
}

impl ParseError {
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

    fn nom_error(input: Input<'_>, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Error((input, Self::new_from_kind(input, kind)))
    }

    fn nom_failure(input: Input<'_>, kind: ParseErrorKind) -> nom::Err<(Input<'_>, Self)> {
        nom::Err::Failure((input, Self::new_from_kind(input, kind)))
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
            ParseErrorKind::CharUnclosed => "unclosed character",
            ParseErrorKind::InvalidChar => "invalid character",
            ParseErrorKind::InvalidEscapedChar => "invalid escaped character",
            ParseErrorKind::StringUnclosed => "unclosed string",
            ParseErrorKind::InvalidEscapedString => "invalid escaped string",
            ParseErrorKind::NumberTooBig => "number out of range - must be between -2,147,483,647 and 2,147,483,647",
            ParseErrorKind::KeywordIdentifier => "found a keyword where an identifier was expected",
            ParseErrorKind::UnexpectedNewline => "encountered an unexpected new line",
            ParseErrorKind::UnclosedComment => "unclosed comment",
            ParseErrorKind::ExpectedNewline => "expected newline",
            ParseErrorKind::ExpectedKeyword { .. } => "expected keyword",
            
            // Expr Errors
            ParseErrorKind::TerminatingParen(_, _) => "missing closing parenthesis",
            ParseErrorKind::TerminatingBracket(_, _) => "missing closing bracket",
            ParseErrorKind::FnArrow => {
                "missing '->' between function parameters and function body"
            }
            ParseErrorKind::MatchBar => "missing '|' before match arm",
            ParseErrorKind::MatchArrow => "missing '->' between match pattern and match body",
            ParseErrorKind::MatchTo => "missing 'to' between match head and body",
            ParseErrorKind::Then => "missing 'then' or '?' after if condition",
            ParseErrorKind::Else => "missing 'else' in if expression",
            ParseErrorKind::LetAssign => "missing '=' in let assignment",
            ParseErrorKind::LetIn => "missing 'in' in let expression",
            ParseErrorKind::DelayAssign => "missing '=' in delay assignment",
            ParseErrorKind::DelayIn => "missing 'in' in delay expression",
            ParseErrorKind::DoIn => "missing 'in' in do expression",
            ParseErrorKind::DoThen => "missing 'then' in do expression",
            ParseErrorKind::ExpectedExpr => "expected expression",
            ParseErrorKind::ExpectedMatch => "expected match",
            ParseErrorKind::ExpectedVariable => "expected variable identifier",
            ParseErrorKind::IncompleteTuple(_, _) => "incomplete match (missing tuple right hand side)",
            ParseErrorKind::MissingRhs => "missing operator right hand side",

            // Decl Errors
            ParseErrorKind::DeclAssign => "missing '=' in expression declaration",

            // Other Errors
            ParseErrorKind::Eof => "reached end of file before parsing was completed",
            ParseErrorKind::Incomplete => "incomplete information found",
            ParseErrorKind::UnhandledNomError => "unknown error from parsing",
            ParseErrorKind::UnknownFailure => "unknown parsing failure",
            ParseErrorKind::NoMatch => "no parser match",
        };
        write!(
            f,
            "error[line {}, column {}]: {}",
            self.line,
            self.column,
            error,
        )
    }
}

impl std::error::Error for ParseError {}

struct ParseErrorOutput<O>(Result<O, ParseError>); 

/// Convert a nom error to a ParseErrorOutput for use outside of the crate
impl<'a, O> From<ParseResult<'a, O>> for ParseErrorOutput<O> {
	fn from(error: ParseResult<'a, O>) -> Self {
		match error {
			Ok((_, result)) => ParseErrorOutput(Ok(result)),
			Err(nom::Err::Error((_, error))) |
				Err(nom::Err::Failure((_, error))) => ParseErrorOutput(Err(error)),
			
			// The parsers shouldn't ever encounter an `Incomplete` since they
			// don't use streaming parsers
			Err(nom::Err::Incomplete(_)) => unreachable!("parser doesn't use 'streaming' parsers"),
		}
	}
}

/// Allow for `ParseErrorOutput` to be dereffed as a Result (allows for implementations
/// of traits on the specialized `Result` type)
impl<O> Deref for ParseErrorOutput<O> {
	type Target = Result<O, ParseError>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// Error handlers
pub mod handlers {
    use super::*;

    /// If there is an error, ignore anything that was parsed by the
    /// parser and just return the original input
    pub fn err_retain_all<'a, F, O, E>(parser: F) -> impl Fn(Input<'a>) -> nom::IResult<Input<'a>, O, (Input<'a>, E)>
    where
        F: Fn(Input<'a>) -> nom::IResult<Input<'a>, O, (Input<'a>, E)>,
    {
        move |input| parser(input).map_err(|error| match error {
            nom::Err::Error((_, error)) => nom::Err::Error((input, error)),
            nom::Err::Failure((_, error)) => nom::Err::Failure((input, error)),
            error => error
        })
    }


    /// Try a different scanner
    // TODO: Get rid of this, just use `alt` instead
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

    /// Indicate that this is the right hand side (rhs) of an operator
    /// 
    /// If the rhs doesn't match anything, alert that the rhs is missing
    pub fn expect_rhs<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::NoMatch | ParseErrorKind::Eof
            ) =>
                ParseError::nom_failure(input, ParseErrorKind::MissingRhs),
            error => error
        })
    }

    /// Indicate that this match pattern is expected
    pub fn expect_match<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::NoMatch | ParseErrorKind::Eof
            ) =>
                ParseError::nom_failure(input, ParseErrorKind::ExpectedMatch),
            error => error
        })
    }

    /// Indicate that this expression is expected
    pub fn expect_expr<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::NoMatch | ParseErrorKind::Eof
            ) =>
                ParseError::nom_failure(input, ParseErrorKind::ExpectedExpr),
            error => error
        })
    }

    /// Indicate that a variable identifier is expected
    pub fn expect_variable<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::NoMatch | ParseErrorKind::Eof
            ) =>
                ParseError::nom_failure(input, ParseErrorKind::ExpectedVariable),
            error => error
        })
    }

    /// Indicate that the given token is expected
    pub fn expect_keyword<'a, F, O>(parser: F, kind: ParseErrorKind) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::ExpectedKeyword { .. }
            ) =>
                ParseError::nom_failure(input, kind),
            error => error
        })
    }

    /// Indicate that at least one match arm is expected
    pub fn expect_match_arms<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
    where
        F: Fn(Input<'a>) -> ParseResult<'a, O>,
    {
        move |input|
        parser(input).map_err(|error| match error {
            nom::Err::Error((input, error)) if matches!(error.kind,
                ParseErrorKind::ExpectedKeyword {
                    keyword: "|",
                    ..
                }
            ) =>
                ParseError::nom_failure(input, ParseErrorKind::MatchBar),
            error => error
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
                        ParseError::nom_error(input, ParseErrorKind::InvalidEscapedChar),
                    Some(c) if tokens::INVALID_CHARS.contains(&c) =>
                        ParseError::nom_error(input, ParseErrorKind::InvalidChar),
                    _ => ParseError::nom_error(input, ParseErrorKind::CharUnclosed)
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
                ParseError::nom_error(input, ParseErrorKind::StringUnclosed),
            nom::Err::Error((input, _)) => // a 
                ParseError::nom_error(input, ParseErrorKind::InvalidEscapedString),
            nom::Err::Failure((input, _)) =>
                ParseError::nom_failure(input, ParseErrorKind::UnknownFailure),
            nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
        }
    }

    pub fn number_error(error: nom::Err<(Input, nom::error::ErrorKind)>) -> nom::Err<(Input, ParseError)> {
        match error {
            nom::Err::Error((input, nom::error::ErrorKind::MapRes)) =>
                ParseError::nom_error(input, ParseErrorKind::NumberTooBig),
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
                ParseError::nom_error(input, ParseErrorKind::KeywordIdentifier),
            nom::Err::Error((input, _)) if input.to_str().is_empty() =>
                ParseError::nom_error(input, ParseErrorKind::Eof),
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
                ParseError::nom_error(full_input, ParseErrorKind::UnclosedComment),
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
                ParseError::nom_error(input, ParseErrorKind::UnexpectedNewline),
            nom::Err::Error((_, nom::error::ErrorKind::TakeUntil)) =>
                ParseError::nom_error(full_input, ParseErrorKind::UnclosedComment),
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
                ParseError::nom_error(input, ParseErrorKind::ExpectedKeyword {
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

    pub fn missing_rhs_error(error: nom::Err<(Input, ParseError)>) -> nom::Err<(Input<'_>, ParseError)> {
        match error {
            nom::Err::Error((input, _)) =>
                ParseError::nom_failure(input, ParseErrorKind::MissingRhs),
            error => error
        }
    }

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
    
    // EXPECTED AST ERRORS
    ExpectedExpr,
    ExpectedMatch,
    ExpectedVariable,
    
    // Match Errors
    IncompleteTuple(usize, usize),
    
    // Expr Errors
    MissingRhs,
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
    DoIn,
    DoThen,

    // Decl Errors
    DeclAssign,

    // Other Errors
    UnknownFailure,
	UnhandledNomError,
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