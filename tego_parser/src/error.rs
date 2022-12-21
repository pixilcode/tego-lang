use crate::{Input, ParseInternalResult};
use std::fmt;
use std::io;
use std::ops::Deref;

/// Error handlers
pub mod parse_handlers;

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
impl<'a, O> From<ParseInternalResult<'a, O>> for ParseErrorOutput<O> {
	fn from(error: ParseInternalResult<'a, O>) -> Self {
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