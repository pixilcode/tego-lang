#![warn(missing_docs)]

//! The parser for the Tego programming language
//! 
//! This crate makes certain parts of the parser available for
//! public use:
//!   * Parsers:
//!     * `decl`—parse a Tego declaration
//!     * `expr`—parse a Tego expression
//!     * `match_`—parse a Tego match pattern
//!     * `prog`—parse a Tego program
//!     * `complete`—ensure that the complete text is consumed
//!   * AST: a general-use output for the parser in the form
//!     of an abstract syntax tree
//!   * Output traits: traits that can be implemented in order
//!     to customize the output of the parser
//!   * Other utilities:
//!     * `ParseError`: the error returned by the parser when it
//!       fails
//!     * `Span`: an input for the parser that tracks the line,
//!       column, and character offset of the string in the program

#[allow(unused_macros)]
macro_rules! basic_test {
    ( $name:ident $( $actual:expr => $expected:expr );+) => {
        #[allow(clippy::eq_op)]
        #[allow(clippy::bool_assert_comparison)]
        #[test]
        fn $name() {
            $( assert_eq!($expected, $actual); )+
        }
    };
}

// Assumes that the full amount is consumed
#[allow(unused_macros)]
macro_rules! parser_test {
    ($name:ident $( (  $func:ident ) : $input:expr => $output:expr );+) => {
        basic_test! {
            $name
            $( $func(Span::new($input)) =>  Ok((empty_span(Span::new($input)), $output)) );+
        }
    };
}

pub mod ast;
mod error;
mod parsers;
mod span;
mod traits;

// Parsers
#[doc(inline)]
pub use crate::parsers::decl::decl;
#[doc(inline)]
pub use crate::parsers::expr::expr;
#[doc(inline)]
pub use crate::parsers::match_::match_;
#[doc(inline)]
pub use crate::parsers::prog::prog;
pub use nom::combinator::all_consuming as complete;

// Traits for parser output
#[doc(inline)]
pub use crate::traits::DeclOutput;
#[doc(inline)]
pub use crate::traits::ExprOutput;
#[doc(inline)]
pub use crate::traits::MatchOutput;
#[doc(inline)]
pub use crate::traits::ProgOutput;

// Utilities
#[doc(inline)]
pub use crate::error::ParseError;
#[doc(inline)]
pub use crate::span::Span;

type Input<'a> = Span<'a>;
type ParseResult<'a, O> = nom::IResult<Input<'a>, O, (Input<'a>, ParseError)>;

mod test {
    use crate::span;
    pub use crate::Span;
    pub use crate::{DeclOutput, ExprOutput, MatchOutput, ProgOutput};

    #[allow(dead_code)]
    pub fn empty_span(input: Span<'_>) -> Span<'_> {
        let mut line = input.line();
        let mut column = input.column();
        let mut offset = input.offset();

        for c in input.to_str().chars() {
            match c {
                '\n' => {
                    line += 1;
                    column = 1;
                    offset += 1;
                }
                '\t' => {
                    column += 4;
                    offset += 1;
                }
                _ => {
                    column += 1;
                    offset += 1;
                }
            }
        }

        span::span_at("", column, line, offset)
    }
}
