#[allow(unused_macros)]
macro_rules! basic_test {
    ( $name:ident $( $actual:expr => $expected:expr );+) => {
        #[allow(clippy::eq_op)]
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
pub use crate::parsers::decl::decl;
pub use crate::parsers::expr::expr;
pub use crate::parsers::match_::match_;
pub use crate::parsers::prog::prog;
pub use nom::combinator::all_consuming as complete;

// Utilities
pub use crate::error::ParseError;
pub use crate::span::Span;

// Traits for parser output
pub use crate::traits::DeclOutput;
pub use crate::traits::ExprOutput;
pub use crate::traits::MatchOutput;
pub use crate::traits::ProgOutput;

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
