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

mod decl;
mod expr;
mod match_;
mod prog;
mod span;
mod tokens;

pub use decl::decl;
pub use expr::expr;
pub use match_::match_;
pub use prog::prog;
pub use span::Span;

type Input<'a> = Span<'a>;

mod test {
    use crate::parser::span;
    pub use crate::parser::Span;

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
