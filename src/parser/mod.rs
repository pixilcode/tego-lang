// Assumes that the full amount is consumed
#[allow(unused_macros)]
macro_rules! parser_test {
    ($name:ident $( (  $func:ident ) : $input:expr => $output:expr );+) => {
        basic_test! {
            $name
            $( $func($input) =>  Ok(("", $output)) );+
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

type Input<'a> = &'a str;
