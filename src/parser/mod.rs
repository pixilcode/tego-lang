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

mod match_;
mod tokens;

pub mod decl;
pub mod expr;
