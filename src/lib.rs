extern crate nom;
extern crate owned_chars;

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

pub mod ast;
mod environment;
mod execute;
pub mod parser;
mod type_;

pub use execute::interpreter;
