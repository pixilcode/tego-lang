extern crate nom;

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

pub mod parser;
pub mod execute;
mod ast;
mod environment;
mod type_;