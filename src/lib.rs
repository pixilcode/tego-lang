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
pub mod checker;
pub mod ast;
pub mod value;
pub mod environment;
mod type_;