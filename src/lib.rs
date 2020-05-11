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

pub use crate::ast::decl::Decl;

mod ast;
mod environment;
pub mod execute;
pub mod parser;
mod type_;
