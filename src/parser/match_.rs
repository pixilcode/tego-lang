use crate::parser::tokens::*;
use crate::ast::Match;

use nom::{
    IResult
};

pub fn match_(input: &'_ str) -> IResult<&'_ str, Match> {
    identifier(input).map(
        |(input, ident)|
        (input, Match::ident(ident))
    )
}