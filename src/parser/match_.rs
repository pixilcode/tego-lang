use crate::parser::tokens::*;
use crate::ast::Match;

use nom::{
    IResult,
    sequence::{
        pair,
        terminated
    },
    combinator::opt
};

pub fn match_(input: &'_ str) -> IResult<&'_ str, Match> {
    tuple(input)
}

pub fn tuple(input: &'_ str) -> IResult<&'_ str, Match> {
    pair(grouping, opt(comma))(input).and_then(
        |(input, (a, comma))|
        match comma {
            Some(_) => tuple(input).map(
                |(input, b)|
                (input, Match::tuple(a, b))
            ),
            None => Ok((input, a))
        }
    )
}

pub fn grouping(input: &'_ str) -> IResult<&'_ str, Match> {
    opt(left_paren)(input).and_then(
        |(input, left_paren)|
        match left_paren {
            Some(_) => terminated(match_, right_paren)(input),
            None => ident(input)
        }
    )
}

pub fn ident(input: &'_ str) -> IResult<&'_ str, Match> {
    identifier(input).map(
        |(input, ident)|
        (input, Match::ident(ident))
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    
    parser_test! {
        ident_test
        (match_): "abc" => Match::ident("abc")
    }
    
    parser_test! {
        tuple_test
        (match_): "a, b" =>
            Match::tuple(
                Match::ident("a"),
                Match::ident("b")
            );
        (match_): "a, b, c" =>
            Match::tuple(
                Match::tuple(
                    Match::ident("a"),
                    Match::ident("b")
                ), Match::ident("c")
            )
    }
    
    // Not actually super effective, the order
    // of tuples doesn't really matter
    parser_test! {
        grouping_test
        (match_): "a, (b , c)" =>
            Match::tuple(
                Match::ident("a"),
                Match::tuple(
                    Match::ident("b"),
                    Match::ident("c")
                )
            )
    }
}