use crate::ast::Match;
use crate::parser::error::*;
use crate::parser::tokens::*;
use crate::parser::Input;
use crate::parser::ParseResult;
use nom::branch::alt;

use nom::{
    combinator::opt,
    sequence::{pair, terminated},
};

type MatchResult<'a> = ParseResult<'a, Match>;

pub fn match_(input: Input<'_>) -> MatchResult<'_> {
    tuple(input)
}

fn tuple(input: Input<'_>) -> MatchResult<'_> {
    pair(grouping, opt(comma))(input).and_then(|(input, (a, comma))| match comma {
        Some(_) => tuple(input).map(|(input, b)| (input, Match::tuple(a, b))),
        None => Ok((input, a)),
    })
}

pub fn grouping(input: Input<'_>) -> MatchResult<'_> {
    left_paren(input)
        .and_then(|(input, open_paren)| {
            right_paren(input)
                .map(|(input, _)| (input, Match::unit()))
                .or_else(try_parser(terminated(match_, right_paren), input))
                .map_err(grouping_match_error((
                    open_paren.line(),
                    open_paren.column(),
                )))
        })
        .or_else(
            try_parser(|input| opt_nl(left_bracket)(input)
            .and_then(|(input, open_bracket)| {
                terminated(opt_nl(match_), right_bracket)(input)
                .map(|(input, inner)| (input, Match::boxed(inner)))
                .map_err(terminating_bracket_error((
                    open_bracket.line(),
                    open_bracket.column()
                )))
            }),
            input)
        )
        .or_else(try_parser(atom, input))
}

fn atom(input: Input<'_>) -> MatchResult<'_> {
    alt((true_val, false_val, underscore, number, identifier))(input)
        .and_then(|(new_input, token)| match token.into() {
            "true" => Ok((new_input, Match::bool(true))),
            "false" => Ok((new_input, Match::bool(false))),
            "_" => Ok((new_input, Match::ignore())),
            lexeme => {
                if let Ok(i) = lexeme.parse::<i32>() {
                    Ok((new_input, Match::int(i)))
                } else {
                    Ok((new_input, Match::ident(lexeme)))
                }
            }
        })
        .or_else(|_| string(input).map(|(input, s)| (input, Match::string(s.into()))))
        .or_else(|_| char(input).map(|(input, c)| (input, Match::char(c))))
        .map_err(basic_match_error)
}

pub fn variable(input: Input<'_>) -> MatchResult<'_> {
    identifier(input)
        .map(|(input, lexeme)| (input, Match::ident(lexeme.into())))
        .map_err(ident_match_error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::test::*;

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
    parser_test! {
        value_test
        (match_): "1" =>
            Match::int(1);
        (match_): "true" =>
            Match::bool(true)
    }
    parser_test! {
        unit_test
        (match_): "()" =>
            Match::Tuple(vec![])
    }
    parser_test! {
        variable_test
        (variable): "abc" =>
            Match::ident("abc")
    }
    parser_test! {
        char_test
        (match_): "'a'" =>
            Match::char('a')
    }
    parser_test! {
        string_test
        (match_): "\"a\"" =>
            Match::string("a")
    }
    parser_test! {
        boxed_test
        (match_): "[ 1 ]" =>
            Match::boxed(Match::int(1))
    }
}
