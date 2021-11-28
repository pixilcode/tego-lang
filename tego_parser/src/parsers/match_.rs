use crate::error::parser::*;
use crate::parsers::tokens::*;
use crate::Input;
use crate::MatchOutput;
use crate::ParseResult;
use nom::branch::alt;

use nom::{
    combinator::opt,
    sequence::{pair, terminated},
};

type MatchResult<'a, M> = ParseResult<'a, M>;

pub fn match_<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    tuple(input)
}

fn tuple<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    pair(grouping, opt(comma))(input).and_then(|(input, (a, comma))| match comma {
        Some(_) => tuple(input).map(|(input, b)| (input, M::tuple(a, b))),
        None => Ok((input, a)),
    })
}

pub fn grouping<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    left_paren(input)
        .and_then(|(input, open_paren)| {
            right_paren(input)
                .map(|(input, _)| (input, M::unit()))
                .or_else(try_parser(terminated(match_, right_paren), input))
                .map_err(grouping_match_error((
                    open_paren.line(),
                    open_paren.column(),
                )))
        })
        .or_else(try_parser(
            |input| {
                opt_nl(left_bracket)(input).and_then(|(input, open_bracket)| {
                    terminated(opt_nl(match_), right_bracket)(input)
                        .map(|(input, inner)| (input, M::boxed(inner)))
                        .map_err(terminating_bracket_error((
                            open_bracket.line(),
                            open_bracket.column(),
                        )))
                })
            },
            input,
        ))
        .or_else(try_parser(atom, input))
}

fn atom<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    alt((true_val, false_val, underscore, number, identifier))(input)
        .map(|(new_input, token)| match token.into() {
            "true" => (new_input, M::bool(true)),
            "false" => (new_input, M::bool(false)),
            "_" => (new_input, M::ignore()),
            lexeme => {
                if let Ok(i) = lexeme.parse::<i32>() {
                    (new_input, M::int(i))
                } else {
                    (new_input, M::ident(lexeme))
                }
            }
        })
        .or_else(|_| string(input).map(|(input, s)| (input, M::string(s.into()))))
        .or_else(|_| char(input).map(|(input, c)| (input, M::char(c))))
        .map_err(basic_match_error)
}

pub fn variable<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    identifier(input)
        .map(|(input, lexeme)| (input, M::ident(lexeme.into())))
        .map_err(ident_match_error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Match;
    use crate::test::*;

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
            );
        (match_): "a, ()" =>
            Match::tuple(
                Match::ident("a"),
                Match::unit()
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
            Match::Unit
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
