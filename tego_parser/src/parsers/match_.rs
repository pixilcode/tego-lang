use crate::error::{
    parse_handlers::{
        tuple_error,
        terminating_paren_error,
        terminating_bracket_error,
        num_expr_error,
        string_expr_error,
        char_expr_error,
        basic_match_error,
        ident_match_error,
    }
};
use crate::parsers::tokens::*;
use crate::Input;
use crate::MatchOutput;
use crate::InternalParseResult;
use nom::branch::alt;

use nom::{
    combinator::{opt, map},
    sequence::{pair, terminated},
};

type MatchResult<'a, M> = InternalParseResult<'a, M>;

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
    // TODO: add 'opt_nl' after 'comma' to match grammar
    pair(grouping, opt(comma))(input).and_then(|(input, (a, comma))| match comma {
        Some(comma) => tuple(input)
            .map(|(input, b)| (input, M::tuple(a, b)))
            .map_err(tuple_error(comma)),
        None => Ok((input, a)),
    })
}

pub fn grouping<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    alt((
        |input|
        opt_nl(left_paren)(input)
        .and_then(|(input, open_paren)|
            map(
                // TODO: add 'opt_nl' after 'match_' to match grammar
                terminated(opt(match_), right_paren),
                |match_result| match_result.unwrap_or_else(M::unit)
            )(input)
            .map_err(terminating_paren_error(
                open_paren.line(),
                open_paren.column(),
            ))
        ),

        |input|
        opt_nl(left_bracket)(input)
        .and_then(|(input, open_bracket)| {
            map(
                terminated(opt_nl(match_), right_bracket),
                |inner| M::boxed(inner)
            )(input)
            .map_err(terminating_bracket_error(
                open_bracket.line(),
                open_bracket.column(),
            ))
        }),

        atom
    ))(input)
}

fn atom<M>(input: Input<'_>) -> MatchResult<'_, M>
where
    M: MatchOutput,
{
    alt((
        map(
            alt((true_val, false_val)),
            |token| match token.into() {
                "true" => M::bool(true),
                "false" => M::bool(false),
                _ => unreachable!(),
            }
        ),

        map(
            underscore,
            |_| M::ignore()
        ),

        map(
            identifier,
            |lexeme| M::ident(lexeme.into())
        ),

        map(
            |input| number(input).map_err(num_expr_error),
            M::int
        ),

        map(
            |input| string(input).map_err(string_expr_error),
            M::string
        ),

        map(
            |input| char(input).map_err(char_expr_error),
            M::char
        )
    ))(input)
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
    use crate::span::span_at;
    use crate::error::ParseErrorKind;

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
            Match::string("a".into())
    }
    parser_test! {
        boxed_test
        (match_): "[ 1 ]" =>
            Match::boxed(Match::int(1))
    }

    // Error tests
    basic_test! {
        variable_error_test
        variable::<()>("".into()) => 
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        variable::<()>("1".into()) =>
            parse_error("1".into(), 1, 1, ParseErrorKind::NoMatch);
        variable::<()>("if".into()) =>
            parse_failure("if".into(), 1, 1, ParseErrorKind::KeywordIdentifier)
    }

    basic_test! {
        atom_error_test
        atom::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        atom::<()>("$".into()) =>
            parse_error("$".into(), 1, 1, ParseErrorKind::NoMatch)
    }

    basic_test! {
        grouping_error_test
        grouping::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        grouping::<()>("(".into()) =>
            parse_failure(span_at("", 2, 1, 1), 2, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("(1".into()) =>
            parse_failure(span_at("", 3, 1, 2), 3, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("(1, 2".into()) =>
            parse_failure(span_at("", 6, 1, 5), 6, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("(1, 2 => ".into()) =>
            parse_failure(span_at("=> ", 7, 1, 6), 7, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("[".into()) =>
            parse_failure(span_at("", 2, 1, 1), 2, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1".into()) =>
            parse_failure(span_at("", 3, 1, 2), 3, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1, 2".into()) =>
            parse_failure(span_at("", 6, 1, 5), 6, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1, 2 => ".into()) =>
            parse_failure(span_at("=> ", 7, 1, 6), 7, 1, ParseErrorKind::TerminatingBracket(1, 1))
    }

    basic_test! {
        tuple_error_test
        tuple::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        tuple::<()>("1, ".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::IncompleteTuple(1, 2));
        tuple::<()>("1, 2, ".into()) =>
            parse_failure(span_at("", 7, 1, 6), 7, 1, ParseErrorKind::IncompleteTuple(1, 5));
        tuple::<()>("1, (2, )".into()) =>
            parse_failure(span_at(")", 8, 1, 7), 8, 1, ParseErrorKind::IncompleteTuple(1, 6))
    }
}
