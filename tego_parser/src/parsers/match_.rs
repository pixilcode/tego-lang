use crate::error::parser::*;
use crate::parsers::tokens::*;
use crate::Input;
use crate::MatchOutput;
use crate::ParseResult;
use nom::branch::alt;

use nom::{
    combinator::{opt, map},
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
    begin_phrase(opt_nl(left_paren))(input)
        .and_then(|(input, open_paren)| {
            map(
                terminated(opt(match_), into_parser(right_paren)),
                |match_result| match match_result {
                    Some(match_result) => match_result,
                    None => M::unit(),
                }
            )(input)
                .map_err(grouping_error(
                    open_paren.line(),
                    open_paren.column(),
                ))
        })
        .or_else(try_parser(
            |input| {
                begin_phrase(opt_nl(left_bracket))(input).and_then(|(input, open_bracket)| {
                    map(
                        terminated(opt_nl(match_), into_parser(right_bracket)),
                        |inner| M::boxed(inner)
                    )(input)
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
    map(
        alt((true_val, false_val)),
        |token| match token.into() {
            "true" => M::bool(true),
            "false" => M::bool(false),
            _ => unreachable!(),
        }
    )(input)
    .or_else(
        try_parser(
            map(underscore, |_| M::ignore()),
            input
        )
    )
    .map(|(input, _)| (input, M::ignore()))
    .or_else(
        try_parser(
            map(number, |int| M::int(int)),
            input
        )
    )
    .or_else(
        try_parser(
            map(identifier, |lexeme| M::ident(lexeme.into())),
            input
        )
    )
    .or_else(
        try_parser(
            map(string, |s| M::string(s)),
            input
        )
    )
    .or_else(
        try_parser(
            map(char, |c| M::char(c)),
            input
        )
    )
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
    use crate::error::scanner::{ScanError, ScanErrorKind};

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
            scan_error("if".into(), 1, 1, ScanErrorKind::KeywordIdentifier)
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
            parse_error("".into(), 2, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("(1, 2".into()) =>
            parse_error("".into(), 6, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("(1, 2 => ".into()) =>
            parse_error("=> ".into(), 7, 1, ParseErrorKind::TerminatingParen(1, 1));
        grouping::<()>("[".into()) =>
            parse_error("".into(), 2, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1, 2".into()) =>
            parse_error("".into(), 6, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1, 2 => ".into()) =>
            parse_error("=> ".into(), 7, 1, ParseErrorKind::TerminatingBracket(1, 1))
    }

    fn parse_error<O>(remaining: Input<'_>, column: usize, line: usize, kind: ParseErrorKind)
        -> ParseResult<'_, O> {
            Err(nom::Err::Error((remaining, ParseError::new(
                column,
                line,
                kind
            ))))
    }

    fn scan_error<O>(remaining: Input<'_>, column: usize, line: usize, kind: ScanErrorKind)
        -> ParseResult<'_, O> {
            Err(nom::Err::Error((remaining, ScanError::new_from(
                column,
                line,
                kind,
            ).into())))
    }
}
