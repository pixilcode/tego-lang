use crate::parser::error::*;
use crate::parser::{Input, ParseResult};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till1},
    character::complete::{anychar, digit1, line_ending, multispace0, space0},
    combinator::{all_consuming, map_res, opt, verify},
    sequence::{preceded, terminated, tuple},
};

const KEYWORDS: &[&str; 15] = &[
    "and", "or", "xor", "not", "true", "false", "if", "then", "else", "let", "in", "fn", "match",
    "to", "delay",
];

pub fn newlines<'a>(
    is_opt: bool,
) -> impl Fn(Input<'a>) -> ParseResult<'a, (Input<'a>, Option<Input<'a>>, Input<'a>)> {
    map_res(
        tuple((space0, opt(line_ending), multispace0)),
        move |(ws1, nl, ws2)| match (is_opt, nl) {
            // The error won't be used in any way, just indicates that it's an error
            (false, None) => Err(()),
            (_, nl) => Ok((ws1, nl, ws2)),
        },
    )
}

pub fn opt_nl<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    terminated(parser, newlines(true))
}

pub fn req_nl<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    terminated(
        parser,
        alt((newlines(false), all_consuming(newlines(true)))),
    )
}

fn token<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    preceded(space0, parser)
}

macro_rules! reserved {
    ($lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
            token(tag($lexeme_str))(input).map_err(token_error)
        }
    };
}

pub fn char(input: Input<'_>) -> ParseResult<'_, char> {
    terminated(preceded(single_quote, anychar), single_quote)(input).map_err(char_error)
}

pub fn string(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    terminated(preceded(double_quote, is_not("\"")), double_quote)(input)
}

pub fn number(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    token(digit1)(input)
}

pub fn identifier(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    verify(
        token(take_till1(|c: char| !c.is_ascii_alphabetic() && c != '\'')),
        |id| !is_keyword(id.to_str()) && !id.to_str().starts_with('\''),
    )(input)
}

reserved!(comma, ",");
reserved!(plus, "+");
reserved!(minus, "-");
reserved!(star, "*");
reserved!(slash, "/");
reserved!(modulo, "%");
reserved!(and, "and");
reserved!(or, "or");
reserved!(xor, "xor");
reserved!(not, "not");
reserved!(true_val, "true");
reserved!(false_val, "false");
reserved!(equal, "==");
reserved!(not_equal, "/=");
reserved!(less_than, "<");
reserved!(greater_than, ">");
reserved!(less_than_equal, "<=");
reserved!(greater_than_equal, ">=");
reserved!(left_paren, "(");
reserved!(right_paren, ")");
reserved!(if_, "if");
reserved!(then, "then");
reserved!(q_mark, "?");
reserved!(else_, "else");
reserved!(colon, ":");
reserved!(let_, "let");
reserved!(in_, "in");
reserved!(assign, "=");
reserved!(fn_, "fn");
reserved!(arrow, "->");
reserved!(match_kw, "match");
reserved!(to, "to");
reserved!(bar, "|");
reserved!(underscore, "_");
reserved!(delay, "delay");
reserved!(single_quote, "'");
reserved!(double_quote, "\"");

fn is_keyword(lexeme: &str) -> bool {
    KEYWORDS.iter().any(|keyword| keyword == &lexeme)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::span::span_at;
    use crate::parser::test::*;

    #[test]
    fn token_parser_test() {
        let parser = token(tag("abc"));
        assert_eq!(
            parser(" \tabc".into()),
            Ok((span_at("", 9, 1, 5), span_at("abc", 6, 1, 2)))
        );
    }
    parser_test!(comma_test (comma): "," => ",".into());
    parser_test!(plus_test (plus): "+" => "+".into());
    parser_test!(minus_test (minus): "-" => "-".into());
    parser_test!(star_test (star): "*" => "*".into());
    parser_test!(slash_test (slash): "/" => "/".into());
    parser_test!(modulo_test (modulo): "%" => "%".into());
    parser_test!(and_test (and): "and" => "and".into());
    parser_test!(or_test (or): "or" => "or".into());
    parser_test!(xor_test (xor): "xor" => "xor".into());
    parser_test!(not_test (not): "not" => "not".into());
    parser_test!(true_test (true_val): "true" => "true".into());
    parser_test!(false_test (false_val): "false" => "false".into());
    parser_test!(left_paren_test (left_paren): "(" => "(".into());
    parser_test!(right_paren_test (right_paren): ")" => ")".into());
    parser_test!(if_test (if_): "if" => "if".into());
    parser_test!(then_test (then): "then" => "then".into());
    parser_test!(q_mark_test (q_mark): "?" => "?".into());
    parser_test!(else_test (else_): "else" => "else".into());
    parser_test!(colon_test (colon): ":" => ":".into());
    parser_test!(number_test (number): "12" => "12".into());
    parser_test!(identifier_test (identifier): "aBc'" => "aBc'".into());
    parser_test!(let_test (let_): "let" => "let".into());
    parser_test!(in_test (in_): "in" => "in".into());
    parser_test!(assign_test (assign): "=" => "=".into());
    parser_test!(fn_test (fn_): "fn" => "fn".into());
    parser_test!(arrow_test (arrow): "->" => "->".into());
    parser_test!(match_kw_test (match_kw): "match" => "match".into());
    parser_test!(to_test (to): "to" => "to".into());
    parser_test!(bar_test (bar): "|" => "|".into());
    parser_test!(underscore_test (underscore): "_" => "_".into());
    parser_test!(delay_test (delay): "delay" => "delay".into());
    parser_test!(single_quote_test (single_quote): "'" => "'".into());
    parser_test!(double_quote_test (double_quote): "\"" => "\"".into());
    parser_test!(string_test (string): "\"abc\"" => span_at("abc", 2, 1, 1));
    basic_test!(char_test char("'a'".into()) => Ok((span_at("", 4, 1, 3), 'a')));
    // Use find and replace
    // Find: reserved!\(([a-z_]+), ("[^"]+")\);
    // Replace: parser_test!($1_test ($1): $2 => $2);
}
