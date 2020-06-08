use crate::parser::error::*;
use crate::parser::{Input, ParseResult};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while1, take_until},
    character::complete::{anychar, digit1, line_ending, multispace0, not_line_ending, space0},
    combinator::{all_consuming, map, map_res, opt, verify, peek, rest_len},
    multi::many0,
    sequence::{preceded, terminated, tuple},
};

const KEYWORDS: &[&str; 15] = &[
    "and", "or", "xor", "not", "true", "false", "if", "then", "else", "let", "in", "fn", "match",
    "to", "delay",
];

pub fn newlines<'a>(
    is_req: bool,
) -> impl Fn(Input<'a>) -> ParseResult<'a, (Vec<Input<'a>>, Option<Input<'a>>, Vec<Input<'a>>)> {
    move |input| {
        map_res(
            tuple((
                comment0,
                opt(alt((line_ending, single_comment, multi_comment))),
                multicomment0,
            )), // This parser cannot fail
            move |(ws1, nl, ws2)| match (is_req, nl) {
                // The error won't be used in any way, just indicates that it's an error
                (true, None) => Err(()),
                (_, nl) => Ok((ws1, nl, ws2)),
            },
        )(input)
        .map_err(newline_error)
    }
}

pub fn comment0(input: Input<'_>) -> ParseResult<'_, Vec<Input<'_>>> {
    terminated(many0(preceded(space0, inline_comment)), space0)(input)
}

pub fn multicomment0(input: Input<'_>) -> ParseResult<'_, Vec<Input<'_>>> {
    terminated(
        many0(preceded(multispace0, alt((single_comment, multi_comment)))),
        multispace0,
    )(input)
}

fn single_comment(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    preceded(
        tag("--"),
        alt((terminated(not_line_ending, line_ending), not_line_ending)),
    )(input)
}

fn inline_comment(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    verify(
        terminated(preceded(tag("{-"), take_until("-}")), tag("-}")),
        |s: &Input| !s.to_str().contains('\n'),
    )(input)
}

fn multi_comment(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    terminated(preceded(tag("{-"), take_until("-}")), tag("-}"))(input)
}

pub fn opt_nl<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    terminated(parser, newlines(false))
}

pub fn preceding_opt_nl<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    preceded(newlines(false), parser)
}

pub fn req_nl<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    terminated(
        parser,
        alt((all_consuming(newlines(false)), newlines(true))),
    )
}

pub fn token<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> ParseResult<'a, O>
where
    F: Fn(Input<'a>) -> ParseResult<'a, O>,
{
    preceded(space0, parser)
}

macro_rules! reserved {
    (keyword $lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
            token(
                terminated(
                    tag($lexeme_str),
                    alt((
                        peek(verify(anychar, |c| !is_identifier_char(*c))),
                        map( // Only used for typechecking purposes
                            verify(rest_len, |len| *len == 0),
                            |_| char::from(0) // This character will be ignored
                        )
                    ))
                )
            )(input).map_err(reserved_error($lexeme_str))
        }
    };

    ($lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
            token(tag($lexeme_str))(input).map_err(reserved_error($lexeme_str))
        }
    };
}

pub fn char(input: Input<'_>) -> ParseResult<'_, char> {
    token(terminated(preceded(single_quote, anychar), single_quote))(input).map_err(char_error)
}

pub fn string(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    token(terminated(
        preceded(double_quote, is_not("\"")),
        double_quote,
    ))(input)
    .map_err(string_error)
}

pub fn number(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    token(digit1)(input).map_err(number_error)
}

pub fn identifier(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    token(verify(
        take_while1(is_identifier_char),
        |id: &Input| !is_keyword(id.to_str()) && !id.to_str().starts_with('\''),
    ))(input)
    .map_err(ident_error)
}

fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '\''
}

reserved!(comma, ",");
reserved!(plus, "+");
reserved!(minus, "-");
reserved!(star, "*");
reserved!(slash, "/");
reserved!(modulo, "%");
reserved!(keyword and, "and");
reserved!(keyword or, "or");
reserved!(keyword xor, "xor");
reserved!(keyword not, "not");
reserved!(keyword true_val, "true");
reserved!(keyword false_val, "false");
reserved!(equal, "==");
reserved!(not_equal, "/=");
reserved!(less_than, "<");
reserved!(greater_than, ">");
reserved!(less_than_equal, "<=");
reserved!(greater_than_equal, ">=");
reserved!(left_paren, "(");
reserved!(right_paren, ")");
reserved!(keyword if_, "if");
reserved!(keyword then, "then");
reserved!(q_mark, "?");
reserved!(keyword else_, "else");
reserved!(keyword let_, "let");
reserved!(keyword in_, "in");
reserved!(assign, "=");
reserved!(keyword fn_, "fn");
reserved!(arrow, "->");
reserved!(keyword match_kw, "match");
reserved!(keyword to, "to");
reserved!(bar, "|");
reserved!(underscore, "_");
reserved!(keyword delay, "delay");
reserved!(single_quote, "'");
reserved!(double_quote, "\"");
reserved!(left_bracket, "[");
reserved!(right_bracket, "]");
reserved!(double_comma, ",,");

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

    // Reserved token parsing
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
    parser_test!(open_bracket_test (left_bracket): "[" => "[".into());
    parser_test!(close_bracket_test (right_bracket): "]" => "]".into());
    parser_test!(double_comma_test (double_comma): ",," => ",,".into());
    // Use find and replace
    // Find: reserved!\(([a-z_]+), ("[^"]+")\);
    // Replace: parser_test!($1_test ($1): $2 => $2.into());

    // Literal parsing
    parser_test!(number_test (number): "12" => "12".into());
    parser_test!(string_test (string): "\"abc\"" => span_at("abc", 2, 1, 1));
    basic_test!(char_test char("'a'".into()) => Ok((span_at("", 4, 1, 3), 'a')));

    // Comment tests
    parser_test!(inline_comment_test (inline_comment): "{- inline -}" => span_at(" inline ", 3, 1, 2));
    parser_test! {
        single_comment_test
        (single_comment): "-- single" => span_at(" single", 3, 1, 2);
        (single_comment): "-- single\n" => span_at(" single", 3, 1, 2)
    }
    parser_test! {
        multi_comment_test
        (multi_comment): "{- multi\ncomment -}" => span_at(" multi\ncomment ", 3, 1, 2)
    }
    basic_test! {
        comment_error_test
        inline_comment("{- \n -}".into()).is_err() => true;
        inline_comment("{- unclosed".into()).is_err() => true;
        multi_comment("{- unclosed".into()).is_err() => true
    }
    parser_test! {
        comment0_test
            (comment0): " \t {- comment -} \t " =>
                vec![
                    span_at(" comment ", 9, 1, 5)
                ];
            (comment0): " \t " => vec![];
            (comment0): "" => vec![]
    }
    parser_test! {
        multicomment0_test
            (multicomment0):
                "
                \t
                -- end of line
                {- multi \n\
                line -}
                " => vec![
                    span_at(" end of line", 19, 3, 37),
                    span_at(" multi \nline ", 19, 4, 68)
                ]
    }
}
