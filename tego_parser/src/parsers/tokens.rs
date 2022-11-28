use crate::error::err_retain_all;
use crate::error::parser::{
    char_error, string_error,
    number_error, ident_error,
    multi_comment_error,
    inline_comment_error,
    single_comment_error,
    newline_error,
    reserved_error,
};
use crate::{Input, ParseResult};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while1, escaped_transform},
    character::complete::{anychar, digit1, line_ending, multispace0, not_line_ending, space0},
    combinator::{all_consuming, map, map_res, opt, peek, rest_len, verify, not as nom_not},
    multi::many0,
    sequence::{preceded, terminated, tuple, pair},
};

const KEYWORDS: &[&str; 16] = &[
    "and", "or", "xor", "not", "true", "false", "if", "then", "else", "let", "in", "fn", "match",
    "to", "delay", "do"
];

pub const INVALID_CHARS: &[char] = &[
    '\n', '\t', '\\'
];

const ESCAPE_CHARS_MAP: &[(char, char)] = &[
    ('n', '\n'),
    ('t', '\t'),
    ('"', '"'),
    ('\'', '\'')
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
    terminated(
        many0(
            preceded(space0, inline_comment)
        ),
        space0
    )(input)
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
    .map_err(single_comment_error)
}

fn inline_comment(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    verify(
        terminated(preceded(tag("{-"), take_until("-}")), tag("-}")),
        |s: &Input| !s.to_str().contains('\n'),
    )(input)
    .map_err(inline_comment_error(input))
}

fn multi_comment(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    terminated(preceded(tag("{-"), take_until("-}")), tag("-}"))(input)
        .map_err(multi_comment_error(input))
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

pub fn token<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> nom::IResult<Input<'a>, O>
where
    F: Fn(Input<'a>) -> nom::IResult<Input<'a>, O>,
{
    preceded(
        space0,
        err_retain_all(parser)
    )
}

macro_rules! reserved {
    (keyword $lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
            token(terminated(
                tag($lexeme_str),
                alt((
                    peek(verify(anychar, |c| !is_identifier_char(*c))),
                    map(
                        // Check if it's at the end of the file
                        verify(rest_len, |len| *len == 0),

                        // Only used for typechecking purposes
                        |_| char::from(0), // This character will be ignored
                    ),
                )),
            ))(input)
            .map_err(reserved_error($lexeme_str))
        }
    };

    ($lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
            token(tag($lexeme_str))(input).map_err(reserved_error($lexeme_str))
        }
    };
}

pub fn char(input: Input<'_>) -> ParseResult<'_, char> {
    token(preceded(
        tag("'"),
        terminated(
            alt((valid_char, escaped_char)),
            tag("'"))
        )
    )(input).map_err(char_error)
}

fn valid_char<'a, E>(input: Input<'a>) -> nom::IResult<Input<'a>, char, E>
where E: nom::error::ParseError<Input<'a>> {
    verify(anychar, |c| !INVALID_CHARS.contains(c))(input)
}

fn escaped_char<'a, E>(input: Input<'a>) -> nom::IResult<Input<'a>, char, E>
where E: nom::error::ParseError<Input<'a>> {
    map_res(
        preceded(tag("\\"), anychar),
        |c| ESCAPE_CHARS_MAP.iter().fold(
            Err(()),
            |acc, (escape_char, mapped_char)| if &c == escape_char {
                Ok(mapped_char.to_owned())
            } else {
                acc
            }
        )
    )(input)
}

pub fn string(input: Input<'_>) -> ParseResult<'_, String> {
    token(preceded(
        tag("\""),
        terminated(
            inner_string,
            tag("\""))
        )
    )(input).map_err(string_error)
}

fn inner_string<'a, E>(input: Input<'a>) -> nom::IResult<Input<'a>, String, E>
where E: nom::error::ParseError<Input<'a>> {
    escaped_transform(
        is_not("\"\\"),
        '\\',
        map_res(
            anychar,
            |c| ESCAPE_CHARS_MAP.iter().fold(
                Err(()),
                |acc, (escape_char, mapped_char)| if &c == escape_char {
                    Ok(mapped_char.to_owned())
                } else {
                    acc
                }
            )
        )
    )(input)
}

pub fn number(input: Input<'_>) -> ParseResult<'_, i32> {
    map_res(
        token(digit1),
        |i| i.to_str().parse::<i32>()
    )(input).map_err(number_error)
}

pub fn identifier(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    token(verify(take_while1(is_identifier_char), |id: &Input| {
        !is_keyword(id.to_str()) && !id.to_str().starts_with('\'')
    }))(input)
    .map_err(ident_error)
}

fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '\''
}

reserved!(comma, ",");
reserved!(plus, "+");
// Has to be done by hand because comments also start with '-'
pub fn minus(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    map(
        pair(token(tag("-")), peek(nom_not(tag("-")))),
        |(minus_op, _)| minus_op
    )(input)
    
    .map_err(reserved_error("-"))
}
reserved!(star, "*");
// Has to be done by hand because 'not_equal' also start with '/'
pub fn slash(input: Input<'_>) -> ParseResult<'_, Input<'_>> {
    map(
        pair(token(tag("/")), peek(nom_not(tag("=")))),
        |(slash_op, _)| slash_op
    )(input)
    
    .map_err(reserved_error("-"))
}
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
reserved!(left_bracket, "[");
reserved!(right_bracket, "]");
reserved!(double_comma, ",,");
reserved!(keyword do_, "do");
reserved!(dot, ".");

fn is_keyword(lexeme: &str) -> bool {
    KEYWORDS.iter().any(|keyword| keyword == &lexeme)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::span_at;
    use crate::test::*;
    use crate::error::parser::ParseErrorKind;

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
    parser_test!(open_bracket_test (left_bracket): "[" => "[".into());
    parser_test!(close_bracket_test (right_bracket): "]" => "]".into());
    parser_test!(double_comma_test (double_comma): ",," => ",,".into());
    parser_test!(do_test (do_): "do" => "do".into());
    parser_test!(dot_test (dot): "." => ".".into());
    // Use find and replace
    // Find: reserved!\(([a-z_]+), ("[^"]+")\);
    // Replace: parser_test!($1_test ($1): $2 => $2.into());

    // Identifier parsing
    parser_test!{
        ident_test
        (identifier): "abc" => "abc".into();
        (identifier): "ab'" => "ab'".into();
        (identifier): "AbC" => "AbC".into()
    }

    // Literal parsing
    parser_test!(number_test (number): "12" => 12);
    parser_test!(string_test (string): "\"abc\"" => "abc".into());
    parser_test!(string_escape_test (string): "\"\\n\\t\\\"\"" => "\n\t\"".into());
    basic_test!(char_test char("'a'".into()) => Ok((span_at("", 4, 1, 3), 'a')));
    basic_test! {
        char_escape_test
        char("'\\n'".into()) => Ok((span_at("", 5, 1, 4), '\n'))
    }

    // Comment tests
    parser_test!(inline_comment_test (inline_comment): "{- inline -}" => span_at(" inline ", 3, 1, 2));
    parser_test! {
        single_comment_test
        (single_comment): "-- single" => span_at(" single", 3, 1, 2);
        (single_comment): "-- single\n" => span_at(" single", 3, 1, 2)
    }
    basic_test! {
        single_comment_ends_line_test
        req_nl(char)("'a' -- single\n".into()) => Ok((span_at("", 1, 2, 14), 'a'))
    }
    parser_test! {
        multi_comment_test
        (multi_comment): "{- multi\ncomment -}" => span_at(" multi\ncomment ", 3, 1, 2)
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
{- multi \n
    line -}
            " => vec![
                span_at(" end of line", 3, 3, 5),
                span_at(" multi \n\n    line ", 3, 4, 20)
                ]
    }

    // Error tests
    basic_test! {
        char_error_tests
        char("".into()) => parse_error("".into(), 1, 1, ParseErrorKind::NoMatch);
        char("'".into()) => parse_error("'".into(), 1, 1, ParseErrorKind::CharUnclosed);
        char("'a".into()) => parse_error("'a".into(), 1, 1, ParseErrorKind::CharUnclosed);
        char("'ab'".into()) => parse_error("'ab'".into(), 1, 1, ParseErrorKind::CharUnclosed);
        char("'\n'".into()) => parse_error("'\n'".into(), 1, 1, ParseErrorKind::InvalidChar);
        char("'\t'".into()) => parse_error("'\t'".into(), 1, 1, ParseErrorKind::InvalidChar);
        char("'\\'".into()) => parse_error("'\\'".into(), 1, 1, ParseErrorKind::InvalidEscapedChar);
        char("'\\a'".into()) => parse_error("'\\a'".into(), 1, 1, ParseErrorKind::InvalidEscapedChar)
    }
    basic_test! {
        string_error_tests
        string("".into()) => parse_error("".into(), 1, 1, ParseErrorKind::NoMatch);
        string("\"".into()) => parse_error("\"".into(), 1, 1, ParseErrorKind::StringUnclosed);
        string("\"a".into()) => parse_error("\"a".into(), 1, 1, ParseErrorKind::StringUnclosed);
        string("\"a\\b\"".into()) => parse_error("\"a\\b\"".into(), 1, 1, ParseErrorKind::InvalidEscapedString)
    }
    basic_test! {
        number_error_tests
        number("".into()) => parse_error("".into(), 1, 1, ParseErrorKind::NoMatch);
        number("2147483648".into()) => parse_error("2147483648".into(), 1, 1, ParseErrorKind::NumberTooBig)
    }
    basic_test! {
        identifier_error_tests
        identifier("".into()) => parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        identifier("*".into()) => parse_error("*".into(), 1, 1, ParseErrorKind::NoMatch);
        identifier("true".into()) => parse_error("true".into(), 1, 1, ParseErrorKind::KeywordIdentifier)
    }
    basic_test! {
        comment_error_tests
        inline_comment("{- \n -}".into()) => parse_error("{- \n -}".into(), 1, 1, ParseErrorKind::UnexpectedNewline);
        inline_comment("{- unclosed inline".into()) => parse_error("{- unclosed inline".into(), 1, 1, ParseErrorKind::UnclosedComment);
        multi_comment("{- unclosed multi".into()) => parse_error("{- unclosed multi".into(), 1, 1, ParseErrorKind::UnclosedComment)
    }
}
            