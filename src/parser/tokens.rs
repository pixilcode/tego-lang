use nom::{
    IResult,
    sequence::{
        preceded,
        terminated
    },
    character::complete::{
        space0,
        digit1,
        multispace0,
        multispace1
    },
    bytes::complete::{
        tag,
        take_till1
    }
};

const KEYWORDS: &[&str; 14] = &[
    "and",
    "or",
    "xor",
    "not",
    "true",
    "false",
    "if",
    "then",
    "else",
    "let",
    "in",
    "fn",
    "match",
    "to"
];

pub fn opt_nl<'a, F, O>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, O>
where F: Fn(&'a str) -> IResult<&'a str, O> {
    terminated(parser, multispace0)
}

pub fn req_nl<'a, F, O>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, O>
where F: Fn(&'a str) -> IResult<&'a str, O> {
    terminated(parser, multispace1)
}

fn token<'a, F, O>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, O>
where F: Fn(&'a str) -> IResult<&'a str, O> {
    preceded(space0, parser)
}

macro_rules! reserved {
    ($lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme<'a>(input: &'a str) ->
        IResult<&'a str, &'a str> {
            token(tag($lexeme_str))(input)
        }
    };
}

pub fn number(input: &'_ str) -> IResult<&'_ str, &'_ str> {
    token(digit1)(input)
}

pub fn identifier(input: &'_ str) -> IResult<&'_ str, &'_ str> {
    token(take_till1(|c: char| !c.is_ascii_alphabetic() && c != '\''))(input)
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

pub fn is_keyword(lexeme: &str) -> bool {
    KEYWORDS.iter().any(
        |keyword| keyword == &lexeme
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn token_parser_test() {
        let parser = token(tag("abc"));
        assert_eq!(parser(" \tabc"), Ok(("", "abc")));
    }
    
    parser_test!(comma_test (comma): "," => ",");
    parser_test!(plus_test (plus): "+" => "+");
    parser_test!(minus_test (minus): "-" => "-");
    parser_test!(star_test (star): "*" => "*");
    parser_test!(slash_test (slash): "/" => "/");
    parser_test!(modulo_test (modulo): "%" => "%");
    parser_test!(and_test (and): "and" => "and");
    parser_test!(or_test (or): "or" => "or");
    parser_test!(xor_test (xor): "xor" => "xor");
    parser_test!(not_test (not): "not" => "not");
    parser_test!(true_test (true_val): "true" => "true");
    parser_test!(false_test (false_val): "false" => "false");
    parser_test!(left_paren_test (left_paren): "(" => "(");
    parser_test!(right_paren_test (right_paren): ")" => ")");
    parser_test!(if_test (if_): "if" => "if");
    parser_test!(then_test (then): "then" => "then");
    parser_test!(q_mark_test (q_mark): "?" => "?");
    parser_test!(else_test (else_): "else" => "else");
    parser_test!(colon_test (colon): ":" => ":");
    parser_test!(number_test (number): "12" => "12");
    parser_test!(identifier_test (identifier): "aBc'" => "aBc'");
    parser_test!(let_test (let_): "let" => "let");
    parser_test!(in_test (in_): "in" => "in");
    parser_test!(assign_test (assign): "=" => "=");
    parser_test!(fn_test (fn_): "fn" => "fn");
    parser_test!(arrow_test (arrow): "->" => "->");
    parser_test!(match_kw_test (match_kw): "match" => "match");
    parser_test!(to_test (to): "to" => "to");
    parser_test!(bar_test (bar): "|" => "|");
    parser_test!(underscore_test (underscore): "_" => "_");
    
    // Use find and replace
    // Find: reserved!\(([a-z_]+), ("[^"]+")\);
    // Replace: parser_test!(\1_test (\1): \2 => \2);
    
}