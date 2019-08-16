use nom::{
    IResult,
    sequence::{
        preceded,
    },
    character::complete::{
        multispace0,
    },
    bytes::complete::tag
};

fn token<'a, F, O>(parser: F) -> impl Fn(&'a str) -> IResult<&'a str, O>
where F: Fn(&'a str) -> IResult<&'a str, O> {
    preceded(multispace0, parser)
}

macro_rules! reserved {
    ($lexeme:ident, $lexeme_str:literal) => {
        pub fn $lexeme<'a>(input: &'a str) ->
        IResult<&'a str, &'a str> {
            token(tag($lexeme_str))(input)
        }
    };
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
reserved!(unit, "unit");
reserved!(true_val, "true");
reserved!(false_val, "false");

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn token_parser_test() {
        let parser = token(tag("abc"));
        assert_eq!(parser(" \t\nabc"), Ok(("", "abc")));
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
    parser_test!(unit_test (unit): "unit" => "unit");
    parser_test!(true_test (true_val): "true" => "true");
    parser_test!(false_test (false_val): "false" => "false");
}