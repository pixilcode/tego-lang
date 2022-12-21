use crate::error::{
    ParseErrorKind,
    handlers::{
        try_parser,
        expect_rhs,
        expect_keyword,
        expect_match,
        expect_match_arms,
        expect_expr,
        expect_variable,
        missing_rhs_error,
        terminating_paren_error,
        terminating_bracket_error,
        num_expr_error,
        string_expr_error,
        char_expr_error,
        literal_error,
    }
};
use crate::parsers::match_::*;
use crate::parsers::tokens::*;
use crate::{ExprOutput, MatchOutput};
use crate::Input;
use crate::ParseResult;

use nom::{
    branch::alt,
    combinator::{opt, map},
    multi::{fold_many0, many1},
    sequence::{pair, preceded, separated_pair, terminated, tuple},
};

type ExprResult<'a, E> = ParseResult<'a, E>;

macro_rules! binary_expr {
    ($name:ident, $op_func:expr, $next_precedence:ident) => {
        fn $name<E>(input: Input<'_>) -> ExprResult<'_, E>
        where
            E: ExprOutput,
        {
            $next_precedence(input)
            .and_then(|(input, lhs)|
                fold_many0(
                    pair(opt_nl($op_func), expect_rhs($next_precedence)),
                    lhs,
                    |lhs, (op, rhs)| E::binary(lhs, op.to_str(), rhs)
                )(input)
            )
        }
    };
}

macro_rules! unary_expr {
    ($name:ident, $op_func:expr, $next_precedence:ident) => {
        fn $name<E>(input: Input<'_>) -> ExprResult<'_, E>
        where
            E: ExprOutput,
        {
            pair($op_func, expect_rhs($name))(input)
                .and_then(|(input, (op, a))| Ok((input, E::unary(op.to_str(), a))))
                .or_else(try_parser($next_precedence, input))
        }
    };
}

pub fn expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    do_expr(input)
}

pub fn do_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    alt((
        map(
            tuple((
                preceded(
                    do_,
                    expect_expr(expr),
                ),
                opt(preceded(
                    in_,
                    expect_match(match_)
                )),
                preceded(
                    opt_nl(expect_keyword(then, ParseErrorKind::DoThen)),
                    expect_expr(expr)
                )
            )),

            |(command, command_match, body)|
                E::do_expr(command, command_match.unwrap_or_else(E::Match::ignore), body)
        ),

        let_expr
    ))(input)
}

pub fn let_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    alt((
        |input|
        alt((let_, delay))(input)
            .and_then(|(input, let_token)| match let_token.into() {
                "let" =>
                    map(
                        tuple((
                            expect_match(match_),
                            expect_keyword(assign, ParseErrorKind::LetAssign),
                            expect_expr(if_expr),
                            opt_nl(expect_keyword(in_, ParseErrorKind::LetIn)),
                            expect_expr(expr)
                        )),
                        |(ident, _, value, _, inner)| E::let_expr(ident, value, inner)
                    )(input),
                "delay" =>
                    map(
                        tuple((
                            expect_variable(variable),
                            expect_keyword(assign, ParseErrorKind::DelayAssign),
                            expect_expr(join_expr),
                            opt_nl(expect_keyword(in_, ParseErrorKind::DelayIn)),
                            expect_expr(expr)
                        )),
                        |(ident, _, value, _, inner)| E::delayed(ident, value, inner)
                    )(input),
                _ => unreachable!(),
            }),

        if_expr
    ))(input)
}

pub fn if_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    alt((
        map(
            tuple((
                if_,
                expect_expr(join_expr),
                preceded(
                    opt_nl(expect_keyword(alt((then, q_mark)), ParseErrorKind::Then)),
                    opt_nl(expect_expr(expr))
                ),
                preceded(
                    opt_nl(expect_keyword(else_, ParseErrorKind::Else)),
                    expect_expr(expr)
                )
            )),
            |(_, cond, t, f)| E::if_expr(cond, t, f)
        ),

        match_expr
    ))(input)
}

pub fn match_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    alt((
        map(
            tuple((
                match_kw,
                terminated(
                    expect_expr(join_expr),
                    expect_keyword(to, ParseErrorKind::MatchTo)
                ),
                // nl has to be preceding so as not to conflict with
                // the `req_nl` parser that likely directly follows the match expr
                expect_match_arms(many1(preceding_opt_nl(match_arm)))
            )),
            |(_, val, patterns)| E::match_(val, patterns)
        ),

        join_expr
    ))(input)
}

pub fn match_arm<E>(input: Input<'_>) -> ParseResult<'_, (E::Match, E)>
where
    E: ExprOutput,
{
    tuple((
        preceded(
            bar,
            expect_match(match_)
        ),
        preceded(
            opt_nl(expect_keyword(arrow, ParseErrorKind::MatchArrow)),
            expect_expr(expr)
        )
    ))(input)
}

binary_expr!(join_expr, comma, flat_join_expr);
binary_expr!(flat_join_expr, double_comma, or_expr);
binary_expr!(or_expr, or, xor_expr);
binary_expr!(xor_expr, xor, and_expr);
binary_expr!(and_expr, and, equal_expr);
binary_expr!(equal_expr, alt((equal, not_equal)), compare_expr);
binary_expr!(
    compare_expr,
    alt((less_than_equal, greater_than_equal, less_than, greater_than)),
    add_expr
);
binary_expr!(add_expr, alt((plus, minus)), mult_expr);
binary_expr!(mult_expr, alt((star, slash, modulo)), negate_expr);

unary_expr!(negate_expr, minus, not_expr);
unary_expr!(not_expr, not, fn_expr);

fn fn_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    alt((
        map(
            preceded(
                fn_,
                separated_pair(
                    expect_match(match_),
                    opt_nl(expect_keyword(arrow, ParseErrorKind::FnArrow)),
                    expect_expr(expr)
                )
            ),
            |(param, body)| E::fn_expr(param, body)
        ),

        fn_application
    ))(input)
}

fn fn_application<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    dot_expr(input).and_then(|(input, val)| fold_many0(dot_expr, val, E::fn_app)(input))
}

fn dot_expr<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    pair(
        grouping,
        opt(opt_nl(dot))
    )(input)
    .and_then(|(input, (lhs, op))| match op {
        // An operator is found
        Some(_) => fn_application(input)
            // The dot operator is reverse function application
            .map(|(input, rhs)| (input, E::fn_app(rhs, lhs)))
            .map_err(missing_rhs_error),
        // No operator is found
        None => Ok((input, lhs))
    })
}

fn grouping<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    opt_nl(left_paren)(input)
        .and_then(|(input, open_paren)| {
            right_paren(input)
                .map(|(input, _)| (input, E::unit()))
                .or_else(try_parser(terminated(opt_nl(expr), right_paren), input))
                .map_err(terminating_paren_error(
                    open_paren.line(),
                    open_paren.column(),
                ))
        })
        .or_else(try_parser(
            |input| {
                opt_nl(left_bracket)(input).and_then(|(input, open_bracket)| {
                    terminated(opt_nl(expr), right_bracket)(input)
                        .map(|(input, inner)| (input, E::boxed(inner)))
                        .map_err(terminating_bracket_error(
                            open_bracket.line(),
                            open_bracket.column(),
                        ))
                })
            },
            input,
        ))
        .or_else(try_parser(literal, input))
}

fn literal<E>(input: Input<'_>) -> ExprResult<'_, E>
where
    E: ExprOutput,
{
    map(
        alt((true_val, false_val)),
        |token| match token.into() {
            "true" => E::bool(true),
            "false" => E::bool(false),
            _ => unreachable!(),
        }
    )(input)
    .or_else(
        try_parser(
            map(identifier, |lexeme| E::variable(lexeme.into())),
            input
        )
    )
    .or_else(
        try_parser(
            map(number, |int| E::int(int)),
            input
        )
    )
    .map_err(num_expr_error)
    .or_else(
        try_parser(
            map(string, |s| E::string(s)),
            input
        )
    )
    .map_err(string_expr_error)
    .or_else(
        try_parser(
            map(char, |c| E::char(c)),
            input
        )
    )
    .map_err(char_expr_error)
    .map_err(literal_error)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Match};
    use crate::span::span_at;
    use crate::test::*;

    parser_test! {
        literal_test
        (expr): "1" => Expr::int(1);
        (expr): "true" => Expr::bool(true);
        (expr): "false" => Expr::bool(false);
        (expr): "()" => Expr::unit()
    }
    parser_test! {
        or_test
        (expr): "true or\nfalse" =>
            Expr::or(
                Expr::bool(true),
                Expr::bool(false))
    }
    parser_test! {
        and_test
        (expr): "true and\nfalse" =>
            Expr::and(
                Expr::bool(true),
                Expr::bool(false))
    }
    parser_test! {
        plus_test
        (expr): "1 +\n2 + 3" =>
            Expr::plus(
                Expr::plus(
                    Expr::int(1),
                    Expr::int(2)
                ),
                Expr::int(3)
            )
    }
    parser_test! {
        minus_test
        (expr): "1 -\n2" =>
            Expr::minus(
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        multiply_test
        (expr): "1 *\n2" =>
            Expr::multiply(
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        divide_test
        (expr): "1 /\n2" =>
            Expr::divide(
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        modulo_test
        (expr): "1 %\n2" =>
            Expr::modulo(
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        equal_test
        (expr): "1 ==\n2" =>
            Expr::equal(
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        negate_test
        (expr): "-1" =>
            Expr::negate(
                Expr::int(1))
    }
    parser_test! {
        not_test
        (expr): "not true" =>
            Expr::not(
                Expr::bool(true))
    }
    parser_test! {
        grouping_test
        (expr): "1 + 2 * 3" =>
            Expr::plus(
                Expr::int(1),
                Expr::multiply(
                    Expr::int(2),
                    Expr::int(3)));
        (expr): "(1 + 2) * 3" =>
            Expr::multiply(
                Expr::plus(
                    Expr::int(1),
                    Expr::int(2)),
                Expr::int(3))
    }
    parser_test! {
        precedence_test
        (expr): "1 or 2 and 3 == 4 + 5 - 6 * 7 / 8 % 9" =>
            Expr::or(
                Expr::int(1),
                Expr::and(
                    Expr::int(2),
                    Expr::equal(
                        Expr::int(3),
                        Expr::minus(
                            Expr::plus(
                                Expr::int(4),
                                Expr::int(5)),
                            Expr::modulo(
                                Expr::divide(
                                    Expr::multiply(
                                        Expr::int(6),
                                        Expr::int(7)),
                                    Expr::int(8))
                                , Expr::int(9))))));
        (expr): "1 and 2 and 3" =>
            Expr::and(
                Expr::and(
                    Expr::int(1),
                    Expr::int(2)),
                Expr::int(3));
        (expr): "1 == 2 /= 3 < 4 > 5 <= 6 >= 7 and 8" =>
            Expr::and(
                Expr::not_equal(
                    Expr::equal(
                        Expr::int(1),
                        Expr::int(2)),
                    Expr::greater_than_equal(
                        Expr::less_than_equal(
                            Expr::greater_than(
                                Expr::less_than(
                                    Expr::int(3), Expr::int(4)),
                                Expr::int(5)),
                            Expr::int(6)),
                        Expr::int(7))),
                Expr::int(8))
    }
    parser_test! {
        if_else_test
        (expr): "if true then\n1\nelse\n2" =>
            Expr::if_expr(
                Expr::bool(true),
                Expr::int(1),
                Expr::int(2));
        (expr): "if true ?\n1\nelse\n2" =>
            Expr::if_expr(
                Expr::bool(true),
                Expr::int(1),
                Expr::int(2))
    }
    parser_test! {
        variable_test
        (expr): "abc" =>
            Expr::variable("abc")
    }
    parser_test! {
        let_test
        (expr): "let a = 1 in\ntrue" =>
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::bool(true));
        (expr): "let a = 1 in\n2" =>
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::int(2)
            )

    }
    parser_test! {
        fn_test
        (expr): "fn a ->\na" =>
            Expr::fn_expr(
                Match::ident("a"),
                Expr::variable("a"));
        (expr): "fn a ->\na + 1" =>
            Expr::fn_expr(
                Match::ident("a"),
                Expr::plus(
                    Expr::variable("a"),
                    Expr::int(1)))
    }
    parser_test! {
        fn_application_test
        (expr): "a 1" =>
            Expr::fn_app(
                Expr::variable("a"),
                Expr::int(1));
        (expr): "a (\n1, 2\n)" =>
            Expr::fn_app(
                Expr::variable("a"),
                Expr::join(
                    Expr::int(1),
                    Expr::int(2)
                )
            );
        (expr): "a 1 2" =>
            Expr::fn_app(
                Expr::fn_app(
                    Expr::variable("a"),
                    Expr::int(1)),
                Expr::int(2))
    }
    parser_test! {
        match_expr_test
        (expr): "match 1 to\n| 1 -> true\n| a -> false" =>
            Expr::match_(Expr::int(1), vec![
                (Match::int(1), Expr::bool(true)),
                (Match::ident("a"), Expr::bool(false))
            ])
    }
    parser_test! {
        delayed_value_test
        (expr): "delay a = 1 in a" =>
            Expr::delayed(
                Match::ident("a"),
                Expr::int(1),
                Expr::variable("a")
            )
    }
    parser_test! {
        do_expr_test
        (expr): "do println 1 in a then b" =>
            Expr::do_expr(
                Expr::fn_app(Expr::variable("println"), Expr::int(1)),
                Match::ident("a"),
                Expr::variable("b")
            );
        (expr): "do println 1 then a" =>
            Expr::do_expr(
                Expr::fn_app(Expr::variable("println"), Expr::int(1)), 
                Match::ignore(),
                Expr::variable("a")
            )
    }
    parser_test! {
        string_test
        (expr): "\"abc\"" => Expr::string("abc".into())
    }
    parser_test! {
        char_test
        (expr): "'a'" => Expr::char('a')
    }
    parser_test! {
        boxed_tuple_test
        (expr): "[1, 2]" => Expr::boxed(Expr::join(Expr::int(1), Expr::int(2)))
    }
    parser_test! {
        flat_join_test
        (expr): "1 ,, 2" => Expr::flat_join(Expr::int(1), Expr::int(2))
    }
    parser_test! {
        dot_expr_test
        (expr): "a.b" => Expr::fn_app(Expr::variable("b"), Expr::variable("a"));
        (expr): "a .\nb" => Expr::fn_app(Expr::variable("b"), Expr::variable("a"));
        (expr): "a.b c" => Expr::fn_app(
            Expr::fn_app(Expr::variable("b"), Expr::variable("c")),
            Expr::variable("a")
        );
        (expr): "(a b).c" => Expr::fn_app(
            Expr::variable("c"),
            Expr::fn_app(Expr::variable("a"), Expr::variable("b")))
    }

    // Error tests
    basic_test! {
        literal_error_test
        literal::<()>("".into()) => 
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        
        // Char literals
        literal::<()>("'".into()) => parse_failure("'".into(), 1, 1, ParseErrorKind::CharUnclosed);
        literal::<()>("'a".into()) => parse_failure("'a".into(), 1, 1, ParseErrorKind::CharUnclosed);
        literal::<()>("'ab'".into()) => parse_failure("'ab'".into(), 1, 1, ParseErrorKind::CharUnclosed);
        literal::<()>("'\n'".into()) => parse_failure("'\n'".into(), 1, 1, ParseErrorKind::InvalidChar);
        literal::<()>("'\t'".into()) => parse_failure("'\t'".into(), 1, 1, ParseErrorKind::InvalidChar);
        literal::<()>("'\\'".into()) => parse_failure("'\\'".into(), 1, 1, ParseErrorKind::InvalidEscapedChar);
        literal::<()>("'\\a'".into()) => parse_failure("'\\a'".into(), 1, 1, ParseErrorKind::InvalidEscapedChar);
        
        // String literals
        literal::<()>("\"".into()) => parse_failure("\"".into(), 1, 1, ParseErrorKind::StringUnclosed);
        literal::<()>("\"a".into()) => parse_failure("\"a".into(), 1, 1, ParseErrorKind::StringUnclosed);
        literal::<()>("\"a\\b\"".into()) => parse_failure("\"a\\b\"".into(), 1, 1, ParseErrorKind::InvalidEscapedString);

        // Number literals
        literal::<()>("2147483648".into()) => parse_failure("2147483648".into(), 1, 1, ParseErrorKind::NumberTooBig)
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
            parse_failure(span_at("=> ", 7, 1, 6), 7, 1, ParseErrorKind::TerminatingBracket(1, 1));
        grouping::<()>("[1, 2, 'a".into()) =>
            parse_failure(span_at("'a", 8, 1, 7), 8, 1, ParseErrorKind::CharUnclosed)
    }

    basic_test! {
        dot_expr_error_test
        dot_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        dot_expr::<()>("1 .".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        dot_expr::<()>("1 . 'a".into()) =>
            parse_failure(span_at("'a", 5, 1, 4), 5, 1, ParseErrorKind::CharUnclosed)
    }

    basic_test! {
        fn_expr_error_test
        fn_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof)
    }

    basic_test! {
        comment_after_expr_test
        opt_nl(expr::<Expr>)("1 -- ignore this".into()) => Ok((span_at("", 17, 1, 16), Expr::int(1)))
    }

    basic_test! {
        op_error_test
        join_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        join_expr::<()>("1 ,".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        flat_join_expr::<()>("1 ,,".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs);
        or_expr::<()>("true or".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::MissingRhs);
        xor_expr::<()>("true xor".into()) =>
            parse_failure(span_at("", 9, 1, 8), 9, 1, ParseErrorKind::MissingRhs);
        and_expr::<()>("true and".into()) =>
            parse_failure(span_at("", 9, 1, 8), 9, 1, ParseErrorKind::MissingRhs);
        equal_expr::<()>("1 ==".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs);
        equal_expr::<()>("1 /=".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs);
        compare_expr::<()>("1 <=".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs);
        compare_expr::<()>("1 >=".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs);
        compare_expr::<()>("1 <".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        compare_expr::<()>("1 >".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        add_expr::<()>("1 +".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        add_expr::<()>("1 -".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        mult_expr::<()>("1 *".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        mult_expr::<()>("1 /".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        mult_expr::<()>("1 %".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        negate_expr::<()>("-".into()) =>
            parse_failure(span_at("", 2, 1, 1), 2, 1, ParseErrorKind::MissingRhs);
        not_expr::<()>("not".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::MissingRhs);
        
        join_expr::<()>("1 , >".into()) =>
            parse_failure(span_at(">", 5, 1, 4), 5, 1, ParseErrorKind::MissingRhs)
    }

    basic_test! {
        match_error_test
        match_arm::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::ExpectedKeyword {
                keyword: "|",
                line: 1,
                column: 1
            });
        match_arm::<()>("| ".into()) =>
            parse_failure(span_at("", 3, 1, 2), 3, 1, ParseErrorKind::ExpectedMatch);
        match_arm::<()>("| 'a".into()) =>
            parse_failure(span_at("'a", 3, 1, 2), 3, 1, ParseErrorKind::CharUnclosed);
        match_arm::<()>("| ->".into()) =>
            parse_failure(span_at("->", 3, 1, 2), 3, 1, ParseErrorKind::ExpectedMatch);
        match_arm::<()>("| _ ".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::MatchArrow);
        match_arm::<()>("| _ -> ".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::ExpectedExpr);
        match_arm::<()>("| _ -> 'a".into()) =>
            parse_failure(span_at("'a", 8, 1, 7), 8, 1, ParseErrorKind::CharUnclosed);
        match_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        match_expr::<()>("if".into()) =>
            parse_error("if".into(), 1, 1, ParseErrorKind::NoMatch);
        match_expr::<()>("match".into()) =>
            parse_failure(span_at("", 6, 1, 5), 6, 1, ParseErrorKind::ExpectedExpr);
        match_expr::<()>("match 1".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::MatchTo);
        match_expr::<()>("match 1 to".into()) =>
            parse_failure(span_at("", 11, 1, 10), 11, 1, ParseErrorKind::MatchBar);
        match_expr::<()>(
            "match 1 to\n\
             | -> 1".into()
        ) =>
            parse_failure(span_at("-> 1", 3, 2, 13), 3, 2, ParseErrorKind::ExpectedMatch);
        match_expr::<()>(
            "match 1 to\n\
             | _ -> 1\n\
             | _ 2".into()
        ) =>
            parse_failure(span_at("2", 5, 3, 24), 5, 3, ParseErrorKind::MatchArrow)
    }

    basic_test! {
        if_error_test
        if_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        if_expr::<()>("let".into()) =>
            parse_error("let".into(), 1, 1, ParseErrorKind::NoMatch);
        if_expr::<()>("if".into()) =>
            parse_failure(span_at("", 3, 1, 2), 3, 1, ParseErrorKind::ExpectedExpr);
        if_expr::<()>("if then".into()) =>
            parse_failure(span_at("then", 4, 1, 3), 4, 1, ParseErrorKind::ExpectedExpr);
        if_expr::<()>("if 1 then else".into()) =>
            parse_failure(span_at("else", 11, 1, 10), 11, 1, ParseErrorKind::ExpectedExpr);
        if_expr::<()>("if 1 then 2 let".into()) =>
            parse_failure(span_at("let", 13, 1, 12), 13, 1, ParseErrorKind::Else);
        if_expr::<()>("if 1 then 2 else".into()) =>
            parse_failure(span_at("", 17, 1, 16), 17, 1, ParseErrorKind::ExpectedExpr)
    }

    basic_test! {
        let_error_test
        let_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        let_expr::<()>("*".into()) =>
            parse_error("*".into(), 1, 1, ParseErrorKind::NoMatch);
        let_expr::<()>("let".into()) =>
            parse_failure(span_at("", 4, 1, 3), 4, 1, ParseErrorKind::ExpectedMatch);
        let_expr::<()>("delay".into()) =>
            parse_failure(span_at("", 6, 1, 5), 6, 1, ParseErrorKind::ExpectedVariable);
        let_expr::<()>("let _".into()) =>
            parse_failure(span_at("", 6, 1, 5), 6, 1, ParseErrorKind::LetAssign);
        let_expr::<()>("delay a".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::DelayAssign);
        let_expr::<()>("let _ =".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::ExpectedExpr);
        let_expr::<()>("delay a =".into()) =>
            parse_failure(span_at("", 10, 1, 9), 10, 1, ParseErrorKind::ExpectedExpr);
        let_expr::<()>("let _ = 1".into()) =>
            parse_failure(span_at("", 10, 1, 9), 10, 1, ParseErrorKind::LetIn);
        let_expr::<()>("delay a = 1".into()) =>
            parse_failure(span_at("", 12, 1, 11), 12, 1, ParseErrorKind::DelayIn);
        let_expr::<()>("let _ = 1 in".into()) =>
            parse_failure(span_at("", 13, 1, 12), 13, 1, ParseErrorKind::ExpectedExpr);
        let_expr::<()>("delay a = 1 in".into()) =>
            parse_failure(span_at("", 15, 1, 14), 15, 1, ParseErrorKind::ExpectedExpr);
        let_expr::<()>("let _ = 1 in 'a".into()) =>
            parse_failure(span_at("'a", 14, 1, 13), 14, 1, ParseErrorKind::CharUnclosed);
        let_expr::<()>("delay a = 1 in 'a".into()) =>
            parse_failure(span_at("'a", 16, 1, 15), 16, 1, ParseErrorKind::CharUnclosed)
    }

    basic_test! {
        do_expr_error_test
        do_expr::<()>("".into()) =>
            parse_error("".into(), 1, 1, ParseErrorKind::Eof);
        do_expr::<()>("*".into()) =>
            parse_error("*".into(), 1, 1, ParseErrorKind::NoMatch);
        do_expr::<()>("do".into()) =>
            parse_failure(span_at("", 3, 1, 2), 3, 1, ParseErrorKind::ExpectedExpr);
        do_expr::<()>("do 1".into()) =>
            parse_failure(span_at("", 5, 1, 4), 5, 1, ParseErrorKind::DoThen);
        do_expr::<()>("do 1 in".into()) =>
            parse_failure(span_at("", 8, 1, 7), 8, 1, ParseErrorKind::ExpectedMatch);
        do_expr::<()>("do 1 in _".into()) =>
            parse_failure(span_at("", 10, 1, 9), 10, 1, ParseErrorKind::DoThen);
        do_expr::<()>("do 1 then".into()) =>
            parse_failure(span_at("", 10, 1, 9), 10, 1, ParseErrorKind::ExpectedExpr)
    }
}
