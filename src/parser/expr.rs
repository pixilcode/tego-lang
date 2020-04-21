use crate::ast::{Expr, BinaryOp, UnaryOp};
use crate::parser::tokens::*;
use crate::parser::match_::*;

use nom::{
    IResult,
    sequence::{separated_pair, pair, terminated},
    combinator::opt,
    branch::alt,
    multi::many1,
    error::ErrorKind
};

type ExprResult<'a> = IResult<&'a str, Expr>;

macro_rules! binary_expr {
    ($name:ident, $op_func:expr, $next_precedence:ident) => {
        fn $name(input: &'_ str) -> ExprResult<'_> {
            pair(
                $next_precedence,
                opt(many1(pair($op_func, $next_precedence)))
            )(input).and_then(
                |(input, (a, other))|
                match other {
                    // Operators found (left to right)
                    Some(others) =>
                        Ok((input, others.into_iter().fold(
                            a,
                            |a, (op, b)| Expr::binary(a, BinaryOp::from(op), b)
                        ))),
                    // No operators found
                    None => Ok((input, a))
                })
        }
    };
}

macro_rules! unary_expr {
    ($name:ident, $op_func:expr, $next_precedence:ident) => {
        fn $name(input: &'_ str) -> ExprResult<'_> {
            opt(pair($op_func, $name))(input).and_then(
                |(input, unary_op)|
                match unary_op {
                    Some((op, a)) =>
                        Ok((input, Expr::unary(
                            UnaryOp::from(op),
                            a
                        ))),
                    None => $next_precedence(input)
                }
            )
        }
    }
}

pub fn expr(input: &'_ str) -> ExprResult<'_> {
    let_expr(input)
}

pub fn let_expr(input: &'_ str) -> ExprResult<'_> {
    opt(let_)(input).and_then(
        |(input, let_token)|
        match let_token {
            Some(_) =>
                separated_pair(
                    separated_pair(
                        match_,
                        assign,
                        join_expr
                    ),
                    in_,
                    expr
                )(input).map(
                    |(input, ((ident, value), inner))|
                    (
                        input,
                        Expr::let_expr(ident, value, inner)
                    )
                ),
            None => if_expr(input)
        }
    )
}

pub fn if_expr(input: &'_ str) -> ExprResult<'_> {
    opt(if_)(input).and_then(
        |(input, if_token)|
        match if_token {
            Some(_) => pair(join_expr, alt((then, q_mark)))(input).and_then(
                |(input, (cond, symbol))| {
                    let next_symbol = match symbol {
                        "then" => else_,
                        "?" => colon,
                        _ => unreachable!()
                    };
                    separated_pair(expr, next_symbol, expr)(input).map(
                        |(input, (t, f))|
                        (input, Expr::if_expr(cond, t, f))
                    )
                }
            ),
            None => join_expr(input)
        }
    )
}

binary_expr!(join_expr, comma, or_expr);
binary_expr!(or_expr, or, xor_expr);
binary_expr!(xor_expr, xor, and_expr);
binary_expr!(and_expr, and, equal_expr);
binary_expr!(equal_expr, alt((equal, not_equal)), compare_expr);
binary_expr!(compare_expr,
    alt((
        less_than_equal,
        greater_than_equal,
        less_than,
        greater_than)),
    add_expr);
binary_expr!(add_expr, alt((plus, minus)), mult_expr);
binary_expr!(mult_expr, alt((star, slash, modulo)), negate_expr);

unary_expr!(negate_expr, minus, not_expr);
unary_expr!(not_expr, not, fn_expr);

fn fn_expr(input: &'_ str) -> ExprResult<'_> {
    opt(fn_)(input).and_then(
        |(input, paren_token)|
        match paren_token {
            Some(_) =>
                separated_pair(match_, arrow, expr)(input).map(
                    |(input, (param, body))|
                    (input, Expr::fn_expr(param, body))
                ),
            None => fn_application(input)
        }
    )
}

fn fn_application(input: &'_ str) -> ExprResult<'_> {
    pair(grouping, opt(grouping))(input).and_then(
        |(input, (val, arg))|
        match arg {
            Some(arg) => Ok((input, Expr::fn_app(val, arg))),
            None => Ok((input, val))
        }
    )
}

fn grouping(input: &'_ str) -> ExprResult<'_> {
    opt(left_paren)(input).and_then(
        |(input, paren_token)|
        match paren_token {
            Some(_) =>
                terminated(
                    opt(expr),
                    right_paren
                )(input)
                .map(|(input, opt_exp)|
                    (input, opt_exp.unwrap_or_else(Expr::unit))
                ),
            None => literal(input)
        }
    )
}

fn literal(input: &'_ str) -> ExprResult<'_> {
    alt((true_val, false_val, number, identifier))(input).and_then(
        |(new_input, token)|
        match token {
            "true" => Ok((new_input, Expr::bool(true))),
            "false" => Ok((new_input, Expr::bool(false))),
            lexeme if is_keyword(lexeme) =>
                Err(nom::Err::Error((input, ErrorKind::Tag))),
            lexeme =>
                if let Ok(i) = lexeme.parse::<i32>() {
                    Ok((new_input, Expr::int(i)))
                } else {
                    Ok((new_input, Expr::variable(lexeme)))
                }
        }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Match;
    
    parser_test! {
        literal_test
        (expr): "1" => Expr::int(1);
        (expr): "true" => Expr::bool(true);
        (expr): "false" => Expr::bool(false);
        (expr): "()" => Expr::unit()
    }
    
    parser_test! {
        or_test
        (expr): "true or false" =>
            Expr::or(
                Expr::bool(true),
                Expr::bool(false))
    }
    
    parser_test! {
        and_test
        (expr): "true and false" =>
            Expr::and(
                Expr::bool(true),
                Expr::bool(false))
    }
    
    parser_test! {
        plus_test
        (expr): "1 + 2" =>
            Expr::plus(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        minus_test
        (expr): "1 - 2" =>
            Expr::minus(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        multiply_test
        (expr): "1 * 2" =>
            Expr::multiply(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        divide_test
        (expr): "1 / 2" =>
            Expr::divide(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        modulo_test
        (expr): "1 % 2" =>
            Expr::modulo(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        equal_test
        (expr): "1 == 2" =>
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
        (expr): "if true then 1 else 2" =>
            Expr::if_expr(
                Expr::bool(true),
                Expr::int(1),
                Expr::int(2));
        (expr): "if true ? 1 : 2" =>
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
        (expr): "let a = 1 in true" =>
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::bool(true));
        (expr): "let a = 1 in 2" =>
            Expr::let_expr(
                Match::ident("a"),
                Expr::int(1),
                Expr::int(2)
            )

    }
    
    parser_test! {
        fn_test
        (expr): "fn a -> a" =>
            Expr::fn_expr(
                Match::ident("a"),
                Expr::variable("a"));
        (expr): "fn a -> a + 1" =>
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
        (expr): "a (1, 2)" =>
            Expr::fn_app(
                Expr::variable("a"),
                Expr::join(
                    Expr::int(1),
                    Expr::int(2)
                )
            )
    }
}