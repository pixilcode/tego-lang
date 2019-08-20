use crate::ast::{Expr, BinaryOp};
use crate::parser::tokens::*;

use nom::{
    IResult,
    sequence::pair,
    combinator::opt,
    branch::alt,
    character::complete::digit1,
    multi::many1
};

macro_rules! binary_expr {
    ($name:ident, $op_func:expr, $next_precedence:ident) => {
        fn $name(input: &'_ str) -> IResult<&'_ str, Expr> {
            match pair(
                $next_precedence,
                opt(
                    many1(pair($op_func, $next_precedence))
                )
            )(input) {
                Ok((input, (a, Some(others)))) =>
                    Ok((input, others.into_iter().fold(
                        a,
                        |a, (op, b)| Expr::binary(a, BinaryOp::from(op), b)))),
                Ok((input, (a, None))) => Ok((input, a)),
                Err(error) => Err(error)
            }
        }
    };
}

pub fn expr(input: &'_ str) -> IResult<&'_ str, Expr> {
    join_expr(input)
}

// Is or, xor, and in the incorrect order,
// Or is add, mult in the incorrect order?
// See Parser.java (Lox)

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
binary_expr!(mult_expr, alt((star, slash, modulo)), literal);

fn literal(input: &'_ str) -> IResult<&'_ str, Expr> {
    match token(alt((true_val, false_val, unit, digit1)))(input) {
        Ok((input, "true")) => Ok((input, Expr::bool(true))),
        Ok((input, "false")) => Ok((input, Expr::bool(false))),
        Ok((input, "unit")) => Ok((input, Expr::unit())),
        Ok((input, lexeme)) => {
            if let Ok(i) = lexeme.parse::<i32>() {
                Ok((input, Expr::int(i)))
            } else {
                Ok((
                    input,
                    Expr::error(&format!(
                        "Couldn't parse lexeme: {}",
                        lexeme
                    )) // TODO Return a nom error here instead
                ))
            }
        },
        Err(error) => Err(error)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    parser_test! {
        literal_test
        (literal): "1" => Expr::int(1);
        (literal): "true" => Expr::bool(true);
        (literal): "false" => Expr::bool(false);
        (literal): "unit" => Expr::unit()
    }
    
    parser_test! {
        or_test
        (or_expr): "true or false" =>
            Expr::or(
                Expr::bool(true),
                Expr::bool(false))
    }
    
    parser_test! {
        and_test
        (and_expr): "true and false" =>
            Expr::and(
                Expr::bool(true),
                Expr::bool(false))
    }
    
    parser_test! {
        plus_test
        (add_expr): "1 + 2" =>
            Expr::plus(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        minus_test
        (add_expr): "1 - 2" =>
            Expr::minus(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        multiply_test
        (mult_expr): "1 * 2" =>
            Expr::multiply(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        divide_test
        (mult_expr): "1 / 2" =>
            Expr::divide(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        modulo_test
        (mult_expr): "1 % 2" =>
            Expr::modulo(
                Expr::int(1),
                Expr::int(2))
    }
    
    parser_test! {
        equal_test
        (equal_expr): "1 == 2" =>
            Expr::equal(
                Expr::int(1),
                Expr::int(2))
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
            Expr::not_equal(
                Expr::equal(
                    Expr::int(1),
                    Expr::int(2)),
                Expr::greater_than_equal(
                    Expr::less_than_equal(
                        Expr::greater_than(
                            Expr::less_than(
                                Expr::int(3),
                                Expr::int(4)),
                            Expr::int(5)),
                        Expr::int(6)),
                    Expr::and(
                        Expr::int(7),
                        Expr::int(8))))
    }
}