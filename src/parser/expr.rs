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
    ($name:ident, $op_func:expr, $next_precedence:ident; $op_capture:ident => $op_type:expr) => {
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
                        |a, ($op_capture, b)| Expr::binary(a, $op_type, b)))),
                Ok((input, (a, None))) => Ok((input, a)),
                Err(error) => Err(error)
            }
        }
    };
}

pub fn expr(input: &'_ str) -> IResult<&'_ str, Expr> {
    or_expr(input)
}

binary_expr!(or_expr, or, and_expr; _op => BinaryOp::Or);
binary_expr!(and_expr, and, add_expr; _op => BinaryOp::And);
binary_expr!(add_expr, alt((plus, minus)), literal;
    op => match op {
        "+" => BinaryOp::Plus,
        "-" => BinaryOp::Minus,
        _ => panic!("Matched an invalid operator: {}", op)
});

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
            Expr::binary(
                Expr::bool(true),
                BinaryOp::Or,
                Expr::bool(false)
            )
    }
    
    parser_test! {
        and_test
        (and_expr): "true and false" =>
            Expr::binary(
                Expr::bool(true),
                BinaryOp::And,
                Expr::bool(false)
            )
    }
    
    parser_test! {
        plus_test
        (add_expr): "1 + 2" =>
            Expr::binary(
                Expr::int(1),
                BinaryOp::Plus,
                Expr::int(2)
            )
    }
    
    parser_test! {
        minus_test
        (add_expr): "1 - 2" =>
            Expr::binary(
                Expr::int(1),
                BinaryOp::Minus,
                Expr::int(2)
            )
    }
    
    parser_test! {
        precedence_test
        (expr): "1 or 2 and 3 + 4 - 5" =>
            Expr::binary(Expr::int(1),
                BinaryOp::Or,
                Expr::binary(Expr::int(2),
                    BinaryOp::And,
                    Expr::binary(Expr::binary(
                        Expr::int(3),
                        BinaryOp::Plus,
                        Expr::int(4)
                    ),
                    BinaryOp::Minus,
                    Expr::int(5))));
        (expr): "1 and 2 and 3" =>
            Expr::binary(Expr::binary(Expr::int(1), BinaryOp::And, Expr::int(2)), BinaryOp::And, Expr::int(3))
    }
}