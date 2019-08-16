use crate::ast::{Expr, BinaryOp};
use crate::parser::tokens::*;

use nom::{
    IResult,
    sequence::pair,
    combinator::opt,
    branch::alt,
    character::complete::digit1,
};

pub fn expr(input: &'_ str) -> IResult<&'_ str, Expr> {
    or_expr(input)
}

fn or_expr(input: &'_ str) -> IResult<&'_ str, Expr> {
    match pair(
        literal,
        opt(
            pair(or, literal)
        )
    )(input) {
        Ok((input, (a, Some((_, b))))) =>
            Ok((input, Expr::binary(a, BinaryOp::Or, b))),
        Ok((input, (a, None))) => Ok((input, a)),
        Err(error) => Err(error)
    }
}

fn literal(input: &'_ str) -> IResult<&'_ str, Expr> {
    match alt((true_val, false_val, unit, digit1))(input) {
        Ok((input, "true")) => Ok((input, Expr::bool(true))),
        Ok((input, "false")) => Ok((input, Expr::bool(false))),
        Ok((input, "unit")) => Ok((input, Expr::unit())),
        Ok((input, lexeme)) => {
            if let Ok(i) = lexeme.parse::<i32>() {
                Ok((input, Expr::int(1)))
            } else {
                Ok((
                    input,
                    Expr::error(&format!(
                        "Couldn't parse lexeme: {}",
                        lexeme
                    ))
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
}