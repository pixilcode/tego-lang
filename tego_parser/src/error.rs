pub mod parser;
pub mod scanner;

use crate::Input;

/// If there is an error, ignore anything that was parsed by the
/// parser and just return the original input
pub fn err_retain_all<'a, F, O>(parser: F) -> impl Fn(Input<'a>) -> nom::IResult<Input<'a>, O>
where
    F: Fn(Input<'a>) -> nom::IResult<Input<'a>, O>,
{
    move |input| parser(input).map_err(|error| match error {
		nom::Err::Error((_, error)) => nom::Err::Error((input, error)),
		nom::Err::Failure((_, error)) => nom::Err::Failure((input, error)),
		error => error
	})
}