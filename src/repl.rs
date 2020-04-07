use std::io::{self, Write};
use tego_lang::parser::expr;
use crate::interpreter;
use nom::combinator::all_consuming;

pub fn run() {
    println!("Welcome to Tego!");
    println!();
    
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        
        let mut code = String::new();
        io::stdin().read_line(&mut code).unwrap();
        
        let code = code.trim();
        
        if code == ":quit"|| code == ":q" {
            break;
        }
        
        let result = match all_consuming(expr::expr)(&code) {
            Ok((_, e)) => interpreter::eval_expr(e),
            Err(error) => {
                println!("{:?}", error);
                continue;
            }
        };
        
        println!("{} : {}", result, result.type_());
    }
}