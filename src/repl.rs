use std::io::{self, Write};
use tego_lang::parser::{expr, decl};
use tego_lang::Decl;
use tego_lang::execute::interpreter::{self, new_env};
use nom::combinator::all_consuming;
use std::rc::Rc;

pub fn run() {
    println!("Welcome to");
    println!(
"
   /\\
  //\\\\
 //||\\\\  _   __   ___
   ||  ||_| / || // \\\\
   ||  ||_  \\_|| \\\\_//
            \\_||       
");
    
    println!("Type ':q' or ':quit' to exit\n");
    repl_loop(Some(new_env()), vec![]);
}

fn repl_loop(env: Option<interpreter::WrappedEnv>, mut decls: Vec<Decl>) {
    print!(">> ");
    io::stdout().flush().unwrap();
    
    let mut code = String::new();
    io::stdin().read_line(&mut code).unwrap();
    
    let code = code.trim();
    
    if code == ":quit"|| code == ":q" {
        return;
    }
    
    let (env, decls) = if let Ok((_, d)) = decl::decl(&code) {
        decls.push(d);
        (None, decls)
    } else {
        match all_consuming(expr::expr)(&code) {
            Ok((_, e)) => {
                let env = env.unwrap_or_else(|| interpreter::env_from_decls(&decls));
                let result = interpreter::eval_expr(e, &Rc::clone(&env));
                println!("{} : {}", result, result.type_());
                (Some(env), decls)
            },
            Err(error) => {
                println!("{:?}", error);
                (env, decls)
            }
        }
    };
    
    repl_loop(env, decls);
}