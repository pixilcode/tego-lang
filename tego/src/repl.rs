use std::io::{self, Write};
use std::rc::Rc;
use tego_interpreter as interpreter;
use tego_parser as parser;
use tego_parser::ast::Decl;

pub fn run() -> io::Result<()> {
    let mut stdout = io::BufWriter::new(io::stdout());

    writeln!(stdout, "Welcome to")?;
    writeln!(
        stdout,
        "
   /\\
  //\\\\
 //||\\\\  _   __   ___
   ||  ||_| / || // \\\\
   ||  ||_  \\_|| \\\\_//
            \\_||       
"
    )?;
    writeln!(stdout, "Type ':q' or ':quit' to exit\n")?;
    stdout.flush()?;
    repl_loop(Some(interpreter::new_env()), vec![], stdout)
}

fn repl_loop(
    env: Option<interpreter::WrappedEnv>,
    mut decls: Vec<Decl>,
    mut stdout: io::BufWriter<io::Stdout>,
) -> io::Result<()> {
    write!(stdout, ">> ")?;
    stdout.flush()?;
    let mut code = String::new();
    io::stdin().read_line(&mut code).unwrap();
    let code = code.trim();

    if code == ":quit" || code == ":q" {
        return Ok(());
    }
    let (env, decls) = if let Ok((_, d)) = parser::decl(code.into()) {
        decls.push(d);
        (None, decls)
    } else {
        match parser::complete(parser::expr)(code.into()) {
            Ok((_, e)) => {
                let env = env.unwrap_or_else(|| interpreter::env_from_decls(&decls));
                let result = interpreter::eval_expr(e, &Rc::clone(&env));
                writeln!(stdout, "{} : {}", result, result.type_())?;
                (Some(env), decls)
            }
            Err(error) => {
                parser::ParseError::from(error).verbose_from_source(code, &mut stdout)?;
                (env, decls)
            }
        }
    };
    stdout.flush()?;
    repl_loop(env, decls, stdout)
}
