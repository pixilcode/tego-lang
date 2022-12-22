use std::io::{self, Write};
use tego_interpreter as interpreter;
use tego_parser as parser;
use tego_parser::ast::Decl;

pub fn run() -> io::Result<()> {
    let mut stdout = io::BufWriter::new(io::stdout());
    let stderr = io::BufWriter::new(io::stderr());

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
    repl_loop(
        Some(interpreter::import_prelude(&interpreter::new_env())),
        vec![],
        stdout,
        stderr,
    )
}

fn repl_loop(
    env: Option<interpreter::WrappedEnv>,
    mut decls: Vec<Decl>,
    mut stdout: io::BufWriter<io::Stdout>,
    mut stderr: io::BufWriter<io::Stderr>
) -> io::Result<()> {
    write!(stdout, ">> ")?;
    stdout.flush()?;
    let mut code = String::new();
    io::stdin().read_line(&mut code).unwrap();
    let code = code.trim();

    if code == ":quit" || code == ":q" {
        return Ok(());
    }


    let (env, decls) = match parser::decl(code.into()) {
        Ok(decl) => {
            decls.push(decl);
            (None, decls)
        },
        Err(error) if !error.is_no_match() => {
            error.verbose_from_source(code, &mut stderr)?;
            (env, decls)
        },
        Err(_) => {
            match parser::expr(code.into()) {
                Ok(expr) => {
                    let env = env.unwrap_or_else(|| {
                        let decl_env = interpreter::env_from_decls(&decls);
                        interpreter::import_prelude(&decl_env)
                    });
                    let result = interpreter::eval_expr(expr, &env);
                    if result.is_error() {
                        writeln!(stdout, "{}", result)?;
                    } else if result.run().is_err() {
                        writeln!(stdout, "{} : {}", result, result.type_())?;
                    } else {
                        // Command was run
                    }
                    (Some(env), decls)
                }
                Err(error) => {
                    error.verbose_from_source(code, &mut stderr)?;
                    (env, decls)
                }
            }
        }
    };
    stdout.flush()?;
    stderr.flush()?;
    repl_loop(env, decls, stdout, stderr)
}
