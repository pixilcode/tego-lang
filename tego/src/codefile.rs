use std::fs;
use std::io::{self, Write};
use std::path::Path;
use tego_interpreter as interpreter;
use tego_parser as parser;

pub fn run<P: AsRef<Path>>(path: P) -> io::Result<()> {
    let mut stdout = io::BufWriter::new(io::stdout());
    let mut stderr = io::BufWriter::new(io::stderr());
    let file = match open_file(path) {
        Ok(f) => f,
        Err(e) => {
            writeln!(stdout, "Error reading file: {}", e)?;
            return wrap_up(stderr, stdout);
        }
    };
    let program = match parser::prog(file.as_str().into()) {
        Ok((_, prog)) => prog,
        Err(err) => {
            println!("{:?}", err);
            todo!(); //parser::ParseError::from(err).verbose_from_source(&file, &mut stderr)?;
            return wrap_up(stderr, stdout);
        }
    };
    let result = match interpreter::run_prog(program) {
        Ok(r) => r,
        Err(e) => {
            writeln!(stderr, "Error running file: {}", e)?;
            return wrap_up(stderr, stdout);
        }
    };
    if result.is_error() {
        writeln!(stderr, "Error running file: {}", result)?;
    } else if result.run().is_err() {
        writeln!(stdout, "{}", result)?;
    } else {
        // Command was run
    }

    wrap_up(stderr, stdout)
}

fn open_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    fs::read_to_string(path)
}

fn wrap_up(
    mut stderr: io::BufWriter<io::Stderr>,
    mut stdout: io::BufWriter<io::Stdout>,
) -> io::Result<()> {
    stderr.flush()?;
    stdout.flush()?;
    Ok(())
}
