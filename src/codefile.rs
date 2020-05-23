use std::fs;
use std::io;
use std::path::Path;
use tego_lang::interpreter;
use tego_lang::parser;

pub fn run<P: AsRef<Path>>(path: P) {
	let file = match open_file(path) {
		Ok(f) => f,
		Err(e) => {
			eprintln!("Error reading file: {}", e);
			return;
		}
	};
	let program = match parser::prog(file.as_str().into()) {
		Ok((_, prog)) => prog,
		Err(e) => {
			eprintln!("Error parsing file: {:?}", e);
			return;
		}
	};
	let result = match interpreter::run_prog(program) {
		Ok(r) => r,
		Err(e) => {
			eprintln!("Error running file: {}", e);
			return;
		}
	};
	if result.is_error() {
		eprintln!("Error running file: {}", result);
	} else {
		println!("{}", result);
	}
}

fn open_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
	fs::read_to_string(path)
}
