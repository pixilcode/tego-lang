use assert_cmd::prelude::*;
use std::fs;
use std::path;
use std::process::Command;

#[test]
#[ignore]
fn repl_test() -> Result<(), Box<dyn std::error::Error>> {
	let mut cmd = Command::cargo_bin("tego-lang")?;
	cmd.arg("repl");
	unimplemented!()
}

#[test]
fn example_tests() -> Result<(), Box<dyn std::error::Error>> {
	std::env::set_current_dir(path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples"))?;
	fs::read_dir(".")?
		.filter(|entry| {
			entry
				.as_ref()
				.map(|entry| {
					entry
						.file_name()
						.into_string()
						.unwrap_or("".into())
						.ends_with(".out")
				})
				.unwrap_or(false)
		})
		.map(|entry| {
			entry
				.as_ref()
				.map(|entry| {
					let outfile: String = entry.file_name().to_string_lossy().into();
					let codefile: String =
						entry.file_name().to_string_lossy().replace(".out", ".tgo");
					(outfile, codefile)
				})
				.unwrap()
		})
		.for_each(|(outfile, codefile)| {
			let mut cmd = Command::cargo_bin("tego-lang").unwrap();
			let output = cmd.arg("run").arg(codefile).output().unwrap();
			let expected = fs::read_to_string(outfile).unwrap();
			assert_eq!(String::from_utf8(output.stdout).unwrap(), expected);
		});
	Ok(())
}
