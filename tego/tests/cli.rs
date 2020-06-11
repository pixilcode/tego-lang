use assert_cmd::prelude::*;
use std::fs;
use std::path;
use std::process::Command;

#[test]
fn example_tests() -> Result<(), Box<dyn std::error::Error>> {
    test_directory("examples")
}

#[test]
fn feature_tests() -> Result<(), Box<dyn std::error::Error>> {
    test_directory("feature-tests")
}

fn test_directory<P: AsRef<path::Path>>(path: P) -> Result<(), Box<dyn std::error::Error>> {
    std::env::set_current_dir(path::Path::new(env!("CARGO_MANIFEST_DIR")))?;
    fs::read_dir(&path)?
        .filter(|entry| {
            entry
                .as_ref()
                .map(|entry| {
                    entry
                        .file_name()
                        .into_string()
                        .unwrap_or_else(|_| "".into())
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
                .map(|(outfile, codefile)| {
                    let outfile = path.as_ref().join(outfile);
                    let codefile = path.as_ref().join(codefile);
                    (outfile, codefile)
                })
                .unwrap()
        })
        .for_each(|(outfile, codefile)| {
            print!("{:?} ... ", codefile);
            let mut cmd = Command::cargo_bin("tego").unwrap();
            let output = cmd.arg("run").arg(codefile).output().unwrap();
            let expected = fs::read_to_string(outfile).unwrap();
            assert_eq!(String::from_utf8(output.stdout).unwrap(), expected);
            println!("ok");
        });
    Ok(())
}