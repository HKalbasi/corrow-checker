use std::eprintln;

use clap::Parser;
use xshell::{cmd, Shell};

fn run_on_all_tests(sh: Shell, job: impl Fn(&str, String)) -> anyhow::Result<()> {
    sh.change_dir("tests");
    let tests = cmd!(sh, "ls").read()?;
    for test in tests.lines() {
        let Some(test) = test.strip_suffix(".c") else {
            continue;
        };
        let output = cmd!(sh, "../target/release/corrow-checker {test}.c").read_stderr()?;
        let output = strip_ansi_escapes::strip_str(output);
        job(test, output);
    }
    Ok(())
}

#[derive(Parser)]
enum TestCommand {
    Check,
    Regenerate,
}

#[derive(Parser)]
enum Command {
    #[clap(subcommand)]
    Test(TestCommand),
}

fn main() -> anyhow::Result<()> {
    let command = Command::parse();
    let sh = Shell::new()?;
    match command {
        Command::Test(command) => {
            cmd!(sh, "cargo build --release").run()?;
            match command {
                TestCommand::Check => run_on_all_tests(sh, |name, text| {
                    let Ok(expected) = std::fs::read_to_string(format!("tests/{name}.stderr"))
                    else {
                        panic!("File {name}.stderr does not exist. Run `cargo xtask test regenerate` to generate it");
                    };
                    if text != expected {
                        eprintln!("Expected:\n{expected}");
                        eprintln!("Actual:\n{text}");
                        panic!("Output for test {name} differs from expectation");
                    }
                })?,
                TestCommand::Regenerate => run_on_all_tests(sh, |name, text| {
                    std::fs::write(format!("tests/{name}.stderr"), text).unwrap();
                })?,
            }
        }
    }
    Ok(())
}
