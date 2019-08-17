extern crate jest_lang;

mod interpreter;
mod repl;

fn main() {
    repl::run();
}