use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
mod codegen;
mod lexer;
mod parser;

#[derive(Default, clap::Parser, Debug)]
struct Arguments {
    #[clap(long, short, action)]
    code: bool,
    input: String,
}

fn read_file_to_string(file_path: &str) -> Result<String, io::Error> {
    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input,
        false => read_file_to_string(args.input.as_str())?,
    };
    // let file_contents = String::from("= 1 + 212.3\n12322 \"adcfdsf\"");
    parser::parse(file_contents);
    codegen::codegen();

    Ok(())
}
