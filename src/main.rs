use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
mod lexer;
use lexer::{Lexer, Token};

fn read_file_to_string(file_path: &str) -> Result<String, io::Error> {
    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args: Vec<String> = env::args().collect();

    // Check that the user provided an argument for the file path
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    // let file_contents = read_file_to_string(file_path)?;
    let file_contents = String::from("= 1 + 212.3");

    let mut l = Lexer::new(file_contents);

    loop {
        let token = l.scan_token();
        if token != Token::Eof {
            println!("{:?}", token);
        } else {
            break;
        }
    }

    Ok(())
}
