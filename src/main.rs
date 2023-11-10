extern crate inkwell;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
use inkwell::context::Context;
use inkwell::passes::PassManager;
mod lexer;
mod parser;
mod symbols;
mod typecheck;

#[derive(Default, clap::Parser, Debug)]
struct Arguments {
    #[clap(long, short, action)]
    code: bool,
    #[clap(long, short, action)]
    interactive: bool,

    input: Option<String>,
}

fn read_file_to_string(file_path: &str) -> Result<String, io::Error> {
    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

#[no_mangle]
pub extern "C" fn ex(x: f64) -> f64 {
    println!("hello extern {}", x);
    x
}
#[used]
static EX: extern "C" fn(f64) -> f64 = ex;

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input.unwrap(),
        false => read_file_to_string(args.input.unwrap().as_str())?,
    };

    let context = Context::create();
    let module = context.create_module("ylc");
    let _builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let program = parser::parse(file_contents);
    // println!("program {:?}", program);
    // typecheck::infer_types(&mut program);

    println!("\x1b[1;35m");
    for s in &program {
        println!("{:?}", s);
    }
    println!("\x1b[1;0m");

    Ok(())
}
