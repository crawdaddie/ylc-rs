extern crate inkwell;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
use codegen::Compiler;
// use dylib::DynamicLibrary;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::BasicTypeEnum;
use inkwell::OptimizationLevel;
use parser::Program;
mod codegen;
mod lexer;
mod parser;
mod symbols;
mod typecheck;
use std::error::Error;
use std::process::Command;

// mod repl;
// use repl::repl;

#[derive(Default, clap::Parser, Debug)]
struct Arguments {
    #[clap(long, short, action)]
    code: bool,
    #[clap(long, short, action)]
    interactive: bool,

    #[clap(long, short, action)]
    object: bool,

    input: Option<String>,
}

fn read_file_to_string(file_path: &str) -> Result<String, io::Error> {
    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

pub fn write_to_object_file(module: &Module, output_filename: &str) -> Result<(), String> {
    let _ = Target::initialize_native(&InitializationConfig::default());
    let target_triple = TargetMachine::get_default_triple();
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();

    let target = Target::from_triple(&target_triple).map_err(|e| format!("{:?}", e))?;

    let target_machine = target
        .create_target_machine(
            &target_triple,
            &cpu,
            &features,
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or_else(|| "Unable to create target machine!".to_string())?;

    target_machine
        .write_to_file(module, FileType::Object, output_filename.as_ref())
        .map_err(|e| format!("{:?}", e))
}
pub fn link(obj_file: &str, exe_name: &str) {
    let _ = Command::new("clang")
        .arg("-o")
        .arg(exe_name)
        .arg(obj_file)
        .output()
        .expect("Failed to execute command");
}

fn compile_program(program: &Program) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("ylc");
    // let ee = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let builder = context.create_builder();
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
    let _main_fn = Compiler::compile(&context, &builder, &fpm, &module, program)?;
    module.print_to_stderr();
    write_to_object_file(&module, "./object")?;
    link("./object", "exe");
    Ok(())
}

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input.unwrap(),
        false => read_file_to_string(args.input.unwrap().as_str())?,
    };
    let mut program = parser::parse(file_contents);
    typecheck::infer_types(&mut program);

    println!("\x1b[1;35mAST\n---------");
    for s in &program {
        println!("{:?}", s);
    }
    println!("\x1b[1;0m");

    let _ = compile_program(&program);

    Ok(())
}
