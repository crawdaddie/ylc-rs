extern crate inkwell;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
use codegen::Compiler;
use dylib::DynamicLibrary;
use inkwell::context::{AsContextRef, Context};
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::OptimizationLevel;
use llvm_sys::core::{LLVMContextDispose, LLVMDisposeModule};
use llvm_sys::execution_engine::LLVMDisposeExecutionEngine;
use parser::Program;
mod codegen;
mod lexer;
mod parser;
mod symbols;
mod typecheck;
use std::error::Error;

// mod repl;
// use repl::repl;

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

fn compile_program(program: &Program) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("ylc");
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
    let main_fn = Compiler::compile(&context, &builder, &fpm, &module, program)?;
    module.print_to_stderr();

    let ee = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let name = main_fn.get_name().to_str().unwrap().to_string();
    let ret_type = main_fn.get_type().get_return_type();

    match ret_type {
        Some(BasicTypeEnum::IntType(i)) if i.get_bit_width() == 1 => unsafe {
            let compiled_fn = ee
                .get_function::<unsafe extern "C" fn() -> bool>(name.as_str())
                .unwrap();
            println!("=> {:?}", compiled_fn.call());
            // println!("{:?}", compiled_fn);
        },
        Some(BasicTypeEnum::IntType(i)) => unsafe {
            let compiled_fn = ee
                .get_function::<unsafe extern "C" fn() -> i64>(name.as_str())
                .unwrap();
            println!("=> {:?}", compiled_fn.call());

            // println!("{:?}", compiled_fn);
        },

        Some(BasicTypeEnum::FloatType(_f)) => unsafe {
            let compiled_fn = ee
                .get_function::<unsafe extern "C" fn() -> f64>(name.as_str())
                .unwrap();
            println!("=> {:?}", compiled_fn.call());
        },
        Some(BasicTypeEnum::StructType(_)) => unsafe {
            let compiled_fn = ee
                .get_function::<unsafe extern "C" fn() -> (i64, i64, i64)>(name.as_str())
                .unwrap();
            println!("=> {:?}", compiled_fn.call());
        },
        _ => {}
    }

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

    let lib_path = Path::new("/Users/adam/projects/langs/ylc/libs/libyalce_synth.so");
    println!("lib path: {:?}", lib_path);
    let x = DynamicLibrary::open(Some(lib_path)).unwrap();

    let _ = compile_program(&program);
    // let _ = repl(|line| {
    //     println!("Line {}", line);
    // });

    Ok(())
}
