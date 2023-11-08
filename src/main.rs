extern crate inkwell;
use std::ffi::{c_char, CStr};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::FunctionValue;
use inkwell::OptimizationLevel;
use parser::Program;

use crate::codegen::Compiler;
use crate::symbols::{Numeric, Ttype};
use crate::typecheck::infer_types;

mod codegen;
mod lexer;
mod parser;
mod repl;
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

#[no_mangle]
pub unsafe extern "C" fn printf(fmt: *const c_char) -> i64 {
    println!("--{:?}", fmt);
    // unsafe {
    //     let s = CStr::from_ptr(fmt).to_str();
    //     println!("{:?}", s);
    // }
    1
}
#[used]
static PRINTF: unsafe extern "C" fn(*const c_char) -> i64 = printf;
fn compile<'ctx, 'a>(
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    fpm: &'a PassManager<FunctionValue<'ctx>>,
    program: &Program,
) {
    if let Ok(main_fn) = Compiler::compile(&context, &builder, &fpm, &module, &program) {
        module.print_to_stderr();
        let ee = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let name = main_fn.get_name().to_str().unwrap().to_string();

        // let final_type = main_fn.get_type().get_return_type();
        let final_type = program.last().unwrap().get_ttype();
        match final_type {
            Some(Ttype::Numeric(Numeric::Int)) => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> i64>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },
            Some(Ttype::Numeric(Numeric::Num)) => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },

            Some(Ttype::Bool) => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> bool>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },

            Some(Ttype::Str) => unsafe {
                let compiled_fn =
                    ee.get_function::<unsafe extern "C" fn() -> Vec<i8>>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },

            // Ttype::Void => unsafe {
            //     let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> bool>(name.as_str());
            //     println!("=> {:?}", compiled_fn.unwrap().call());
            // },
            _ => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> bool>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },
        }
        // unsafe { ee.get_function::<unsafe extern "C" fn() -> i64>(name.as_str()) };
    }
}
fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input.unwrap(),
        false => read_file_to_string(args.input.unwrap().as_str())?,
    };

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

    let mut program = parser::parse(file_contents);
    infer_types(&mut program);

    println!("\x1b[1;35m");
    for s in &program {
        // print_ast(s.clone(), 0);
        println!("{:?}", s);
    }
    println!("\x1b[1;0m");
    compile(&context, &builder, &module, &fpm, &program);

    if args.interactive {
        // println!("interactive");

        let _ = repl::repl(|line| {
            let mut program = parser::parse(line);
            infer_types(&mut program);

            println!("\x1b[1;35m");
            for s in &program {
                // print_ast(s.clone(), 0);
                println!("{:?}", s);
            }
            println!("\x1b[1;0m");
            // compile(&context, &builder, &module, &fpm, &program);
        });
    }

    Ok(())
}
