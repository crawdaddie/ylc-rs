extern crate inkwell;
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use clap::Parser;
use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::OptimizationLevel;

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

// extern "C" {
//     fn printf(format: *const i8, ...) -> i32;
// }
// #[no_mangle]
// pub unsafe extern "C" fn printf(fmt: *const i8) -> i32 {
//     unsafe {
//         println!("{:?}", fmt.as_ref());
//     }
//     1
// }
//
//
// macro used to print & flush without printing a new line
// macro_rules! print_flush {
//     ( $( $x:expr ),* ) => {
//         print!( $($x, )* );
//
//         std::io::stdout().flush().expect("Could not flush to standard output.");
//     };
// }
// #[no_mangle]
// pub extern "C" fn putchard(x: f64) -> f64 {
//     print_flush!("{}", x as u8 as char);
//     x
// }

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input.unwrap(),
        false if args.interactive => "".into(),
        false => read_file_to_string(args.input.unwrap().as_str())?,
    };

    let context = Context::create();
    let module = context.create_module("ylc");
    let builder = context.create_builder();
    // let execution_engine = module
    //     .create_jit_execution_engine(OptimizationLevel::None)
    //     .unwrap();

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

    if let Ok(main_fn) = Compiler::compile(&context, &builder, &fpm, &module, &program) {
        module.print_to_stderr();
        let ee = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let name = main_fn.get_name().to_str().unwrap().to_string();

        let final_type = main_fn.get_type().get_return_type();
        match final_type {
            // TODO: try to match the real llvm return type of the fn
            // NB: compile_program sets this as a void_type by default for now
            Some(BasicTypeEnum::IntType(_)) => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> i64>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },
            Some(BasicTypeEnum::FloatType(_)) => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },

            // Some(BasicTypeEnum::IntType()) => unsafe {
            //     let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> bool>(name.as_str());
            //     println!("=> {:?}", compiled_fn.unwrap().call());
            // },

            // Ttype::Void => unsafe {
            //     let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> bool>(name.as_str());
            //     println!("=> {:?}", compiled_fn.unwrap().call());
            // },
            _ => unsafe {
                let compiled_fn = ee.get_function::<unsafe extern "C" fn() -> i64>(name.as_str());
                println!("=> {:?}", compiled_fn.unwrap().call());
            },
        }
        // unsafe { ee.get_function::<unsafe extern "C" fn() -> i64>(name.as_str()) };
    }

    // if codegen_program(program, &mut ctx).is_ok() {
    //     ctx.module.print_to_stderr();
    //     if let Some(main_fn) = ctx.get_function::<MainFunc>("main") {
    //         unsafe {
    //             main_fn.call();
    //         }
    //     }
    // };

    // println!("top-level env: {:?}", ctx.env.current().unwrap());

    // let ts_ctx = Arc::new(Mutex::new(ctx)); // unfortunately need to wrap
    // in Arc(Mutex(...)) because rust can't prove that the closure won't be passed to another
    // thread

    // if args.interactive {
    //     let _ = repl::repl(|line| {
    //         let mut program = parser::parse(line);
    //
    //         infer_types(&mut program);
    //
    //         println!("\x1b[1;35m");
    //         for s in &program {
    //             // print_ast(s.clone(), 0);
    //             println!("{:?}", s);
    //         }
    //         println!("\x1b[1;0m");
    //
    //         let mut ctx = ts_ctx.lock().unwrap();
    //         if codegen_program(program, &mut ctx).is_ok() {
    //             ctx.module.print_to_stderr();
    //
    //             if let Some(main_fn) = ctx.get_function::<MainFunc>("main") {
    //                 unsafe {
    //                     main_fn.call();
    //                 }
    //             }
    //         }
    //     });
    // }

    Ok(())
}
