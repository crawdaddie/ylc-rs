use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::sync::{Arc, Mutex};

use clap::Parser;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::OptimizationLevel;
use symbols::Symbol;

use crate::codegen::Compiler;
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

    input: String,
}

fn read_file_to_string(file_path: &str) -> Result<String, io::Error> {
    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

pub struct CodegenCtx<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    env: symbols::Env<Symbol>,
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn get_function<T>(&self, fn_name: &str) -> Option<JitFunction<'ctx, T>>
    where
        T: UnsafeFunctionPointer,
    {
        unsafe { self.execution_engine.get_function::<T>(fn_name).ok() }
    }
}

/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type MainFunc = unsafe extern "C" fn() -> ();

fn main() -> Result<(), io::Error> {
    // Parse command-line arguments
    let args = Arguments::parse();

    let file_contents = match args.code {
        true => args.input,
        false => read_file_to_string(args.input.as_str())?,
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
    // let mut ctx = CodegenCtx {
    //     context: &context,
    //     module,
    //     builder: context.create_builder(),
    //     execution_engine,
    //     env: symbols::Env::<Symbol>::new(),
    // };

    let mut program = parser::parse(file_contents);
    infer_types(&mut program);

    println!("\x1b[1;35m");
    for s in &program {
        // print_ast(s.clone(), 0);
        println!("{:?}", s);
    }
    println!("\x1b[1;0m");

    // ctx.env.push();
    Compiler::compile(&context, &builder, &fpm, &module, &program);

    // if codegen_program(program, &mut ctx).is_ok() {
    //     ctx.module.print_to_stderr();
    //     if let Some(main_fn) = ctx.get_function::<MainFunc>("main") {
    //         unsafe {
    //             main_fn.call();
    //         }
    //     }
    // };

    println!("top-level env: {:?}", ctx.env.current().unwrap());

    let ts_ctx = Arc::new(Mutex::new(ctx)); // unfortunately need to wrap
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
