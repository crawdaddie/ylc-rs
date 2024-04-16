extern crate inkwell;
extern crate ylc;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use ylc::codegen::Compiler;

use inkwell::OptimizationLevel;
use ylc::parser::Program;

use std::error::Error;

use std::io::{self};
use ylc::codegen::MainFunc;

// use llvm_sys::linker::LLVMLinkModules2;
use rustyline::DefaultEditor;
fn compile_interactive<'ctx>(
    program: &Program,
    context: &'ctx Context,
    main_name: String,
) -> Result<Module<'ctx>, Box<dyn Error>> {
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
    let _main_fn = Compiler::compile(&context, &builder, &fpm, &module, program, Some(main_name))?;
    module.print_to_stderr();

    Ok(module)
}
fn main() -> Result<(), io::Error> {
    let context = Context::create();

    // let ts_ctx = Arc::new(Mutex::new(context));
    let mut prev_module: Option<Module> = None;
    let mut rl = DefaultEditor::new().unwrap();
    let mut iter = 0;
    loop {
        let readline = rl.readline("\x1b[1;31mλ \x1b[1;0m");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                let mut program = ylc::parser::parse(line);
                ylc::typecheck::infer_types(&mut program);

                let main_name = format!("main.{}", iter);
                match compile_interactive(&program, &context, main_name.clone()) {
                    Ok(module) => {
                        // module.print_to_stderr();
                        // if let Some(prev) = &prev_module {
                        //     unsafe {
                        //         let _ = LLVMLinkModules2(module.as_mut_ptr(), prev.as_mut_ptr());
                        //     }
                        // }

                        // Update prev_module with the current module
                        let ee = module
                            .create_jit_execution_engine(OptimizationLevel::None)
                            .unwrap();
                        unsafe {
                            let main = ee
                                .get_function::<MainFunc>(main_name.as_str())
                                .ok()
                                .unwrap();

                            println!("{}", main.call());
                        }
                        prev_module = Some(module);
                        iter = iter + 1;
                    }
                    _ => panic!("repl failiyre"),
                }
            }
            _ => panic!("repl failure"),
        }
    }
}
