extern crate inkwell;
extern crate ylc;

use clap::Parser;
use ylc::codegen::Compiler;
use ylc::util::read_file_to_string;
// use dylib::DynamicLibrary;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

use inkwell::OptimizationLevel;
use ylc::parser::Program;

use std::error::Error;
use std::process::Command;

use std::path::Path;

use std::io::{self};

// use llvm_sys::linker::LLVMLinkModules2;
use rustyline::DefaultEditor;

// mod repl;
// use repl::repl;

#[derive(Default, clap::Parser, Debug)]
struct Arguments {
    #[clap(long, short, action)]
    stdin: bool,

    #[clap(long, short, action)]
    compile: bool,

    #[clap(long, short, action)]
    interactive: bool,

    #[clap(long, short, action)]
    object: bool,

    input: Option<String>,
}

fn transform_filepath(filepath: String) -> String {
    let basename = Path::new(&filepath).file_stem().unwrap().to_str().unwrap();
    let transformed = basename.replace(".ylc", ".exe");
    format!("build/{}", transformed)
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
        .map_err(|e| {
            println!("error writing object file {}", e);
            format!("{:?}", e)
        })
}
pub fn link(obj_file: &str, exe_name: &str) {
    let _ = Command::new("clang")
        .arg("-o")
        .arg(exe_name)
        .arg(obj_file)
        .output()
        .expect("Failed to execute command");
}

fn compile_program(program: &Program, input_filename: String) -> Result<(), Box<dyn Error>> {
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
    let _main_fn = Compiler::compile(&context, &builder, &fpm, &module, program, None)?;
    module.print_to_stderr();

    write_to_object_file(&module, "./object")?;
    link("./object", &transform_filepath(input_filename));
    Ok(())
}

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
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type MainFunc = unsafe extern "C" fn() -> i32;

fn repl() -> Result<(), io::Error> {
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

fn compile_input_file(input: Option<String>) -> Result<(), io::Error> {
    let (input_filename, input_content) = match input {
        Some(s) => (s.clone(), read_file_to_string(s.as_str())?),
        _ => panic!("no input filename"),
    };

    let mut program = ylc::parser::parse(input_content);
    ylc::typecheck::infer_types(&mut program);

    println!("\x1b[1;35mAST\n---------");
    for s in &program {
        let json = serde_json::to_string_pretty(&s).unwrap();
        println!("{}", json);
    }
    println!("\x1b[1;0m");

    let _ = compile_program(&program, input_filename);

    Ok(())
}

fn main() -> Result<(), io::Error> {
    let args = Arguments::parse();
    match args.interactive {
        true => repl(),
        false => compile_input_file(args.input),
    }
}
