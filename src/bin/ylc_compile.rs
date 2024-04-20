extern crate inkwell;
extern crate ylc;

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
pub fn link(obj_file: &str, exe_name: &str, linker_flags: Vec<String>) {
    let mut command = Command::new("clang");

    // Add linker flags
    for flag in linker_flags {
        command.arg(flag);
    }

    // Add other arguments
    let _ = command
        .arg("-o")
        .arg(exe_name)
        .arg(obj_file)
        .output()
        .expect("Failed to execute command");
}

fn compile_program(
    program: &Program,
    input_filename: String,
    linker_flags: Vec<String>,
) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("ylc");
    // let ee = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let builder = context.create_builder();
    // Create FPM
    let fpm = PassManager::create(&module);

    // fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    // fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();
    let _main_fn = Compiler::compile(&context, &builder, &fpm, &module, program, None)?;
    module.print_to_stderr();

    write_to_object_file(&module, "./object")?;
    link(
        "./object",
        &transform_filepath(input_filename),
        linker_flags,
    );
    Ok(())
}

fn compile_input_file(input: Option<&String>, linker_flags: Vec<String>) -> Result<(), io::Error> {
    let (input_filename, input_content) = match input {
        Some(s) => (s.clone(), read_file_to_string(s.as_str())?),
        _ => panic!("no input filename"),
    };

    let mut program = ylc::parser::parse(input_content);
    println!("\x1b[1;35mAST\n---------");
    for s in &program {
        let json = serde_json::to_string_pretty(&s).unwrap();
        println!("{}", json);
    }
    println!("\x1b[1;0m");
    ylc::typecheck::infer_types(&mut program);

    let _ = compile_program(&program, input_filename, linker_flags);

    Ok(())
}

fn main() -> Result<(), io::Error> {
    let mut linker_args = Vec::new();
    let mut other_args = Vec::new();

    for (index, arg) in std::env::args().enumerate() {
        if index == 0 {
            continue; // Skipping the first argument which is the program name
        }

        if arg.starts_with("-L") || arg.starts_with("-l") {
            linker_args.push(arg.clone());
        } else if arg.ends_with(".ylc") {
            other_args.push(arg.clone());
        }
    }

    let input_file = other_args.first();
    let _ = compile_input_file(input_file, linker_args);

    Ok(())
}
