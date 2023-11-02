use std::error::Error;

use crate::parser::Program;
use crate::symbols::{Env, Symbol};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    env: Env<Symbol>,
}
impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        program: &Program,
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        let mut compiler = Self {
            context,
            builder,
            fpm: pass_manager,
            module,
            env: Env::<Symbol>::new(),
        };
        compiler.compile_program(program)
    }
    fn compile_program(
        &mut self,
        program: &Program,
    ) -> Result<FunctionValue<'ctx>, Box<dyn Error>> {
        self.env.push();

        let main_fn =
            self.module
                .add_function("main", self.context.void_type().fn_type(&[], false), None);

        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);

        // for stmt in program {
        //     codegen(&stmt, ctx);
        // }
        self.builder.build_return(None);
        Ok(main_fn)
    }
}
