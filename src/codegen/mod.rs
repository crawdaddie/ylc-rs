use std::error::Error;

use crate::lexer::Token;
use crate::parser::{Ast, Program};
use crate::symbols::{Env, Symbol};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue,
    GenericValue, IntMathValue, PointerValue,
};
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
    ) -> Result<FunctionValue<'ctx>, &'ctx str> {
        let mut compiler = Self {
            context,
            builder,
            fpm: pass_manager,
            module,
            env: Env::<Symbol>::new(),
        };
        compiler.compile_program(program)
    }
    fn compile_program(&mut self, program: &Program) -> Result<FunctionValue<'ctx>, &'ctx str> {
        self.env.push();

        let main_fn =
            self.module
                .add_function("main", self.context.void_type().fn_type(&[], false), None);

        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);

        for stmt in program {
            let _ = self.codegen(&stmt);
        }
        self.builder.build_return(None);
        Ok(main_fn)
    }

    fn codegen(&mut self, expr: &Ast) -> Option<AnyValueEnum<'ctx>> {
        match expr {
            Ast::Let(
                id,
                _t,          // optional explicit type parameter
                Some(value), // optional immediate assignment expression
            ) => {
                self.env
                    .bind_symbol(id.clone(), Symbol::Variable(value.get_ttype().unwrap()));
                None
            }

            Ast::FnDeclaration(id, fn_expr) => {
                let v = self.codegen(fn_expr);
                self.env.bind_symbol(
                    id.clone(),
                    Symbol::Function((*fn_expr).get_ttype().unwrap()),
                );
                v
            }

            Ast::TypeDeclaration(_id, _type_expr) => None,

            Ast::Id(_id, _ttype) => None,
            Ast::Binop(token, left, right, ttype) => {
                let l = self.codegen(&left).unwrap();
                let r = self.codegen(&right).unwrap();
                let v = match (token, l, r) {
                    (Token::Plus, AnyValueEnum::IntValue(l), AnyValueEnum::IntValue(r)) => Some(
                        AnyValueEnum::IntValue(self.builder.build_int_add(l, r, "tmp_add")),
                    ),
                    (Token::Minus, _, _) => None,
                    (Token::Star, _, _) => None,
                    (Token::Slash, _, _) => None,
                    (Token::Modulo, _, _) => None,
                    _ => None,
                };
                println!("binop {:?}", v.unwrap());
                v

                //
            }
            Ast::Unop(_token, _operand, _ttype) => None,
            Ast::Tuple(_exprs, _ttype) => None,
            Ast::Index(_obj, _idx, _ttype) => None,
            Ast::Assignment(_assignee, _val, _ttype) => None,
            Ast::Fn(_params, _ret_type, _body, _ttype) => None,
            Ast::Call(_callable, _args, _ttype) => None,
            Ast::Body(_stmts, _ttype) => None,
            Ast::If(_cond, _then, _elze, _ttype) => None,

            Ast::Int8(i) => None,
            Ast::Integer(i) => {
                let value = self
                    .context
                    .i64_type()
                    .const_int(TryInto::try_into(*i).unwrap(), true);
                Some(AnyValueEnum::IntValue(value))
            }
            Ast::Number(f) => {
                let value = self.context.f64_type().const_float(*f);
                Some(AnyValueEnum::FloatValue(value))
            }
            Ast::Bool(b) => {
                let value = self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false);
                Some(AnyValueEnum::IntValue(value))
            }
            Ast::String(_s) => None,
            _ => None,
        }
    }
}
