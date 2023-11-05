use crate::lexer::Token;
use crate::parser::{Ast, Program};
use crate::symbols::{max_numeric_type, Env, Numeric, Symbol, Ttype};
use std::error::Error;
mod conditional;
mod function;
mod numeric_binop;
mod numeric_comparison;
mod types;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
    PointerValue,
};

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    env: Env<Symbol>,
    fn_stack: Vec<FunctionValue<'ctx>>,
}

pub fn to_basic_value_enum(x: AnyValueEnum) -> BasicValueEnum {
    match x {
        AnyValueEnum::ArrayValue(v) => v.as_basic_value_enum(),
        AnyValueEnum::IntValue(v) => v.as_basic_value_enum(),
        AnyValueEnum::FloatValue(v) => v.as_basic_value_enum(),
        AnyValueEnum::PointerValue(v) => v.as_basic_value_enum(),
        AnyValueEnum::StructValue(v) => v.as_basic_value_enum(),
        AnyValueEnum::VectorValue(v) => v.as_basic_value_enum(),
        _ => panic!(),
    }
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
            fn_stack: vec![],
        };
        compiler.compile_program(program)
    }
    // fn into_value(&mut self, v: AnyValueEnum) {
    //     TryFrom::<dyn BasicValue>::try_from(v);
    // }
    fn add_return_value(&mut self, v: AnyValueEnum) {
        match v {
            AnyValueEnum::IntValue(_) => {
                self.builder.build_return(Some(&v.into_int_value()));
            }

            AnyValueEnum::FloatValue(_) => {
                self.builder.build_return(Some(&v.into_float_value()));
            }
            _ => {
                self.builder.build_return(None);
            }
        }
    }

    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    fn current_fn(&self) -> Option<&FunctionValue<'ctx>> {
        self.fn_stack.last()
    }

    fn push_fn_stack(&mut self, function: &FunctionValue<'ctx>) {
        self.env.push();
        self.fn_stack.push(*function);
    }

    fn pop_fn_stack(&mut self) {
        self.env.pop();
        self.fn_stack.pop();
    }

    fn compile_program(&mut self, program: &Program) -> Result<FunctionValue<'ctx>, &'ctx str> {
        let mut main_fn =
            self.module
                .add_function("main", self.context.void_type().fn_type(&[], false), None);

        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
        self.push_fn_stack(&main_fn);

        let mut v = None;
        for stmt in program {
            v = self.codegen(&stmt);
        }

        if let Some(v) = v {
            self.add_return_value(v);
            // TODO: clone old main_fn into new one with correct return type
            // let fn_ret_type = v.get_type();
            // match fn_ret_type {
            //     AnyTypeEnum::IntType(it) => {
            //         let og_blocks = main_fn.get_basic_blocks();
            //
            //         main_fn = self
            //             .module
            //             .add_function("main", it.fn_type(&[], false), None);
            //
            //         let basic_block = self.context.append_basic_block(main_fn, "entry");
            //     }
            //
            //     AnyTypeEnum::FloatType(ft) => {
            //         main_fn = self
            //             .module
            //             .add_function("main", ft.fn_type(&[], false), None)
            //     }
            //
            //
            // _ => {}
        } else {
            self.builder.build_return(None);
        }

        println!("top level env: {:?}", self.env);

        self.pop_fn_stack();

        Ok(main_fn)
    }

    fn cast_numeric(
        &mut self,
        x: BasicValueEnum<'ctx>,
        desired_cast: Numeric,
    ) -> BasicValueEnum<'ctx> {
        match x {
            BasicValueEnum::IntValue(i) if desired_cast == Numeric::Num => self
                .builder
                .build_signed_int_to_float(i, self.context.f64_type(), "int_to_float_cast")
                .as_basic_value_enum(),
            BasicValueEnum::IntValue(i) => x,
            BasicValueEnum::FloatValue(f) => x,
            _ => panic!(
                "Unknown numeric type {:?} to cast to {:?}",
                x.get_type(),
                desired_cast
            ),
        }
    }
    fn codegen_binop(
        &mut self,
        token: Token,
        left: &Ast,
        right: &Ast,
        ttype: Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let mut l = to_basic_value_enum(self.codegen(left).unwrap());
        let mut r = to_basic_value_enum(self.codegen(right).unwrap());
        match token {
            Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Modulo => {
                if let Ttype::Numeric(desired_cast) = ttype {
                    if ttype.is_num() {
                        l = self.cast_numeric(l, desired_cast);
                        r = self.cast_numeric(r, desired_cast);
                    }
                    self.codegen_numeric_binop(token, l, r, desired_cast)
                } else {
                    match (l, r) {
                        (BasicValueEnum::IntValue(_), BasicValueEnum::IntValue(_)) => {
                            self.codegen_numeric_binop(token, l, r, Numeric::Int)
                        }

                        (BasicValueEnum::FloatValue(_), BasicValueEnum::FloatValue(_)) => {
                            self.codegen_numeric_binop(token, l, r, Numeric::Num)
                        }

                        (BasicValueEnum::FloatValue(_), BasicValueEnum::IntValue(_)) => {
                            let r = self.cast_numeric(r, Numeric::Num);
                            self.codegen_numeric_binop(token, l, r, Numeric::Num)
                        }

                        (BasicValueEnum::IntValue(_), BasicValueEnum::FloatValue(_)) => {
                            let l = self.cast_numeric(l, Numeric::Num);
                            self.codegen_numeric_binop(token, l, r, Numeric::Num)
                        }
                        _ => panic!("attempt to use a numeric binop with non-numeric types"),
                    }
                }
            }
            Token::Equality | Token::Gt | Token::Gte | Token::Lt | Token::Lte => {
                let ltype = left.get_ttype().unwrap();
                let rtype = left.get_ttype().unwrap();
                if let Some(max_type) = max_numeric_type(ltype, rtype) {
                    if let Ttype::Numeric(desired_cast) = max_type {
                        let mut l = to_basic_value_enum(self.codegen(left).unwrap());
                        let mut r = to_basic_value_enum(self.codegen(right).unwrap());
                        if max_type.is_num() {
                            l = self.cast_numeric(l, desired_cast);
                            r = self.cast_numeric(r, desired_cast);
                        };
                        self.codegen_numeric_comparison(token, l, r, desired_cast)
                    } else {
                        panic!("attempt to compare values with non-numeric types")
                    }
                } else {
                    panic!()
                }
            }
            _ => None,
        }
    }
    fn codegen_block(&mut self, block: &Vec<Ast>) -> Option<AnyValueEnum<'ctx>> {
        self.env.push();
        let mut c = None;
        for v in block {
            c = self.codegen(v);
        }
        self.env.pop();
        c
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
                if let Ast::Fn(params, return_type, body, fn_type) = (**fn_expr).clone() {
                    let func = self.codegen_fn(id, &params, return_type, body, fn_type);
                    self.env
                        .bind_symbol(id.clone(), Symbol::Function(fn_expr.get_ttype().unwrap()));

                    func.map(|f| f.as_any_value_enum())
                } else {
                    None
                }
            }

            Ast::TypeDeclaration(_id, _type_expr) => None,

            Ast::Id(id, ttype) => {
                if let Some(sym) = self.env.lookup(id.clone()) {
                    match sym {
                        Symbol::FnParam(idx, ttype) => {
                            let current_fn = self.current_fn().unwrap();
                            current_fn.get_nth_param(idx.clone()).map(|e| e.into())
                        }
                        Symbol::Function(fn_type) => self.get_function(&id).map(|f| f.into()),
                        Symbol::RecursiveRef => self.current_fn().map(|f| f.as_any_value_enum()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Ast::Binop(token, left, right, ttype) => {
                // println!("codegen binop {:?}", expr);
                self.codegen_binop(token.clone(), left, right, ttype.clone())
            }
            Ast::Unop(token, operand, ttype) => match token {
                Token::Minus => {
                    let op = to_basic_value_enum(self.codegen(operand).unwrap());
                    if ttype.is_num() {
                        let zero = self.context.f64_type().const_float(0.0);
                        Some(
                            self.builder
                                .build_float_sub(zero, op.into_float_value(), "negative_num")
                                .as_any_value_enum(),
                        )
                    } else {
                        let zero = self.context.i64_type().const_int(0, false);
                        Some(
                            self.builder
                                .build_int_sub(zero, op.into_int_value(), "negative_num")
                                .as_any_value_enum(),
                        )
                    }
                }
                Token::Bang => {
                    let op = to_basic_value_enum(self.codegen(operand).unwrap());
                    let one = self.context.bool_type().const_int(1, false);
                    Some(
                        self.builder
                            .build_xor(op.into_int_value(), one, "negated_value")
                            .as_any_value_enum(),
                    )
                }
                _ => None,
            },
            Ast::Tuple(_exprs, _ttype) => None,
            Ast::Index(_obj, _idx, _ttype) => None,
            Ast::Assignment(_assignee, _val, _ttype) => None,
            Ast::Fn(_params, _ret_type, _body, _ttype) => None,
            Ast::Call(callable, args, _ttype) => {
                if let Some(callable) = self.codegen(callable) {
                    // callable is an AnyValue enum
                    let callable_fn = callable.into_function_value();
                    let compiled_args: Vec<BasicValueEnum> = args
                        .iter()
                        .map(|a| to_basic_value_enum(self.codegen(a).unwrap()))
                        .collect();

                    let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                        .iter()
                        .by_ref()
                        .map(|&val| val.into())
                        .collect();

                    Some(
                        self.builder
                            .build_call(callable_fn, argsv.as_slice(), "call")
                            .as_any_value_enum(),
                    )
                } else {
                    None
                }
            }
            Ast::Body(_stmts, _ttype) => None,
            Ast::If(cond, then, elze, ttype) => {
                let condition = self.codegen(cond)?.into_int_value();
                self.codegen_conditional_expr(condition, then, elze, ttype.clone())
            }

            Ast::Int8(i) => {
                let value = self
                    .context
                    .i8_type()
                    .const_int(TryInto::try_into(*i).unwrap(), true);
                Some(AnyValueEnum::IntValue(value))
            }
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
