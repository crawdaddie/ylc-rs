use crate::lexer::Token;
use crate::parser::{Ast, Program};
use crate::symbols::{max_numeric_type, Env, Environment, Numeric, StackFrame, Ttype};
use std::collections::HashMap;

use llvm_sys::core::{LLVMConstArray, LLVMTypeOf};
use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};

mod conditional;
mod function;
mod numeric_binop;
mod numeric_comparison;
mod types;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    AnyValue, AnyValueEnum, ArrayValue, AsValueRef, BasicValue, BasicValueEnum, FunctionValue,
    IntValue, PointerValue,
};

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum Symbol<'ctx> {
    TypeDecl,
    Function(Ttype),
    Variable(Ttype, Option<PointerValue<'ctx>>, Option<LLVMTypeRef>),
    FnParam(u32, Ttype),
    RecursiveRef,
}

impl<'ctx> Env<Symbol<'ctx>> {
    pub fn new() -> Self {
        let stack: Vec<StackFrame<Symbol<'ctx>>> = vec![];
        Self { stack }
    }
}

impl<'ctx> Environment<Symbol<'ctx>> for Env<Symbol<'ctx>> {
    fn push(&mut self) -> Option<&mut StackFrame<Symbol<'ctx>>> {
        let frame = HashMap::new();
        self.stack.push(frame);
        self.stack.last_mut()
    }

    fn pop(&mut self) -> Option<&mut StackFrame<Symbol<'ctx>>> {
        self.stack.pop();
        self.stack.last_mut()
    }
    fn current(&mut self) -> Option<&mut StackFrame<Symbol<'ctx>>> {
        self.stack.last_mut()
    }

    fn bind_symbol(&mut self, name: String, value: Symbol<'ctx>) {
        if let Some(frame) = self.current() {
            frame.insert(name, value);
        }
    }
    fn lookup(&self, name: String) -> Option<&Symbol<'ctx>> {
        for frame in self.stack.iter().rev() {
            let x = frame.get(&name);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct GenericFns {
    ast: Ast,
    // _impls: HashSet<Ttype>,
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    env: Env<Symbol<'ctx>>,
    generic_fns: HashMap<String, GenericFns>,
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
            generic_fns: HashMap::new(),
            fn_stack: vec![],
        };
        compiler.compile_program(program)
    }

    fn add_return_value(&mut self, v: AnyValueEnum<'ctx>) {
        match v {
            AnyValueEnum::IntValue(_) => {
                self.builder.build_return(Some(&v.into_int_value()));
            }

            AnyValueEnum::FloatValue(_) => {
                self.builder.build_return(Some(&v.into_float_value()));
            }

            AnyValueEnum::ArrayValue(_) => {
                self.builder.build_return(Some(&v.into_array_value()));
            }

            AnyValueEnum::StructValue(_) => {
                self.builder.build_return(Some(&v.into_struct_value()));
            }

            AnyValueEnum::PointerValue(_) => {
                self.builder.build_return(Some(&v.into_struct_value()));
            }

            _ => {
                self.builder.build_return(None);
            }
        }
    }

    fn compile_program(&mut self, program: &Program) -> Result<FunctionValue<'ctx>, &'ctx str> {
        let main_fn = self.module.add_function(
            "main",
            self.type_to_llvm_fn(program.last().unwrap().ttype()),
            // self.context.void_type().fn_type(&[], false),
            None,
        );

        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
        self.push_fn_stack(&main_fn);

        let mut v = None;

        for stmt in program {
            v = self.codegen(stmt);
        }

        // let b_value_enum = BasicValueEnum::(
        //     self.context.void_type(), // .const_int(TryInto::try_into(0).unwrap(), true),
        // );

        // self.builder.build_return(None);

        if let Some(v) = v {
            self.add_return_value(v);
        } else {
            self.builder.build_return(None);
        };

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
            BasicValueEnum::IntValue(_i) => x,
            BasicValueEnum::FloatValue(_f) => x,
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
                    l = self.cast_numeric(l, desired_cast);
                    r = self.cast_numeric(r, desired_cast);
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
                let ltype = left.ttype();
                let rtype = left.ttype();
                let max_type = max_numeric_type(ltype, rtype);
                if let Ttype::Numeric(desired_cast) = max_type {
                    let mut l = to_basic_value_enum(self.codegen(left).unwrap());
                    let mut r = to_basic_value_enum(self.codegen(right).unwrap());
                    l = self.cast_numeric(l, desired_cast);
                    r = self.cast_numeric(r, desired_cast);
                    self.codegen_numeric_comparison(token, l, r, desired_cast)
                } else {
                    panic!("attempt to compare values with non-numeric types")
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
                let llvm_value = self.codegen(value).unwrap();
                unsafe {
                    let type_ref = LLVMTypeOf(llvm_value.as_value_ref());
                    let llvm_basic_type = BasicTypeEnum::new(type_ref);
                    let alloc = self
                        .builder
                        .build_alloca(llvm_basic_type, format!("alloc_{id}").as_str());
                    self.builder
                        .build_store(alloc, to_basic_value_enum(llvm_value));

                    self.env.bind_symbol(
                        id.clone(),
                        Symbol::Variable(
                            value.ttype(),
                            Some(alloc),
                            Some(LLVMTypeOf(llvm_value.as_value_ref())),
                        ),
                    );
                };

                Some(llvm_value)
            }

            Ast::FnDeclaration(id, fn_expr) => match (**fn_expr).clone() {
                Ast::Fn(_params, _body, fn_type) if fn_type.is_generic() => {
                    self.env.bind_symbol(id.clone(), Symbol::Function(fn_type));

                    self.generic_fns.insert(
                        id.clone(),
                        GenericFns {
                            ast: (**fn_expr).clone(),
                        },
                    );
                    None
                }
                Ast::Fn(params, body, fn_type) => {
                    let func = self.codegen_fn(id, &params, body, fn_type);
                    self.env
                        .bind_symbol(id.clone(), Symbol::Function(fn_expr.ttype()));

                    func.map(|f| f.as_any_value_enum())
                }
                _ => None,
            },

            Ast::TypeDeclaration(_id, _type_expr) => None,

            Ast::Id(id, _ttype) => {
                if let Some(sym) = self.env.lookup(id.clone()) {
                    match sym {
                        Symbol::FnParam(idx, _ttype) => {
                            let current_fn = self.current_fn().unwrap();
                            current_fn.get_nth_param(*idx).map(|e| e.into())
                        }

                        Symbol::Function(_fn_type) => self.get_function(id).map(|f| f.into()),
                        Symbol::RecursiveRef => self.current_fn().map(|f| f.as_any_value_enum()),
                        Symbol::Variable(_, llvm_val, llvm_type) => {
                            let llvm_type = unsafe { BasicTypeEnum::new(llvm_type.unwrap()) };
                            let load = self.builder.build_load(
                                llvm_type,
                                llvm_val.unwrap(),
                                format!("load_{id}").as_str(),
                            );
                            println!("LOADED {:?}", load);

                            Some(load.as_any_value_enum())
                        }
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
                    if ttype.is_numeric() {
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
                            .build_xor(op.into_int_value(), one, "xor")
                            .as_any_value_enum(),
                    )
                }
                _ => None,
            },
            Ast::Tuple(exprs, ttype) => {
                let es: Vec<AnyValueEnum> =
                    exprs.iter().map(|e| self.codegen(e).unwrap()).collect();
                let struct_val =
                    self.context.const_struct(
                        es.iter()
                            .map(|v| to_basic_value_enum(*v))
                            .collect::<Vec<_>>()
                            .as_slice(),
                        false,
                    );
                Some(struct_val.into())
            }

            Ast::List(exprs, ttype) => {
                let es: Vec<AnyValueEnum> =
                    exprs.iter().map(|e| self.codegen(e).unwrap()).collect();
                if let Ttype::List(list_type) = ttype {
                    let llvm_list_type = self
                        .type_to_llvm_array_type(*list_type.clone(), es.len().try_into().unwrap());

                    let array_alloca = self.builder.build_array_alloca(
                        llvm_list_type,
                        self.context
                            .i32_type()
                            .const_int(es.len().try_into().unwrap(), false),
                        "array_alloca",
                    );

                    let mut values: Vec<LLVMValueRef> =
                        es.iter().map(|val| val.as_value_ref()).collect();

                    for (i, v) in es.iter().enumerate() {
                        unsafe {
                            let gep = self.builder.build_in_bounds_gep(
                                llvm_list_type,
                                array_alloca,
                                &[self
                                    .context
                                    .i32_type()
                                    .const_int(TryInto::try_into(i).unwrap(), true)],
                                "inbounds_gep",
                            );

                            self.builder.build_store(gep, to_basic_value_enum(*v));
                        };
                    }

                    Some(array_alloca.as_any_value_enum())
                } else {
                    None
                }
            }
            Ast::Index(_obj, _idx, _ttype) => None,
            Ast::Assignment(_assignee, _val, _ttype) => None,
            Ast::Fn(_params, _body, _ttype) => None,
            Ast::Call(callable, args, _ttype) => match *callable.clone() {
                Ast::Id(fn_name, specific_type) => {
                    let callable = self.get_callable(fn_name, &specific_type);
                    self.compile_call(callable.unwrap(), args)
                }
                _ => None,
            },
            // Ast::Body(_stmts, _ttype) => None,
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
            Ast::String(s) => {
                // Ast::Tuple(exprs, ttype) => {
                //     let es: Vec<AnyValueEnum> =
                //         exprs.iter().map(|e| self.codegen(e).unwrap()).collect();
                //     let struct_val =
                //         self.context.const_struct(
                //             es.iter()
                //                 .map(|v| to_basic_value_enum(*v))
                //                 .collect::<Vec<_>>()
                //                 .as_slice(),
                //             false,
                //         );
                //     Some(struct_val.into())
                // }

                let value = self.context.const_string(s.as_bytes(), true);
                let size = self
                    .context
                    .i64_type()
                    .const_int(TryInto::try_into(s.len()).unwrap(), true);
                let es: Vec<AnyValueEnum> = [value.into(), size.into()].to_vec();

                let struct_val =
                    self.context.const_struct(
                        es.iter()
                            .map(|v| to_basic_value_enum(*v))
                            .collect::<Vec<_>>()
                            .as_slice(),
                        false,
                    );
                Some(struct_val.into())

                // Some(AnyValueEnum::ArrayValue(value))
            }
            _ => None,
        }
    }
}
// impl<'a, 'ctx> Drop for Compiler<'a, 'ctx> {
//     fn drop(&mut self) {
//         println!("Dropping compiler!!");
//     }
// }
