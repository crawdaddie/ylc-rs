use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum, FunctionValue};
use inkwell::IntPredicate;

use super::Compiler;

use crate::parser::{Ast, Program};
use crate::symbols::{Env, Numeric, Symbol, Ttype};

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn codegen_fn_proto(
        &mut self,
        name: &str,
        params: &Vec<Ast>,
        fn_type: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let mut param_types = vec![];
        for p in params {
            param_types.push(self.get_type_enum(p.get_ttype().unwrap()))
        }

        if let Ttype::Fn(fn_types) = fn_type {
            let ret_type = fn_types.last().unwrap().clone();

            let param_types = param_types.as_slice();

            let fn_type = if ret_type == Ttype::Numeric(Numeric::Int) {
                let ret = self.context.i64_type();
                ret.fn_type(param_types, false)
            } else if ret_type == Ttype::Numeric(Numeric::Num) {
                let ret = self.context.f64_type();
                ret.fn_type(param_types, false)
            } else {
                let ret = self.context.void_type();
                ret.fn_type(param_types, false)
            };

            let fn_val = self.module.add_function(name, fn_type, None);
            for (i, arg) in fn_val.get_param_iter().enumerate() {
                if let Ast::Id(n, _) = &params[i] {
                    arg.set_name(n.as_str());
                }
            }
            Some(fn_val)
        } else {
            None
        }
    }
    pub fn codegen_fn(
        &mut self,
        name: &str,
        params: &Vec<Ast>,
        _return_type: Option<Ttype>,
        body: Vec<Ast>,
        ttype: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let function = self.codegen_fn_proto(name, params, ttype).unwrap();
        if body.is_empty() {
            // extern fn
            return Some(function);
        }

        let previous_block = self.builder.get_insert_block().unwrap();
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.fn_stack.push(function);
        self.env.push();

        for (idx, p) in params.iter().enumerate() {
            if let Ast::Id(p, ttype) = p {
                self.env.bind_symbol(
                    p.clone(),
                    Symbol::FnParam(u32::try_from(idx).unwrap(), ttype.clone()),
                )
            }
        }
        self.env.bind_symbol(name.into(), Symbol::RecursiveRef);

        let mut v = None;
        for x in body {
            v = self.codegen(&x);
        }

        if v.is_some() {
            self.add_return_value(v.unwrap());
        } else {
            self.builder.build_return(None);
        }

        self.env.pop();
        self.fn_stack.pop();

        self.builder.position_at_end(previous_block);
        Some(function)
    }
}
