use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue,
};
use inkwell::IntPredicate;

use super::{to_basic_value_enum, Compiler, GenericFns};

use crate::parser::{Ast, Program};
use crate::symbols::{Env, Numeric, Symbol, Ttype};

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    #[inline]
    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    pub fn get_generic(&mut self, name: &str) -> &mut GenericFns {
        self.generic_fns.get_mut(name).unwrap()
    }
    pub fn generic_variant_name(&self, name: &str, t: Ttype) -> String {
        format!("{}_{}", name, t.mangle_name())
    }

    pub fn get_generic_function(
        &mut self,
        id: &str,
        fn_types: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let gf_exists = self
            .generic_fns
            .get(id)
            .map_or(false, |g| g.impls.contains(&fn_types));

        if gf_exists {
            self.get_function(self.generic_variant_name(id, fn_types).as_str())
        } else {
            None
        }
    }

    pub fn add_generic_function(&mut self, id: &str, fn_types: Ttype) {
        let mut gf = self.generic_fns.get_mut(id).unwrap();
        gf.impls.insert(fn_types);
    }

    pub fn current_fn(&self) -> Option<&FunctionValue<'ctx>> {
        self.fn_stack.last()
    }

    pub fn push_fn_stack(&mut self, function: &FunctionValue<'ctx>) {
        self.env.push();
        self.fn_stack.push(*function);
    }

    pub fn pop_fn_stack(&mut self) {
        self.env.pop();
        self.fn_stack.pop();
    }
    pub fn codegen_fn_proto(
        &mut self,
        name: &str,
        params: &Vec<Ast>,
        fn_type: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let mut param_types = vec![];
        let mut is_var_arg = false;
        for p in params {
            match p {
                Ast::VarArg => {
                    is_var_arg = true;
                }
                Ast::Id(_, _) => {
                    param_types.push(self.get_type_enum(p.get_ttype().unwrap()));
                }
                _ => {}
            }
        }

        if let Ttype::Fn(fn_types) = fn_type {
            let ret_type = fn_types.last().unwrap().clone();

            let param_types = param_types.as_slice();

            let fn_type = if ret_type == Ttype::Numeric(Numeric::Int) {
                let ret = self.context.i64_type();
                ret.fn_type(param_types, is_var_arg)
            } else if ret_type == Ttype::Numeric(Numeric::Num) {
                let ret = self.context.f64_type();
                ret.fn_type(param_types, is_var_arg)
            } else {
                let ret = self.context.void_type();
                ret.fn_type(param_types, is_var_arg)
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
        let function = self.codegen_fn_proto(name, params, ttype.clone()).unwrap();
        if body.is_empty() {
            // extern fn
            return Some(function);
        }

        let previous_block = self.builder.get_insert_block().unwrap();
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.fn_stack.push(function);
        self.env.push();

        let fn_types = if let Ttype::Fn(fn_types) = ttype {
            fn_types
        } else {
            panic!();
        };

        for (idx, p) in params.iter().enumerate() {
            if let Ast::Id(p, _) = p {
                let ttype = &fn_types[idx];
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

    pub fn compile_call(
        &mut self,
        callable_fn: FunctionValue<'ctx>,
        args: &Vec<Ast>,
    ) -> Option<AnyValueEnum<'ctx>> {
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
    }
}
