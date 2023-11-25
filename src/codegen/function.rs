use std::collections::HashMap;

use inkwell::types::BasicType;
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue,
};

use super::{to_basic_value_enum, Compiler, GenericFns, Symbol};

use crate::parser::Ast;
use crate::symbols::{Env, Environment, Numeric, StackFrame, Ttype};
use crate::typecheck::update_types;

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    #[inline]
    pub fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    pub fn generic_variant_name(&self, name: &str, t: &Ttype) -> String {
        let ts = t.contained_types();
        let (_, tss) = ts.split_last().unwrap();
        format!(
            "{}_{}",
            name,
            tss.iter()
                .map(|t| { format!("{:?}", t).to_string() })
                .collect::<Vec<String>>()
                .join("_")
        )
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
                    param_types.push(self.get_type_enum(p.ttype()));
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
        if let Some(v) = v {
            self.add_return_value(v);
        } else {
            self.builder.build_return(None);
        }

        self.env.pop();
        self.fn_stack.pop();

        self.builder.position_at_end(previous_block);
        Some(function)
    }
    pub fn compile_generic_fn_variant(
        &mut self,
        mangled_variant_name: String,
        fn_expr: &mut Ast,
        spec_types: &Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let mut subs = HashMap::<Ttype, Ttype>::new();
        if let (Ast::Fn(params_vec, _, _), Ttype::Fn(ts)) = (&fn_expr, spec_types) {
            for (p, t) in params_vec.iter().zip(ts) {
                subs.insert(p.ttype(), t.clone());
            }
        } else {
            panic!("Codegen error");
        };

        update_types(fn_expr, &subs, &mut Env::<crate::symbols::Symbol>::new());

        if let Ast::Fn(params, body, fn_type) = fn_expr {
            self.codegen_fn(
                mangled_variant_name.as_str(),
                params,
                body.clone(),
                fn_type.clone(),
            )
        } else {
            None
        }
    }

    pub fn get_callable(
        &mut self,
        fn_name: String,
        spec_type: &Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        match self.env.lookup(fn_name.clone()) {
            Some(Symbol::Function(fn_type)) if fn_type.is_generic() => {
                let mangled_variant_name = self.generic_variant_name(fn_name.as_str(), spec_type);
                match self.get_function(mangled_variant_name.as_str()) {
                    Some(fn_value) => Some(fn_value),

                    None => {
                        if let Some(gen_fn) = self.generic_fns.get(&fn_name) {
                            // compile new version of callable
                            self.compile_generic_fn_variant(
                                mangled_variant_name,
                                &mut gen_fn.ast.clone(),
                                spec_type,
                            )
                        } else {
                            panic!();
                        }
                    }
                }
            }
            Some(Symbol::Function(_fn_type)) => self.get_function(&fn_name),
            _ => panic!(),
        }
    }

    pub fn compile_call(
        &mut self,
        callable_fn: FunctionValue<'ctx>,
        args: &[Ast],
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

#[cfg(test)]
mod tests {}
