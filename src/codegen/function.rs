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
        params: Vec<Ast>,
        fn_type: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        let mut param_types = vec![];
        for p in &params {
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
                    arg.set_name(n.clone().as_str());
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
        params: Vec<Ast>,
        _return_type: Option<Box<Ast>>,
        body: Vec<Ast>,
        ttype: Ttype,
    ) -> Option<FunctionValue<'ctx>> {
        println!(
            "
compile function {:?}
params:     {:?}
body:       {:?}
fn type:    {:?}
",
            name, params, body, ttype,
        );
        let function = self.codegen_fn_proto(name, params, ttype).unwrap();
        println!("Function proto: {:?}", function);
        None
    }
}
