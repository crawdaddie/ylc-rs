use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum};
use inkwell::IntPredicate;

use super::Compiler;

use crate::codegen::to_basic_value;
use crate::lexer::Token;
use crate::parser::{Ast, Program};
use crate::symbols::{Env, Numeric, Symbol, Ttype};

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn codegen_fn(&mut self, name: &str, fn_expr: &Ast) -> Option<AnyValueEnum<'ctx>> {
        None
    }
}
