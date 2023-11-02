use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum};

use super::Compiler;

use crate::codegen::to_basic_value;
use crate::lexer::Token;
use crate::parser::{Ast, Program};
use crate::symbols::{Env, Numeric, Symbol, Ttype};

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn codegen_numeric_binop(
        &mut self,
        token: Token,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        desired_cast: Numeric,
    ) -> Option<AnyValueEnum<'ctx>> {
        match token {
            Token::Plus if is_num(desired_cast) => Some(
                self.builder
                    .build_float_add(
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        "tmp_add",
                    )
                    .as_any_value_enum(),
            ),
            Token::Plus => Some(
                self.builder
                    .build_int_add(l.into_int_value(), r.into_int_value(), "tmp_add")
                    .as_any_value_enum(),
            ),
            Token::Minus if is_num(desired_cast) => Some(
                self.builder
                    .build_float_sub(
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        self.cast_numeric(r, desired_cast).into_float_value(),
                        "tmp_sub",
                    )
                    .as_any_value_enum(),
            ),
            Token::Minus => Some(
                self.builder
                    .build_int_sub(l.into_int_value(), r.into_int_value(), "tmp_sub")
                    .as_any_value_enum(),
            ),

            Token::Star if is_num(desired_cast) => Some(
                self.builder
                    .build_float_mul(
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        self.cast_numeric(r, desired_cast).into_float_value(),
                        "tmp_mul",
                    )
                    .as_any_value_enum(),
            ),
            Token::Star => Some(
                self.builder
                    .build_int_mul(l.into_int_value(), r.into_int_value(), "tmp_mul")
                    .as_any_value_enum(),
            ),

            Token::Slash if is_num(desired_cast) => Some(
                self.builder
                    .build_float_div(
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        self.cast_numeric(r, desired_cast).into_float_value(),
                        "tmp_div",
                    )
                    .as_any_value_enum(),
            ),
            Token::Slash => Some(
                self.builder
                    .build_int_signed_div(l.into_int_value(), r.into_int_value(), "tmp_div")
                    .as_any_value_enum(),
            ),

            Token::Modulo if is_num(desired_cast) => Some(
                self.builder
                    .build_float_rem(
                        self.cast_numeric(l, desired_cast).into_float_value(),
                        self.cast_numeric(r, desired_cast).into_float_value(),
                        "tmp_modulo",
                    )
                    .as_any_value_enum(),
            ),
            Token::Modulo => Some(
                self.builder
                    .build_int_signed_rem(l.into_int_value(), r.into_int_value(), "tmp_modulo")
                    .as_any_value_enum(),
            ),
            _ => None,
        }
    }
}
