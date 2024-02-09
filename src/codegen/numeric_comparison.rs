use inkwell::values::{AnyValue, AnyValueEnum, BasicValueEnum};
use inkwell::{FloatPredicate, IntPredicate};

use super::Compiler;

use crate::lexer::Token;

use crate::symbols::Numeric;

fn is_num(n: Numeric) -> bool {
    n == Numeric::Num
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn codegen_numeric_comparison(
        &mut self,
        token: Token,
        l: BasicValueEnum<'ctx>,
        r: BasicValueEnum<'ctx>,
        desired_cast: Numeric,
    ) -> Option<AnyValueEnum<'ctx>> {
        match token {
            Token::Lt if desired_cast == Numeric::Num => Some(
                self.builder
                    .build_float_compare(
                        FloatPredicate::OLT,
                        l.into_float_value(),
                        r.into_float_value(),
                        "tmp_lt",
                    )
                    .as_any_value_enum(),
            ),
            Token::Lt => Some(
                self.builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        l.into_int_value(),
                        r.into_int_value(),
                        "tmp_lt",
                    )
                    .as_any_value_enum(),
            ),

            Token::Lte if desired_cast == Numeric::Num => Some(
                self.builder
                    .build_float_compare(
                        FloatPredicate::OLE,
                        l.into_float_value(),
                        r.into_float_value(),
                        "tmp_lte",
                    )
                    .as_any_value_enum(),
            ),

            Token::Lte => Some(
                self.builder
                    .build_int_compare(
                        IntPredicate::SLE,
                        l.into_int_value(),
                        r.into_int_value(),
                        "tmp_lte",
                    )
                    .as_any_value_enum(),
            ),

            Token::Gt if desired_cast == Numeric::Num => Some(
                self.builder
                    .build_float_compare(
                        FloatPredicate::OGT,
                        l.into_float_value(),
                        r.into_float_value(),
                        "tmp_gt",
                    )
                    .as_any_value_enum(),
            ),

            Token::Gt => Some(
                self.builder
                    .build_int_compare(
                        IntPredicate::SGT,
                        l.into_int_value(),
                        r.into_int_value(),
                        "tmp_gt",
                    )
                    .as_any_value_enum(),
            ),

            Token::Gte if desired_cast == Numeric::Num => Some(
                self.builder
                    .build_float_compare(
                        FloatPredicate::OGE,
                        l.into_float_value(),
                        r.into_float_value(),
                        "tmp_gte",
                    )
                    .as_any_value_enum(),
            ),

            Token::Gte => Some(
                self.builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        l.into_int_value(),
                        r.into_int_value(),
                        "tmp_gte",
                    )
                    .as_any_value_enum(),
            ),

            Token::Equality if desired_cast == Numeric::Num => Some(
                self.builder
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        l.into_float_value(),
                        r.into_float_value(),
                        "tmp_eq",
                    )
                    .as_any_value_enum(),
            ),

            Token::Equality => Some(
                self.builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        l.into_int_value(),
                        r.into_int_value(),
                        "tmp_eq",
                    )
                    .as_any_value_enum(),
            ),

            _ => None,
        }
    }
}
