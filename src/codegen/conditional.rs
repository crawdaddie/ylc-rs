use inkwell::values::{AnyValue, AnyValueEnum, IntValue};

use crate::{parser::Ast, symbols::Ttype};

use super::{to_basic_value_enum, Compiler};

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn codegen_conditional_expr(
        &mut self,
        condition: IntValue<'ctx>,
        then: &Vec<Ast>,
        elze: &Option<Vec<Ast>>,
        _ttype: Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let parent = self.current_fn()?;

        let then_bb = self.context.append_basic_block(*parent, "then");
        let else_bb = self.context.append_basic_block(*parent, "else");
        let cont_bb = self.context.append_basic_block(*parent, "ifcont");

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb);

        // build then block
        self.builder.position_at_end(then_bb);
        let then_val = self.codegen_block(then)?;
        self.builder.build_unconditional_branch(cont_bb);

        let then_bb = self.builder.get_insert_block().unwrap();

        // build else block
        let else_val = if let Some(elze) = elze {
            self.builder.position_at_end(else_bb);

            self.codegen_block(elze)
        } else {
            None
        };

        self.builder.build_unconditional_branch(cont_bb);

        let else_bb = self.builder.get_insert_block().unwrap();

        // emit merge block
        self.builder.position_at_end(cont_bb);

        let phi = self
            .builder
            .build_phi(to_basic_value_enum(then_val).get_type(), "iftmp");

        if let Some(else_val) = else_val {
            phi.add_incoming(&[
                (&to_basic_value_enum(then_val), then_bb),
                (&to_basic_value_enum(else_val), else_bb),
            ]);
        } else {
            phi.add_incoming(&[(&to_basic_value_enum(then_val), then_bb)])
        }
        Some(phi.as_any_value_enum())
    }
}
