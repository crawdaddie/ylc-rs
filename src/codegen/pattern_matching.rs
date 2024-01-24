use inkwell::{
    basic_block::BasicBlock,
    values::{AnyValue, AnyValueEnum, AsValueRef, IntValue, PhiValue},
};
use llvm_sys::{
    core::LLVMAddIncoming,
    prelude::{LLVMBasicBlockRef, LLVMValueRef},
};

use crate::parser::Ast;
use crate::symbols::{Environment, Ttype};

use super::{Compiler};

fn add_incoming<'ctx>(
    phi: &mut PhiValue<'ctx>,
    incoming: Vec<(AnyValueEnum<'ctx>, &BasicBlock<'ctx>)>,
) {
    let (mut values, mut basic_blocks): (Vec<LLVMValueRef>, Vec<LLVMBasicBlockRef>) = {
        incoming
            .iter()
            .map(|&(v, bb)| (v.as_value_ref(), bb.as_mut_ptr()))
            .unzip()
    };

    unsafe {
        LLVMAddIncoming(
            phi.as_value_ref(),
            values.as_mut_ptr(),
            basic_blocks.as_mut_ptr(),
            incoming.len() as u32,
        );
    }
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn match_pattern(&mut self, _pattern: &Ast) -> IntValue<'ctx> {
        self.int(&1).unwrap().into_int_value()
    }
    // fn process_arm(
    //     &mut self,
    //     pattern: &Ast,
    //     expr: &Ast,
    //     block: &BasicBlock<'ctx>,
    //     next_block: &BasicBlock<'ctx>,
    // ) -> (AnyValueEnum<'ctx>, &BasicBlock<'ctx>) {
    //     let condition = self.match_pattern(pattern);
    //     self.builder
    //         .build_conditional_branch(condition, *block, *next_block);
    //     self.builder.position_at_end(*block);
    //     // incoming[idx] = (self.codegen(expr).unwrap(), block);
    //     self.env.pop();
    //
    //     (self.codegen(expr).unwrap(), block)
    // }
    pub fn codegen_match_expr(
        &mut self,
        var: &Ast,
        arms: &Vec<(Ast, Ast)>,
        ttype: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let _var = self.codegen(var)?;
        let parent = self.current_fn()?;
        let arms: Vec<(&(Ast, Ast), BasicBlock<'ctx>)> = arms
            .iter()
            .map(|arm| (arm, self.context.append_basic_block(*parent, "")))
            .collect();

        let continue_block = self.context.append_basic_block(*parent, "continue");

        let mut phi = self
            .builder
            .build_phi(self.type_to_llvm_type(ttype.clone()), "match_phi");

        let num_arms = arms.len();
        let incoming: Vec<(AnyValueEnum<'ctx>, &BasicBlock<'ctx>)> = arms
            .iter()
            .enumerate()
            .map(|(idx, ((pattern, expr), block))| {
                let next_arm_block = if idx == num_arms - 1 {
                    continue_block
                } else {
                    arms[idx + 1].1
                };
                self.env.push();

                let condition = self.match_pattern(pattern);
                self.builder
                    .build_conditional_branch(condition, *block, next_arm_block);
                self.builder.position_at_end(*block);
                self.builder.build_unconditional_branch(next_arm_block);
                // incoming[idx] = (self.codegen(expr).unwrap(), block);
                self.env.pop();

                (self.codegen(expr).unwrap(), block)
            })
            .collect();

        // self.builder.build_unconditional_branch(continue_block);
        self.builder.position_at_end(continue_block);

        add_incoming(&mut phi, incoming);
        Some(phi.as_any_value_enum())

        // self.int(&291)
    }
}
