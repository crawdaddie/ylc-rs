use inkwell::{
    basic_block::BasicBlock,
    values::{AnyValue, AnyValueEnum, AsValueRef, BasicValue, BasicValueEnum, IntValue, PhiValue},
};
use llvm_sys::{
    core::LLVMAddIncoming,
    prelude::{LLVMBasicBlockRef, LLVMValueRef},
};

use crate::parser::{Ast, MatchArm};
use crate::symbols::{Environment, Ttype};

use super::{to_basic_value_enum, Compiler};

// fn add_incoming<'ctx>(
//     phi: &mut PhiValue<'ctx>,
//     incoming: Vec<(AnyValueEnum<'ctx>, &BasicBlock<'ctx>)>,
// ) {
//     let (mut values, mut basic_blocks) = incoming.iter().unzip();
//
//     unsafe {
//         LLVMAddIncoming(
//             phi.as_value_ref(),
//             values.as_mut_ptr(),
//             basic_blocks.as_mut_ptr(),
//             incoming.len() as u32,
//         );
//     }
// }

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn add_to_phi(&mut self, mut phi: PhiValue, incoming: Vec<(LLVMValueRef, LLVMBasicBlockRef)>) {
        let len = incoming.len() as u32;
        let (mut values, mut basic_blocks): (Vec<LLVMValueRef>, Vec<LLVMBasicBlockRef>) =
            incoming.into_iter().unzip();

        unsafe {
            LLVMAddIncoming(
                phi.as_value_ref(),
                values.as_mut_ptr(),
                basic_blocks.as_mut_ptr(),
                len,
            );
        }
    }
    fn eq(&mut self, pattern: &Ast, var: &AnyValueEnum<'ctx>, var_type: Ttype) {}
    fn build_branch_condition(
        &mut self,
        pattern: &Ast,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> IntValue<'ctx> {
        self.int(&1).unwrap().into_int_value()
    }

    pub fn codegen_match_expr(
        &mut self,
        var: &Ast,
        branches: &Vec<MatchArm>,
        ttype: &Ttype,
    ) -> Option<AnyValueEnum<'ctx>> {
        let var_type = var.ttype();
        let var = self.codegen(var)?;
        let parent = self.current_fn()?;
        let num_arms = branches.len();
        let branches: Vec<(&MatchArm, BasicBlock<'ctx>)> = branches
            .iter()
            .enumerate()
            .map(|(idx, branch)| {
                (
                    branch,
                    self.context
                        .append_basic_block(*parent, format!("branch{}", idx).as_str()),
                )
            })
            .collect();

        let continue_block = self.context.append_basic_block(*parent, "continue");

        let mut branch_iter = branches.iter().peekable();
        let mut cont = true;

        let mut phi_incoming_vals: Vec<(LLVMValueRef, LLVMBasicBlockRef)> = vec![];

        while cont {
            let branch = branch_iter.next().unwrap();
            self.env.push();
            cont = match branch_iter.peek() {
                Some(next) => {
                    println!("{:?} -- {:?}", branch.1.get_name(), next.1.get_name());
                    let ((pattern_ast, branch_ast), branch_block) = branch;
                    let (_, next_block) = next;
                    self.builder.build_conditional_branch(
                        self.build_branch_condition(pattern_ast, &var, &var_type),
                        *branch_block,
                        *next_block,
                    );
                    self.builder.position_at_end(*branch_block);
                    let branch_codegen = self.codegen_block(branch_ast)?;
                    let codegen_val_enum = to_basic_value_enum(branch_codegen);
                    self.builder.build_unconditional_branch(continue_block);
                    phi_incoming_vals.push((
                        codegen_val_enum.as_value_ref(),
                        self.builder.get_insert_block().unwrap().as_mut_ptr(),
                    ));
                    true
                }
                None => {
                    let ((_, branch_ast), default_block) = branch;
                    self.builder.position_at_end(*default_block);
                    let branch_codegen = self.codegen_block(branch_ast)?;
                    let codegen_val_enum = to_basic_value_enum(branch_codegen);
                    self.builder.build_unconditional_branch(continue_block);
                    phi_incoming_vals.push((
                        codegen_val_enum.as_value_ref(),
                        self.builder.get_insert_block().unwrap().as_mut_ptr(),
                    ));
                    false
                }
            };
            self.env.pop();
        }

        self.builder.position_at_end(continue_block);

        let phi = self
            .builder
            .build_phi(self.type_to_llvm_type(ttype.clone()), "pattern_match_phi");

        self.add_to_phi(phi, phi_incoming_vals);

        Some(phi.as_any_value_enum())
        // for (&branch, &next) in branches.iter().tuple_windows() {
        //     println!("{:?} -- {:?}", branch.1.get_name(), next.1.get_name());
        // }

        // self.int(&291)

        // let continue_block = self.context.append_basic_block(*parent, "continue");
        //
        // let ((pattern_ast, branch_ast), branch_block) = arms[0];
        // let (_, next_block) = arms[1];
        //
        // self.builder.build_conditional_branch(
        //     self.build_match_condition(pattern_ast),
        //     branch_block,
        //     next_block,
        // );
        // self.builder.position_at_end(branch_block);
        // let branch_val = self.int(&291).unwrap();
        // self.builder.build_unconditional_branch(continue_block);
        //
        // let phi = self
        //     .builder
        //     .build_phi(self.type_to_llvm_type(ttype.clone()), "match_phi");
        //
        // phi.add_incoming(&[(&to_basic_value_enum(branch_val), branch_block)]);
        //
        // Some(phi.as_any_value_enum())
        /*


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


        add_incoming(&mut phi, incoming);
            */
    }
}
