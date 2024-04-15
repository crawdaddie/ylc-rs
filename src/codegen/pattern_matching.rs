use inkwell::{
    basic_block::BasicBlock,
    values::{AnyValue, AnyValueEnum, AsValueRef, IntValue, PhiValue},
};
use llvm_sys::{
    core::LLVMAddIncoming,
    prelude::{LLVMBasicBlockRef, LLVMValueRef},
};

use crate::symbols::{max_numeric_type, Environment, Ttype};
use crate::{
    lexer::Token,
    parser::{Ast, MatchArm},
};

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
//
type AssignmentList<'a> = Vec<(Ast, &'a AnyValueEnum<'a>)>;
type MatchConditionResult<'a> = (IntValue<'a>, Option<AssignmentList<'a>>);

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn add_to_phi(&mut self, phi: PhiValue, incoming: Vec<(LLVMValueRef, LLVMBasicBlockRef)>) {
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
    fn eq(&mut self, pattern: &Ast, var: &AnyValueEnum<'ctx>, var_type: &Ttype) -> IntValue<'ctx> {
        let ltype = var_type;
        let rtype = pattern.ttype();
        let max_type = max_numeric_type(ltype.clone(), rtype);
        if let Ttype::Numeric(desired_cast) = max_type {
            let mut l = to_basic_value_enum(*var);
            let mut r = to_basic_value_enum(self.codegen(pattern).unwrap());
            l = self.cast_numeric(l, desired_cast);
            r = self.cast_numeric(r, desired_cast);

            let res = self
                .codegen_numeric_comparison(Token::Equality, l, r, desired_cast)
                .unwrap()
                .into_int_value();
            println!("eq comparison {:?}", res.to_string());
            res
        } else {
            panic!("attempt to compare values with non-numeric types")
        }
    }
    fn match_list(
        &mut self,
        members: &Vec<Ast>,
        list_type: &Ttype,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> MatchConditionResult<'ctx> {
        (self.int(&0).unwrap().into_int_value(), None)
    }
    fn build_branch_condition(
        &mut self,
        pattern: &Ast,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> MatchConditionResult<'ctx> {
        println!("build branch {:?} -> {:?} [{:?}]", pattern, var, var_type);
        match pattern {
            Ast::Int8(_i8) => (self.eq(pattern, var, var_type), None),
            Ast::Integer(_i64) => (self.eq(pattern, var, var_type), None),
            Ast::Number(_f64) => (self.eq(pattern, var, var_type), None),
            Ast::Bool(_bool) => (self.eq(pattern, var, var_type), None),
            Ast::List(members, list_type) => self.match_list(members, list_type, var, var_type),
            Ast::Unop(tok, expr, t) => match tok {
                // &Token::If => {
                //     // self.
                //
                // }
                _ => (self.int(&0).unwrap().into_int_value(), None),
            },
            _ => (self.int(&0).unwrap().into_int_value(), None),
        }
    }
    fn build_default_pattern(
        &mut self,
        pattern_ast: &Ast,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) {
        println!(
            "final branch {:?} {:?} [{:?}]",
            pattern_ast, &var, &var_type
        );
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
        let branches: Vec<(&MatchArm, BasicBlock<'ctx>)> = branches
            .iter()
            .enumerate()
            .map(|(idx, branch)| {
                (
                    branch,
                    self.context
                        .append_basic_block(*parent, format!("BranchBlock{}", idx).as_str()),
                )
            })
            .collect();

        let continue_block = self.context.append_basic_block(*parent, "continue");

        let mut branch_iter = branches.iter().peekable();
        let mut cont = true;

        let mut phi_incoming_vals: Vec<(LLVMValueRef, LLVMBasicBlockRef)> = vec![];

        let mut branch_idx = 0;
        while cont {
            let branch = branch_iter.next().unwrap();
            self.env.push();
            cont = match branch_iter.peek() {
                Some(next) => {
                    // NORMAL BRANCH
                    let ((pattern_ast, branch_ast), branch_block) = branch;
                    let (_, next_block) = next;
                    let (branch_cond, _) =
                        self.build_branch_condition(pattern_ast, &var, &var_type);
                    branch_cond.set_name(format!("Pattern{}", branch_idx).as_str());

                    let _ = self.builder.build_conditional_branch(
                        branch_cond,
                        *branch_block,
                        *next_block,
                    );
                    self.builder.position_at_end(*branch_block);

                    let branch_codegen = self.codegen_block(branch_ast)?;
                    let codegen_val_enum = to_basic_value_enum(branch_codegen);

                    let _ = self.builder.build_unconditional_branch(continue_block);

                    phi_incoming_vals.push((
                        codegen_val_enum.as_value_ref(),
                        self.builder.get_insert_block().unwrap().as_mut_ptr(),
                    ));
                    true
                }
                None => {
                    // DEFAULT / FINAL BRANCH
                    let ((pattern_ast, branch_ast), default_block) = branch;
                    self.build_default_pattern(pattern_ast, &var, &var_type);
                    self.builder.position_at_end(*default_block);

                    let branch_codegen = self.codegen_block(branch_ast)?;
                    let codegen_val_enum = to_basic_value_enum(branch_codegen);

                    let _ = self.builder.build_unconditional_branch(continue_block);

                    phi_incoming_vals.push((
                        codegen_val_enum.as_value_ref(),
                        self.builder.get_insert_block().unwrap().as_mut_ptr(),
                    ));
                    false
                }
            };
            self.env.pop();
            branch_idx += 1;
        }

        self.builder.position_at_end(continue_block);

        let phi = self
            .builder
            .build_phi(self.type_to_llvm_type(ttype.clone()), "pattern_match_phi")
            .unwrap();

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
