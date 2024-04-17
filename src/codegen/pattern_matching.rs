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
// type AssignmentList<'a> = Vec<(Ast, &'a AnyValueEnum<'a>)>;
type MatchConditionResult<'a> = IntValue<'a>;

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
    fn _falsy(&mut self) -> IntValue<'ctx> {
        self.int(&0).unwrap().into_int_value()
    }

    fn _truthy(&mut self) -> IntValue<'ctx> {
        self.int(&1).unwrap().into_int_value()
    }

    fn match_list(
        &mut self,
        members: &Vec<Ast>,
        list_type: &Ttype,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> MatchConditionResult<'ctx> {
        let len_destructures = members
            .iter()
            .filter(|el| match el {
                Ast::Id(_, _) => true,
                _ => false,
            })
            .count();

        // for
        // TODO: for each element in arrayed pattern assign to a pointer offset
        // and handle any spread list operators
        self._falsy()
    }
    fn branch_subcondition(
        &mut self,
        pattern: &Ast,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> MatchConditionResult<'ctx> {
        if let Ast::Tuple(els, _) = pattern {
            let ass = &els[0];
            let x = self.build_branch_condition(ass, var, var_type);
            let _subcondition = self.codegen(&els[1]).unwrap().into_int_value();
            self.builder
                .build_and(x, _subcondition, "PatternMatchIf")
                .unwrap()
        } else {
            self._falsy()
        }
    }

    fn build_branch_condition(
        &mut self,
        pattern: &Ast,
        var: &AnyValueEnum<'ctx>,
        var_type: &Ttype,
    ) -> MatchConditionResult<'ctx> {
        // println!("build branch {:?} -> {:?} [{:?}]", pattern, var, var_type);
        match pattern {
            Ast::Int8(_i8) => self.eq(pattern, var, var_type),
            Ast::Integer(_i64) => self.eq(pattern, var, var_type),
            Ast::Number(_f64) => self.eq(pattern, var, var_type),
            Ast::Bool(_bool) => self.eq(pattern, var, var_type),
            Ast::List(members, list_type) => self.match_list(members, list_type, var, var_type),
            Ast::Id(x, _t) => {
                self.bind_name(x, var, var_type);
                self._truthy()
            }
            Ast::Unop(tok, expr, _t) => match tok {
                &Token::If => self.branch_subcondition(expr, var, var_type),
                _ => self._falsy(),
            },
            _ => self._falsy(),
        }
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
                    self.env.push();

                    let branch_cond = self.build_branch_condition(pattern_ast, &var, &var_type);

                    branch_cond.set_name(format!("Pattern{}", branch_idx).as_str());

                    let _ = self.builder.build_conditional_branch(
                        branch_cond,
                        *branch_block,
                        *next_block,
                    );
                    self.builder.position_at_end(*branch_block);

                    let branch_codegen = {
                        let mut c = None;
                        for v in branch_ast {
                            c = self.codegen(v);
                        }
                        c
                    }?;

                    self.env.pop();

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
                    self.env.push();

                    self.build_branch_condition(pattern_ast, &var, &var_type);
                    self.builder.position_at_end(*default_block);

                    let branch_codegen = self.codegen_block(branch_ast)?;
                    self.env.pop();
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
        println!("return match codegen {:?}\n", phi);

        Some(phi.as_any_value_enum())
    }
}
