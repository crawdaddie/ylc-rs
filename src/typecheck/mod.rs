use crate::{
    parser::{Ast, Program},
    symbols::Ttype,
    typecheck::unify::Substitutions,
};

mod constraints;
mod unify;

use constraints::ConstraintGenerator;
use unify::unify_constraints;

fn apply_substitution(t: &mut Ttype, subs: &Substitutions) {
    if let Ttype::Var(type_name) = t {
        if let Some(subs_type) = subs.get(type_name) {
            *t = subs_type.clone();
        }
    }
}

fn update_types(ast: &mut Ast, subs: &Substitutions) {
    match ast {
        Ast::Let(_id, _type_expr, value) => {
            if let Some(value) = value {
                update_types(&mut *value, subs)
            }
        }
        Ast::FnDeclaration(_id, fn_expr) => update_types(&mut *fn_expr, subs),
        Ast::TypeDeclaration(_id, _type_expr) => {}
        Ast::Id(_id, ttype) => {
            apply_substitution(ttype, subs);
            // self.id(id.clone(), ttype.clone());
        }
        Ast::Fn(params_vec, _ret_type_ast, body, ttype) => {
            apply_substitution(ttype, subs);
            for p in params_vec {
                update_types(p, subs);
            }
            for s in body {
                update_types(s, subs);
            }
        }

        Ast::Binop(_token, left_box, right_box, ttype) => {
            apply_substitution(ttype, subs);

            // self.binop(token.clone(), &*left_box, &*right_box, ttype.clone());
            update_types(&mut *left_box, subs);
            update_types(&mut *right_box, subs);
        }

        Ast::Unop(_token, operand_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *operand_box, subs);
        }

        Ast::Tuple(exprs_vec, ttype) => {
            apply_substitution(ttype, subs);
            for x in exprs_vec {
                update_types(x, subs);
            }
        }

        Ast::Index(object_box, index_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *object_box, subs);
            update_types(&mut *index_box, subs);
        }

        Ast::Assignment(assignee_box, value_box, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *assignee_box, subs);
            update_types(&mut *value_box, subs);
        }
        Ast::If(condition, then, elze, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *condition, subs);
            for t in then {
                update_types(t, subs);
            }
            if let Some(e) = elze {
                for t in e {
                    update_types(t, subs);
                }
            }
        }
        Ast::Call(callee_box, params_vec, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *callee_box, subs);

            for a in params_vec {
                update_types(a, subs);
            }
        }
        _ => (),
    }
}
pub fn infer_types(expr: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for e in expr.clone() {
        cg.generate_constraints(&e);
    }

    cg.env.pop();

    println!("\x1b[1;31m");
    for c in &cg.constraints {
        println!("{:?}", c);
    }
    println!("\x1b[1;0m");
    let subs = unify_constraints(cg.constraints, &mut Substitutions::new());

    println!("unified substitutions: {:?}", subs);
    for e in expr {
        update_types(e, &subs);
    }
}
