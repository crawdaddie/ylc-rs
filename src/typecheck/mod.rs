use crate::{
    parser::{Ast, Program},
    symbols::Ttype,
    typecheck::unify::{lookup_contained_types, Substitutions},
};

mod constraints;
mod unify;

use constraints::ConstraintGenerator;
use unify::unify_constraints;

fn apply_substitution(t: &mut Ttype, subs: &Substitutions) {
    if let Ttype::Var(type_name) = t {
        if let Some(subs_type) = subs.get(type_name) {
            *t = lookup_contained_types(subs_type.clone(), subs);
        }
    }
}
fn update_callable(ast: &mut Ast, params: &mut Vec<Ast>) {
    match ast {
        Ast::Id(callable, ttype) if ttype.is_generic() => {
            println!(
                "update generic callable {:?} {:?} {:?}",
                callable, ttype, params
            );
            *ttype =
                ttype.transform_generic(params.iter().map(|p| p.get_ttype().unwrap()).collect())
        }
        _ => {}
    }
}

fn update_types(ast: &mut Ast, subs: &Substitutions) {
    match ast {
        Ast::Let(_id, _type_expr, Some(value)) => update_types(&mut *value, subs),
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
            // update_callable(&mut *callee_box, params_vec);
            // println!("update {:?} {:?}", callee_box, ttype);
            // if let Ast::Id(callable, ttype) = *callee_box {}
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

    // for c in &cg.constraints {
    //     println!("{:?}", c);
    // }
    let subs = unify_constraints(cg.constraints, &mut Substitutions::new());
    println!("\x1b[1;31m");
    println!("substitutions\n----------");
    for (k, v) in subs.iter() {
        println!("{}:{:?}", k, v);
    }
    println!("\x1b[1;0m");

    for e in expr {
        update_types(e, &subs);
    }
}
#[cfg(test)]
mod tests {
    use crate::{int_expr, lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn curried_function() {
        let input = r#"
        let f = fn (a, b, c, d) {
            a + b + c + d
        }
        f(1, 2)
        "#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        infer_types(&mut program);

        if let Ast::Call(fn_id, args, ttype) = program[1].clone() {
            let mut fn_types = vec![];
            if let Ast::Id(_, fn_type) = *fn_id {
                if let Ttype::Fn(fn_types_vec) = fn_type {
                    fn_types = fn_types_vec;
                } else {
                    panic!()
                }
            };
            if fn_types.is_empty() {
                panic!()
            }
            assert!(fn_types.len() == 5);
            assert_eq!(ttype, Ttype::Fn(fn_types[2..].into()));
            assert_eq!(args, vec![int_expr!(1), int_expr!(2)]);
        }
    }

    #[test]
    fn generic_function() {
        let input = r#"
        let f = fn (a) {
            a + 1
        }
        f(1)
        "#;

        let mut parser = Parser::new(Lexer::new(input.into()));
        let mut program = parser.parse_program();
        infer_types(&mut program);

        if let Ast::Call(fn_id, args, Ttype::Application(_, app_types, _)) = program[1].clone() {
            let mut fn_types = vec![];
            if let Ast::Id(_, fn_type) = *fn_id {
                let transformed_fn_type = fn_type.transform_generic(app_types);
                println!("call type {:?}", transformed_fn_type.fn_return().unwrap());
                if let Ttype::Fn(fn_types_vec) = fn_type {
                    fn_types = fn_types_vec;
                } else {
                    panic!()
                }
            };
            if fn_types.is_empty() {
                panic!()
            }
            assert!(false);
        }
    }
}
