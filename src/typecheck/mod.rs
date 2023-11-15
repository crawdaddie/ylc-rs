use crate::{
    parser::{Ast, Program},
    symbols::Ttype,
    typecheck::constraints::ConstraintGenerator,
};

use self::unify::{unify_constraints, Substitutions};
mod constraints;
mod unify;

fn follow(t: Ttype, subs: &Substitutions) -> Ttype {
    match subs.get(&t) {
        Some(x) => follow(x.clone(), subs),
        None => t,
    }
}
fn apply_substitution(t: &mut Ttype, subs: &Substitutions) {
    if let Ttype::Var(_) = t {
        *t = follow(t.clone(), subs)
    }
}
fn update_types(ast: &mut Ast, subs: &Substitutions) {
    match ast {
        Ast::Let(
            id,
            t,           // optional explicit type parameter
            Some(value), // optional immediate assignment expression
        ) => update_types(value, subs),
        Ast::FnDeclaration(id, fn_expr) => update_types(fn_expr, subs),
        // Ast::TypeDeclaration(id, type_expr) => {}
        Ast::Id(id, ttype) => apply_substitution(ttype, subs),
        Ast::Binop(token, left, right, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *left, subs);
            update_types(&mut *right, subs);
        }
        Ast::Unop(token, operand, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *operand, subs);
        }
        Ast::Tuple(exprs, ttype) => {
            apply_substitution(ttype, subs);
            for e in exprs {
                update_types(&mut *e, subs);
            }
        }
        Ast::Index(obj, idx, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *obj, subs);
            update_types(&mut *idx, subs);
        }
        Ast::Assignment(assignee, val, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *assignee, subs);
            update_types(&mut *val, subs);
        }
        Ast::Fn(params, body, ttype) => {
            apply_substitution(ttype, subs);
            for p in params {
                update_types(p, subs);
            }
            for e in body {
                update_types(e, subs);
            }
        }
        Ast::Call(callable, args, ttype) => {}
        Ast::If(cond, then, elze, ttype) => {
            apply_substitution(ttype, subs);
            update_types(&mut *cond, subs);
            for t in then {
                update_types(t, subs);
            }
            if let Some(e) = elze {
                for t in e {
                    update_types(t, subs);
                }
            }
        }

        // Ast::Int8(i) => {}
        // Ast::Integer(i) => {}
        // Ast::Number(f) => {}
        // Ast::Bool(b) => {}
        // Ast::String(s) => {}
        _ => {}
    }
}
pub fn infer_types(program: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for e in program.clone() {
        cg.generate_constraints(&e);
    }

    let mut subs = Substitutions::new();
    println!("constraints:\n");
    for c in cg.constraints.clone() {
        println!("{:?}", c);
    }
    unify_constraints(cg.constraints, &mut subs);

    println!("\x1b[1;31m");
    println!("substitutions\n----------");
    for (k, v) in subs.iter() {
        println!("{:?}::{:?}", k, v);
    }
    println!("\x1b[1;0m");

    for mut e in program {
        update_types(&mut e, &subs);
    }
}
