use crate::{parser::Program, typecheck::constraints::ConstraintGenerator};
mod constraints;
mod unify;

pub fn infer_types(program: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for e in program {
        cg.generate_constraints(e);
    }

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
