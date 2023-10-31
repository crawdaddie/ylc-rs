use crate::{parser::Program, typecheck::unify::Substitutions};

mod constraints;
mod unify;

use constraints::ConstraintGenerator;
use unify::unify_types;

pub fn infer_types(expr: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for e in expr {
        cg.generate_constraints(e);
    }

    // println!("Constraints:");
    // for c in &cg.constraints {
    //     println!("{:?}", c);
    // }
    // println!("");
    // println!("{:?}", cg.env);
    cg.env.pop();

    println!("\x1b[1;31m");
    for c in &cg.constraints {
        println!("{:?}", c);
    }
    println!("\x1b[1;0m");
    let subs = unify_types(cg.constraints, &mut Substitutions::new());

    println!("unified substitutions: {:?}", subs);
}
