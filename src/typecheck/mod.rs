use crate::parser::Program;

mod constraints;
mod unify;

use constraints::ConstraintGenerator;
use unify::unify_types;

pub fn infer_types(expr: &mut Program) {
    let mut cg = ConstraintGenerator::new();
    for mut e in expr {
        cg.generate_constraints(&mut e);
    }

    // println!("Constraints:");
    // for c in &cg.constraints {
    //     println!("{:?}", c);
    // }
    // println!("");
    // println!("{:?}", cg.env);
    cg.env.pop();

    unify_types(cg.constraints);
}
