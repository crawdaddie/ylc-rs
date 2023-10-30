use super::constraints::Constraint;

pub fn unify_types(constraints: Vec<Constraint>) {
    println!("\x1b[1;31m");
    for c in &constraints {
        println!("{:?}", c);
    }
    println!("\x1b[1;0m");
}
