use crate::{parser::Ast, symbols::Ttype};

pub struct Constraint {
    pub lhs: Ttype,
    pub rhs: Ttype,
}
pub struct ConstraintGenerator {
    pub constraints: Vec<Constraint>,
}
impl ConstraintGenerator {
    pub fn new() -> Self {
        Self {}
    }
    pub fn generate_constraints(&mut self, ast: &mut Ast) {}
}
