use std::collections::HashMap;

use crate::symbols::Ttype;

use super::constraints::Constraint;
// #[derive(Clone)]
// pub struct Substitutions(HashMap<String, Ttype>);
// impl Substitutions {
//     pub fn insert(&mut self, k: String, v: Ttype) {
//         self.0.insert(k, v);
//     }
//     pub fn get(&self, k: String) -> Option<Ttype> {
//         while
//         self.get(k)
//     }
// }
pub type Substitutions = HashMap<String, Ttype>;
fn unify_var(left: Ttype, right: Ttype, subs: &mut Substitutions) {
    if let Ttype::Var(lname) = left {
        if subs.contains_key::<String>(&lname) {}
    }
}

/// Unifies variable v with term x, using subs.
/// Returns updated subs or None on failure.
pub fn unify_types(constraints: &mut Vec<Constraint>, subs: &mut Substitutions) -> Substitutions {
    if constraints.is_empty() {
        return subs.clone();
    }

    let (first, rest) = constraints.split_at_mut(1);
    let l = first[0].lhs.clone();
    let r = first[0].rhs.clone();
    match (&l, &r) {
        (Ttype::Var(lname), _) => unify_var(l, r, subs),
        (_, Ttype::Var(rname)) => unify_var(r, l, subs),
        // Ttype::Var(
        // Ttype::Var(var) => {
        //     if let Some(s) = subs.get(var.into()) {}
        //     subs.insert(var.into(), first[0].rhs.clone());
        // }
        _ => {}
    }

    unify_types(&mut rest.to_vec(), subs)
}
