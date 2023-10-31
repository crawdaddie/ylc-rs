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
pub fn unify_types(constraints: Vec<Constraint>, subs: &mut Substitutions) -> Substitutions {
    if constraints.is_empty() {
        return subs.clone();
    }

    let (first, rest) = constraints.split_at(1);
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

    unify_types(rest.to_vec(), subs)
}
#[cfg(test)]
mod tests {
    use crate::symbols::Numeric;

    use super::*;
    #[test]
    fn test_unify_constraints() {
        let tests: Vec<(Vec<Constraint>, Vec<(String, Ttype)>)> = vec![(
            vec![
                Constraint {
                    lhs: Ttype::tvar("t7"),
                    rhs: Ttype::tvar("t2"),
                },
                Constraint {
                    lhs: Ttype::tvar("t8"),
                    rhs: Ttype::tvar("t3"),
                },
                Constraint {
                    lhs: Ttype::tvar("t6"),
                    rhs: Ttype::Bool,
                },
                Constraint {
                    lhs: Ttype::tvar("t6"),
                    rhs: Ttype::Bool,
                },
                Constraint {
                    lhs: Ttype::tvar("t11"),
                    rhs: Ttype::tvar("t2"),
                },
                Constraint {
                    lhs: Ttype::tvar("t9"),
                    rhs: Ttype::MaxNumeric(
                        Box::new(Ttype::Numeric(Numeric::Int)),
                        Box::new(Ttype::tvar("t11")),
                    ),
                },
                Constraint {
                    lhs: Ttype::tvar("t5"),
                    rhs: Ttype::tvar("t9"),
                },
                Constraint {
                    lhs: Ttype::tvar("t13"),
                    rhs: Ttype::tvar("t3"),
                },
                Constraint {
                    lhs: Ttype::tvar("t14"),
                    rhs: Ttype::tvar("t4"),
                },
                Constraint {
                    lhs: Ttype::tvar("t12"),
                    rhs: Ttype::MaxNumeric(
                        Box::new(Ttype::tvar("t13")),
                        Box::new(Ttype::tvar("t14")),
                    ),
                },
                Constraint {
                    lhs: Ttype::tvar("t5"),
                    rhs: Ttype::tvar("t12"),
                },
                Constraint {
                    lhs: Ttype::tvar("t1"),
                    rhs: Ttype::Fn(vec![
                        Ttype::tvar("t2"),
                        Ttype::tvar("t3"),
                        Ttype::tvar("t4"),
                        Ttype::tvar("t5"),
                    ]),
                },
            ],
            // **
            // Constraint { lhs: 't7, rhs: 't2 }
            // Constraint { lhs: 't8, rhs: 't3 }
            // Constraint { lhs: 't6, rhs: Bool }
            // Constraint { lhs: 't6, rhs: Bool }
            // Constraint { lhs: 't11, rhs: 't2 }
            // Constraint { lhs: 't9, rhs: MaxNumeric(Int, 't11) }
            // Constraint { lhs: 't5, rhs: 't9 }
            // Constraint { lhs: 't13, rhs: 't3 }
            // Constraint { lhs: 't14, rhs: 't4 }
            // Constraint { lhs: 't12, rhs: MaxNumeric('t13, 't14) }
            // Constraint { lhs: 't5, rhs: 't12 }
            // Constraint { lhs: 't1, rhs: 't2->'t3->'t4->'t5 }
            vec![
                ("t1".into(), Ttype::tvar("")),
                ("t2".into(), Ttype::tvar("")),
                ("t3".into(), Ttype::tvar("")),
                ("t4".into(), Ttype::tvar("")),
                ("t5".into(), Ttype::tvar("")),
                ("t6".into(), Ttype::tvar("")),
            ],
        )];
        for (cons, expect) in tests {
            let subs = unify_types(cons, &mut Substitutions::new());
            assert_eq!(subs, expect.into_iter().collect());
        }
    }
}
