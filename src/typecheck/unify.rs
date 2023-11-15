use std::collections::HashMap;

use crate::symbols::Ttype;

use super::constraints::Constraint;

pub type Substitutions = HashMap<Ttype, Ttype>;

/// does the type 'tl occur in the type r
/// if r == tl -> trul
/// if r is compound type check each contained type
fn occurs(l: &Ttype, r: &Ttype, subs: &Substitutions) -> bool {
    match r {
        Ttype::Var(v2) if l == r => true,
        Ttype::Var(v2) => subs.get(&l).map_or(false, |t| occurs(l, t, subs)),
        Ttype::Fn(f) | Ttype::Tuple(f) => f.iter().any(|p| occurs(l, p, subs)),
        Ttype::Array(t) => occurs(l, t, subs),
        _ => false,
    }
}
pub fn unify(lhs: &Ttype, rhs: &Ttype, subs: &mut Substitutions) {
    match (lhs, rhs) {
        (Ttype::Var(_), r) => {
            if !occurs(lhs, r, subs) {
                subs.insert(lhs.clone(), r.clone());
            }
        }

        (l, Ttype::Var(_)) => {
            if !occurs(rhs, l, subs) {
                subs.insert(rhs.clone(), l.clone());
            }
        }

        (Ttype::Fn(v1), Ttype::Fn(v2)) | (Ttype::Tuple(v1), Ttype::Tuple(v2)) => {
            for (l, r) in v1.iter().zip(v2) {
                unify(l, r, subs);
            }
        }

        _ => {}
    }
}

pub fn unify_constraints(constraints: Vec<Constraint>, subs: &mut Substitutions) {
    for c in constraints {
        let l = c.lhs;
        let r = c.rhs;
        unify(&l, &r, subs);
    }
}

#[cfg(test)]
mod tests {
    use crate::symbols::tvar;

    use super::*;
    #[test]
    fn unif_0() {
        let mut subs: Substitutions = HashMap::from([]);
        unify(&tvar("t0"), &tvar("t1"), &mut subs);
        assert_eq!(subs, HashMap::from([(tvar("t0"), tvar("t1"))]))
    }

    #[ignore]
    #[test]
    fn unif_1() {
        let mut subs = HashMap::from([(tvar("t1"), tvar("t2")), (tvar("t2"), tvar("t3"))]);
        unify(&tvar("t0"), &tvar("t1"), &mut subs);
        assert_eq!(subs, HashMap::from([(tvar("t0"), tvar("t1"))]))
    }
}
