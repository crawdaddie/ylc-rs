use super::constraints::Constraint;
use crate::{
    symbols::{max_numeric_types, Ttype},
    typecheck::apply_substitution,
};
use std::{collections::HashMap, iter::zip};

pub type Substitutions = HashMap<Ttype, Ttype>;
pub fn lookup_contained_types(t: Ttype, subs: &Substitutions) -> Ttype {
    // println!("{:?} -- {:?}", t, subs);
    match &t {
        Ttype::Var(_t_name) => {
            if let Some(t_lookup) = subs.get(&t) {
                lookup_contained_types(t_lookup.clone(), subs)
            } else {
                t
            }
        }
        Ttype::MaxNumeric(ts) => {
            let ts = ts
                .iter()
                .map(|t| lookup_contained_types(t.clone(), subs))
                .collect::<Vec<Ttype>>();

            if ts.iter().all(|x| x.is_numeric()) {
                max_numeric_types(ts)
            } else {
                Ttype::MaxNumeric(ts)
            }
        }

        Ttype::Fn(fn_components) => Ttype::Fn(
            fn_components
                .iter()
                .map(|v| lookup_contained_types(v.clone(), subs))
                .collect(),
        ),

        Ttype::List(el_type) => {
            Ttype::List(Box::new(lookup_contained_types((**el_type).clone(), subs)))
        }

        Ttype::Nth(el_type, i) => Ttype::Nth(
            Box::new(lookup_contained_types((**el_type).clone(), subs)),
            *i,
        ),
        Ttype::Tuple(vals) => {
            let new_vals = vals
                .iter()
                .map(|v| lookup_contained_types(v.clone(), subs))
                .collect();
            Ttype::Tuple(new_vals)
        }
        _ => t,
    }
}
impl Ttype {
    fn occurs_in(&self, r: &Ttype, subs: &Substitutions) -> bool {
        match (self, r) {
            (Ttype::Var(_), Ttype::Var(_)) if self == r => true,
            (Ttype::Var(_), Ttype::Var(_)) => {
                if let Some(t) = subs.get(r) {
                    self.occurs_in(t, subs)
                } else {
                    false
                }
            }
            // (Ttype::Var(_), _) | (_, Ttype::Var(_)) => false,
            (Ttype::Numeric(l), Ttype::Numeric(r)) => l == r,

            (Ttype::MaxNumeric(l), Ttype::MaxNumeric(r))
            | (Ttype::Tuple(l), Ttype::Tuple(r))
            | (Ttype::Fn(l), Ttype::Fn(r))
            | (Ttype::Union(l), Ttype::Union(r)) => {
                l.iter().zip(r.iter()).all(|(l, r)| l.occurs_in(r, subs))
            }

            (Ttype::List(l), Ttype::List(r)) => l.occurs_in(r, subs),
            (Ttype::Nth(l, i), Ttype::Nth(r, j)) if i == j => l.occurs_in(r, subs),
            _ => false,
        }
    }

    pub fn substitute(&self, mem: &Ttype, sub: &Ttype) -> Self {
        match self {
            Ttype::Fn(t) => {
                let tt = t.iter().map(|v| v.substitute(mem, sub)).collect();
                Ttype::Fn(tt)
            }

            Ttype::Tuple(t) => {
                let tt = t.iter().map(|v| v.substitute(mem, sub)).collect();
                Ttype::Tuple(tt)
            }

            Ttype::MaxNumeric(ts) => {
                Ttype::MaxNumeric(ts.iter().map(|t| t.substitute(mem, sub)).collect())
            }
            Ttype::List(t) => Ttype::List(Box::new(t.substitute(mem, sub))),
            Ttype::Nth(t, i) => Ttype::Nth(Box::new(t.substitute(mem, sub)), *i),
            Ttype::Var(_) if self == mem => sub.clone(),
            Ttype::Var(_) => sub.clone(),
            _ => self.clone(),
        }
    }

    pub fn transform_generic(&mut self, application_types: Vec<Ttype>) -> Self {
        let mut t = self.clone();
        t.transform_generic_inplace(application_types.clone());
        // println!(
        //     "transformed {:?} to {:?} with {:?}",
        //     self, t, application_types
        // );
        t
    }
    pub fn transform_generic_inplace(&mut self, application_types: Vec<Ttype>) {
        if let Ttype::Fn(gen_ts) = self {
            let mut subs = Substitutions::new();
            for (member, app) in gen_ts.iter().zip(application_types) {
                if member.is_var() {
                    subs.insert(member.clone(), app);
                }
            }
            apply_substitution(self, &subs);
        }
    }
}
pub fn unify(l: &Ttype, r: &Ttype, subs: &mut Substitutions) {
    match (l, r) {
        (Ttype::Fn(l), Ttype::Fn(r)) | (Ttype::Tuple(l), Ttype::Tuple(r)) => {
            for (l, r) in zip(l, r) {
                unify(l, r, subs);
            }
        }
        (Ttype::List(l), Ttype::List(r)) => {
            unify(l, r, subs);
        }

        (Ttype::Var(_l), Ttype::Nth(inner_r, i)) => {
            if let Some(sub) = subs.clone().get(inner_r) {
                match sub {
                    Ttype::List(t) => {
                        unify(l, t, subs);
                    }

                    Ttype::Tuple(ts) => {
                        let idx = *i;
                        if ts.len() < idx + 1 {
                            panic!("idx larger than tuple size")
                        }
                        unify(l, &ts[idx], subs);
                    }
                    _ => {}
                }
            } else {
                subs.insert(l.clone(), r.clone());
            }
        }
        (Ttype::Var(_v), _) => {
            if !l.occurs_in(r, subs) {
                if let Some(sub) = subs.clone().get(r) {
                    // println!("found sub {:?} for l {:?}", sub, l);
                    unify(l, sub, subs)
                } else {
                    // println!("save {:?}::{:?} subs {:?}", l, r, subs);
                    subs.insert(l.clone(), r.clone());
                }
            }
        }
        (_, Ttype::Var(_v)) => {
            if !r.occurs_in(l, subs) {
                if let Some(sub) = subs.clone().get(l) {
                    unify(r, sub, subs)
                } else {
                    // println!("save {:?}::{:?} subs {:?}", l, r, subs);
                    subs.insert(r.clone(), l.clone());
                }
            }
        }

        _ => {}
    }
}

pub fn unify_constraints(constraints: Vec<Constraint>, subs: &mut Substitutions) {
    for (l, r) in constraints {
        unify(&l, &r, subs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::{tbool, tint, tvar};
    pub type Subs<'a> = HashMap<Ttype, &'a Ttype>;

    #[test]
    fn unify_simple() {
        let mut subs: Substitutions = Substitutions::new();
        unify(&tvar("t0"), &tvar("t1"), &mut subs);
        println!("subs: {:?}", subs);
        assert_eq!(subs, vec![(tvar("t0"), tvar("t1"))].into_iter().collect());
    }

    #[ignore]
    #[test]
    fn unify_complex() {
        let cons = vec![
            (tvar("t3"), Ttype::List(Box::new(tvar("t5")))),
            (tvar("t4"), tvar("t1")),
            (tvar("t4"), tint()),
            (tvar("t3"), Ttype::List(Box::new(tvar("t6")))),
            (tvar("t6"), tvar("t4")),
            (tvar("t3"), tvar("t2")),
        ];
        let mut subs: Substitutions = Substitutions::new();
        for (l, r) in cons {
            unify(&l, &r, &mut subs);
        }
        println!("subs: {:?}", subs);
        assert_eq!(
            subs,
            vec![(tvar("t4"), tint()), (tvar("t3"), tvar("t2"))]
                .into_iter()
                .collect()
        );
    }

    #[ignore]
    #[test]
    fn unify_complex_2() {
        let mut subs: Substitutions = Substitutions::new();
        unify(&tvar("t2"), &tvar("t0"), &mut subs);
        unify(&tvar("t3"), &tbool(), &mut subs);
        unify(&tvar("t2"), &tint(), &mut subs);
        unify(&tvar("t3"), &tbool(), &mut subs);
        unify(&tvar("t4"), &tint(), &mut subs);
        unify(&tvar("t4"), &tvar("t6"), &mut subs);
        unify(&tvar("t5"), &tint(), &mut subs);
        unify(&tvar("t5"), &tvar("t6"), &mut subs);
        unify(
            &Ttype::Fn(vec![tvar("t0"), tvar("t1")]),
            &Ttype::Fn(vec![tvar("t0"), tvar("t6")]),
            &mut subs,
        );
        unify(&tvar("t0"), &tvar("t0"), &mut subs);

        println!("subs: {:?}", subs);
        assert_eq!(
            subs,
            vec![(tvar("t4"), tint()), (tvar("t3"), tvar("t2"))]
                .into_iter()
                .collect()
        );
    }
}
