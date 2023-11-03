use super::constraints::Constraint;
use crate::symbols::Ttype;
use std::collections::HashMap;

pub type Substitutions = HashMap<String, Ttype>;
pub fn lookup_contained_types(t: Ttype, subs: &Substitutions) -> Ttype {
    match &t {
        Ttype::Var(t_name) => {
            if let Some(t_lookup) = subs.get(&t_name.clone()) {
                t_lookup.clone()
            } else {
                t
            }
        }
        Ttype::MaxNumeric(ref x, ref y) => {
            // resolve a max numeric type into the max of two types if possible
            let xresolved = lookup_contained_types(*x.clone(), subs);
            let yresolved = lookup_contained_types(*y.clone(), subs);
            match (xresolved, yresolved) {
                (Ttype::Numeric(xnum), Ttype::Numeric(ynum)) => {
                    Ttype::Numeric(if xnum >= ynum { xnum } else { ynum })
                }
                _ => Ttype::MaxNumeric(
                    Box::new(lookup_contained_types(*x.clone(), subs).clone()),
                    Box::new(lookup_contained_types(*y.clone(), subs).clone()),
                ),
            }
        }
        Ttype::Application(fn_name, args) => {
            let new_args = args
                .iter()
                .map(|v| lookup_contained_types(v.clone(), subs))
                .collect();
            Ttype::Application(fn_name.clone(), new_args)
        }

        Ttype::Fn(fn_components) => Ttype::Fn(
            fn_components
                .iter()
                .map(|v| lookup_contained_types(v.clone(), subs))
                .collect(),
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
/// Unifies variable v with term x, using subs.
/// Returns updated subs or None on failure.
/// The algorithm begins with the set of all constraints, and the empty substitution.
/// Each constraint is considered once and removed from the set.
/// As constraints are disposed, the substitution set tends to grow.
/// When all constraints have been disposed, unification returns the final substitution set.
/// For a given constraint, the unifier examines the left-hand-side of the equation.
/// If it is a variable (Ttype::Var), it is now ripe for elimination.
/// The unifier adds the variable’s right-hand-side to the
/// substitution and, to truly eliminate it, replaces all occurrences of the variable in the
/// substitution with the right-hand-side. In practice this needs to be implemented
/// efficiently; for instance, using a mutational representation of these variables can avoid having
/// to search-and-replace all occurrences.
/// We said that the unifier eliminates the variable by replacing all instances of it in the
/// substitution.
/// However, that assumes that the right-hand-side does not contain any instances of
/// the same variable.
/// Otherwise we have a circular definition, and it becomes impossible to perform this particular substitution.
/// For this reason, unifiers include an occurs check: a check for
/// whether the same variable occurs on both sides and, if it does, decline to unify.
///
pub fn unify(lhs: &Ttype, rhs: &Ttype, subs: &mut Substitutions) {
    // println!("unify {:?}::{:?}\n{:?}", lhs, rhs, subs);
    match (lhs, rhs) {
        (Ttype::Var(v1), Ttype::Var(v2)) if v1 == v2 => {}
        (Ttype::Var(v1), Ttype::Var(v2)) => {
            if let Some(v2_follow) = subs.clone().get(v2) {
                unify(lhs, v2_follow, subs);
            } else {
                subs.insert(v1.into(), rhs.clone());
            }
        }
        // (Ttype::Var(v), Ttype::MaxNumeric(w, x)) => {
        //     w = subs.get(w.var_name().unwrap().into());
        //     subs.insert(v.clone(), *w.clone());
        // }
        (Ttype::Var(v), t) => {
            let lookup = lookup_contained_types(t.clone(), subs);
            if occurs(v, &lookup, subs) {
                panic!("Occurs check failed");
            }

            subs.insert(v.clone(), lookup);
        }
        (t, Ttype::Var(v)) => {
            let lookup = lookup_contained_types(t.clone(), subs);
            if occurs(v, &lookup, subs) {
                panic!("Occurs check failed");
            }

            subs.insert(v.clone(), lookup);
        }
        (Ttype::Application(c1, args1), Ttype::Application(c2, args2)) if c1 == c2 => {
            for (arg1, arg2) in args1.iter().zip(args2) {
                unify(arg1, arg2, subs);
            }
        }
        (Ttype::Tuple(args1), Ttype::Tuple(args2)) => {
            for (arg1, arg2) in args1.iter().zip(args2) {
                unify(arg1, arg2, subs);
            }
        }
        (Ttype::Numeric(n1), Ttype::Numeric(n2)) => {}
        _ => panic!("Cannot unify types"),
    };
}

fn occurs(l: &str, r: &Ttype, subs: &HashMap<String, Ttype>) -> bool {
    match r {
        Ttype::Var(v2) if l == v2 => true,
        Ttype::Var(v2) => subs.get(v2).map_or(false, |t| occurs(l, t, subs)),
        Ttype::Application(_, args) | Ttype::Tuple(args) => {
            args.iter().any(|arg| occurs(l, arg, subs))
        }
        Ttype::MaxNumeric(u, v) => occurs(l, u, subs) || occurs(l, v, subs),
        // ... other types
        _ => false,
    }
}
pub fn unify_constraints(constraints: Vec<Constraint>, subs: &mut Substitutions) -> Substitutions {
    if constraints.is_empty() {
        return subs.clone();
    }

    let (first, rest) = constraints.split_at(1);
    let l = first[0].lhs.clone();
    let r = first[0].rhs.clone();
    unify(&l, &r, subs);

    unify_constraints(rest.to_vec(), subs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbols::{tbool, tint, tvar, Numeric};

    // use pretty_assertions::assert_eq;

    #[ignore]
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
            // let x = fn (a, b, c) {
            //   if a == b {
            //     2 * a --
            //   } else {
            //     b + c -- t5
            //   }
            // } -->
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
            // Constraint { lhs: 't1, rhs: 't2->'t3->'t4->'t5 } -- fn expr for x
            //
            //FnDeclaration("x",
            //  Fn(
            //      Args:
            //      [
            //          Id("a", 't2),
            //          Id("b", 't3),
            //          Id("c", 't4)
            //      ],
            //
            //      Return Type:
            //      None,
            //
            //      Body:
            //      [If(
            //          Cond:
            //          Binop(Equality, Id("a", 't7), Id("b", 't8), 't6),
            //
            //          Then:
            //          [Binop(Star, Integer(2), Id("a", 't11), 't9)],
            //
            //          Else:
            //          Some([Binop(Plus, Id("b", 't13), Id("c", 't14), 't12)]),
            //
            //      't5)],
            //
            //  't1))
            vec![
                ("t6".into(), Ttype::Bool),
                (
                    "t5".into(),
                    Ttype::MaxNumeric(
                        Box::new(Ttype::Numeric(Numeric::Int)),
                        Box::new(Ttype::tvar("t11")),
                    ),
                ),
                (
                    "t5".into(),
                    Ttype::MaxNumeric(
                        Box::new(Ttype::Numeric(Numeric::Int)),
                        Box::new(Ttype::tvar("t11")),
                    ),
                ),
                ("t3".into(), tvar("")),
                ("t4".into(), tvar("")),
                ("t5".into(), tvar("")),
                ("t6".into(), tvar("")),
            ],
        )];
        for (cons, expect) in tests {
            let subs = unify_constraints(cons, &mut Substitutions::new());
            assert_eq!(subs, expect.into_iter().collect());
        }
    }

    #[test]
    fn simple_constraints() {
        let tests: Vec<(Vec<Constraint>, Vec<(String, Ttype)>)> = vec![
            // λ let a = if b == 1 { (0,1) } else { (2, 3) }
            // Let("a", None, Some(
            //      If(
            //          Binop(Equality, Id("b", 't3), Integer(1), 't2),
            //          [Tuple([Integer(0), Integer(1)], 't5)],
            //          Some([Tuple([Integer(2), Integer(3)], 't8)]),
            //      't1)))
            //
            // Constraint { lhs: 't2, rhs: Bool }
            // Constraint { lhs: 't2, rhs: Bool }
            // Constraint { lhs: 't5, rhs: Tuple([Int, Int]) }
            // Constraint { lhs: 't1, rhs: 't5 }
            // Constraint { lhs: 't8, rhs: Tuple([Int, Int]) }
            // Constraint { lhs: 't1, rhs: 't8 }
            // Constraint { lhs: 't3, rhs: Int }
            //
            // unified substitutions: {
            // "t1": 't5,
            // "t8": Tuple([Int, Int]),
            // "t2": Bool,
            // "t5": Tuple([Int, Int])}
            (
                vec![
                    Constraint {
                        lhs: tvar("t2"),
                        rhs: tbool(),
                    },
                    Constraint {
                        lhs: tvar("t3"),
                        rhs: tint(),
                    },
                    Constraint {
                        lhs: tvar("t5"),
                        rhs: Ttype::Tuple(vec![tint(), tint()]),
                    },
                    Constraint {
                        lhs: tvar("t1"),
                        rhs: tvar("t5"),
                    },
                    Constraint {
                        lhs: tvar("t8"),
                        rhs: Ttype::Tuple(vec![tint(), tint()]),
                    },
                    Constraint {
                        lhs: tvar("t1"),
                        rhs: tvar("t8"),
                    },
                ],
                vec![
                    ("t1".into(), Ttype::Tuple(vec![tint(), tint()])),
                    ("t5".into(), Ttype::Tuple(vec![tint(), tint()])),
                    ("t8".into(), Ttype::Tuple(vec![tint(), tint()])),
                    ("t2".into(), tbool()),
                    ("t3".into(), tint()),
                ],
            ),
        ];
        for (cons, expect) in tests {
            let subs = unify_constraints(cons, &mut Substitutions::new());
            let ex: Substitutions = expect.into_iter().collect();
            assert_eq!(ex, subs);
        }
    }
}
