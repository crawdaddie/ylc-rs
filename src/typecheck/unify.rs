use super::constraints::Constraint;

pub fn unify_types(
    constraints: &mut Vec<Constraint>,
    subs: &mut Vec<Constraint>,
) -> Vec<Constraint> {
    if constraints.is_empty() {
        return subs.to_vec();
    }

    let (_first, rest) = constraints.split_at_mut(1);
    unify_types(&mut rest.to_vec(), subs)
}
