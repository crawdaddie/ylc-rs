use core::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Numeric {
    Int8 = 0, // int8 - alias char
    Int = 1,  // int
    Num = 2,  // double
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Ttype {
    Bool,
    Str, // str

    Numeric(Numeric),
    MaxNumeric(Vec<Ttype>), // expression representing maximum of 2 or more numeric types (can
    // contain Variable types

    // compound types
    Tuple(Vec<Ttype>),
    Array(Box<Ttype>),

    Fn(Vec<Ttype>),

    Var(String), // 'x

    Union(Vec<Ttype>),
}

impl fmt::Debug for Ttype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ttype::Var(var) => write!(f, "'{}", var),
            Ttype::Numeric(numeric) => match numeric {
                Numeric::Int8 => write!(f, "Int8"),
                Numeric::Int => write!(f, "Int"),
                Numeric::Num => write!(f, "Num"),
            },
            Ttype::MaxNumeric(v) => {
                write!(f, "Max({:?})", v)
            }
            Ttype::Str => write!(f, "Str"),
            Ttype::Bool => write!(f, "Bool"),
            Ttype::Tuple(types) => write!(f, "Tuple({:?})", types),
            // Ttype::Struct => write!(f, "Struct"),
            // Ttype::Void => write!(f, "Void"),
            Ttype::Fn(params) => {
                let _ = write!(
                    f,
                    "{}",
                    params
                        .iter()
                        .map(|x| format!("{:?}", x).clone())
                        .collect::<Vec<String>>()
                        .join("->")
                );
                Ok(())
            }
            // Ttype::Ptr => write!(f, "Ttype::Ptr"),
            Ttype::Array(inner_type) => write!(f, "Ttype::Array({:?})", inner_type),
            // Ttype::Uptr => write!(f, "Ttype::Uptr"),
            // Ttype::Nth(i, t) => write!(f, "{:?}[{}]", t, i),
            // Ttype::Application(n, args, _) => write!(f, "apply({:?} ({:?}))", n, args),
            Ttype::Union(v) => write!(
                f,
                "{:?}",
                v.iter()
                    .map(|x| format!("{:?}", x).clone())
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
        }
    }
}

impl Ttype {
    pub fn is_var(&self) -> bool {
        matches!(self, Ttype::Var(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Ttype::Numeric(_))
    }
    pub fn is_generic(&self) -> bool {
        match self {
            Ttype::Fn(ts) | Ttype::Tuple(ts) => {
                let mut gen = false;
                for t in ts {
                    if t.is_generic() {
                        gen = true;
                        break;
                    }
                }
                gen
            }

            Ttype::Array(ts) => ts.is_generic(),
            _ => self.is_var(),
        }
    }
    pub fn contained_types(&self) -> Vec<Ttype> {
        match self {
            Ttype::Fn(ts) | Ttype::Tuple(ts) => ts.to_vec(),
            _ => panic!(),
        }
    }
}

pub fn tint8() -> Ttype {
    Ttype::Numeric(Numeric::Int8)
}

pub fn tint() -> Ttype {
    Ttype::Numeric(Numeric::Int)
}

pub fn tnum() -> Ttype {
    Ttype::Numeric(Numeric::Num)
}

pub fn tbool() -> Ttype {
    Ttype::Bool
}
pub fn tvar(s: &str) -> Ttype {
    Ttype::Var(s.into())
}

type StackFrame<T> = HashMap<String, T>;

#[derive(Debug)]
pub struct Env<T> {
    stack: Vec<StackFrame<T>>,
}
pub trait Environment<T> {
    fn push(&mut self) -> Option<&mut StackFrame<T>>;
    fn pop(&mut self) -> Option<&mut StackFrame<T>>;
    fn current(&mut self) -> Option<&mut StackFrame<T>>;
    fn bind_symbol(&mut self, name: String, value: T);
    fn lookup(&self, name: String) -> Option<&T>;
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Symbol {
    TypeDecl,
    Function(Ttype),
    Variable(Ttype),
    FnParam(u32, Ttype),
    RecursiveRef,
}

impl Env<Symbol> {
    pub fn new() -> Self {
        let stack: Vec<StackFrame<Symbol>> = vec![];
        Self { stack }
    }
}

impl Environment<Symbol> for Env<Symbol> {
    fn push(&mut self) -> Option<&mut StackFrame<Symbol>> {
        let frame = HashMap::new();
        self.stack.push(frame);
        self.stack.last_mut()
    }

    fn pop(&mut self) -> Option<&mut StackFrame<Symbol>> {
        self.stack.pop();
        self.stack.last_mut()
    }
    fn current(&mut self) -> Option<&mut StackFrame<Symbol>> {
        self.stack.last_mut()
    }

    fn bind_symbol(&mut self, name: String, value: Symbol) {
        if let Some(frame) = self.current() {
            frame.insert(name, value);
        }
    }
    fn lookup(&self, name: String) -> Option<&Symbol> {
        for frame in self.stack.iter().rev() {
            let x = frame.get(&name);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

pub fn max_numeric_type(l: Ttype, r: Ttype) -> Ttype {
    match (l, r) {
        (Ttype::Numeric(l), Ttype::Numeric(r)) => Ttype::Numeric(if l >= r { l } else { r }),
        (l, r) => Ttype::MaxNumeric(vec![l, r]),
    }
}
/// reduces a MaxNumeric type to either maximum of concrete types + any generics
/// or the maximum of the concrete types
pub fn max_numeric_types(ts: Vec<Ttype>) -> Ttype {
    let (numeric, generic): (Vec<Ttype>, Vec<Ttype>) = ts.into_iter().partition(|x| x.is_numeric());

    let max_numeric = *numeric
        .iter()
        .map(|x| match x {
            Ttype::Numeric(vt) => vt,
            _ => panic!(),
        })
        .max()
        .unwrap();

    if generic.is_empty() {
        Ttype::Numeric(max_numeric)
    } else {
        let mut types = vec![Ttype::Numeric(max_numeric)];

        for g in generic {
            types.push(g)
        }

        Ttype::MaxNumeric(types)
    }
}

// pub fn max_numeric(ts: Ttype) -> Ttype {
//     if let Ttype::MaxNumeric(ts) = ts {
//         let m = max_numeric_types(ts);
//         if m.len() == 1 && m[0].is_numeric() {
//             m[0].clone()
//         } else {
//             Ttype::MaxNumeric(m.clone())
//         }
//     } else {
//         ts
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn max_numeric_() {
        let l = tint();
        let r = tint8();
        assert_eq!(max_numeric_types(vec![l, r]), tint());
    }

    #[test]
    fn max_numeric_var() {
        let l = tint();
        let r = tvar("v");
        assert_eq!(
            max_numeric_types(vec![l, r]),
            Ttype::MaxNumeric(vec![tint(), tvar("v")])
        );
    }

    #[test]
    fn max_numeric_vars() {
        let l = tint();
        let l2 = tnum();
        let r = tvar("v");
        assert_eq!(
            max_numeric_types(vec![l, l2, r]),
            Ttype::MaxNumeric(vec![tnum(), tvar("v")])
        );
    }

    #[test]
    fn generic_type_transformation() {
        let mut generic_fn_type = Ttype::Fn(vec![
            tvar("t0"),
            Ttype::MaxNumeric(vec![tvar("t0"), tint()]),
        ]);
        let application_types = vec![tint()];

        assert_eq!(
            generic_fn_type.transform_generic(application_types),
            Ttype::Fn(vec![tint(), tint()])
        );
    }

    #[test]
    fn generic_type_transformation_more_complex() {
        let mut generic_fn_type = Ttype::Fn(vec![
            tvar("a"),
            tvar("b"),
            Ttype::Tuple(vec![
                tvar("a"),
                tint(),
                Ttype::Tuple(vec![tint(), tvar("b")]),
            ]),
        ]);
        let application_types = vec![tint(), tnum()];

        assert_eq!(
            generic_fn_type.transform_generic(application_types),
            Ttype::Fn(vec![
                tint(),
                tnum(),
                Ttype::Tuple(vec![tint(), tint(), Ttype::Tuple(vec![tint(), tnum()]),]),
            ])
        );
    }
}
