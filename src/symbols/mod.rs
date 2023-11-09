use std::{collections::HashMap, fmt};

use inkwell::values::FunctionValue;

use crate::parser::Ast;

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
    Numeric(Numeric),
    MaxNumeric(Box<Ttype>, Box<Ttype>),
    Str,               // str
    Bool,              // bool
    Tuple(Vec<Ttype>), // struct or tuple
    Struct,            // struct or tuple
    Void,              // void
    Fn(Vec<Ttype>),    // t1 -> t2 -> ... -> return_type
    Ptr,               // &'x
    Array(Box<Ttype>), // 't[n]
    Uptr,
    Var(String), // 'x
    Application(String, Vec<Ttype>, Box<Ttype>),
    Nth(usize, Box<Ttype>),
}

impl Ttype {
    pub fn is_var(&self) -> bool {
        matches!(self, Ttype::Var(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Ttype::Numeric(Numeric::Num))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Ttype::Numeric(Numeric::Int))
    }

    pub fn is_fn(&self) -> bool {
        matches!(self, Ttype::Fn(_))
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

            Ttype::MaxNumeric(t1, t2) => {
                max_numeric_type(t1.substitute(mem, sub), t2.substitute(mem, sub)).unwrap()
            }
            Ttype::Array(t) => Ttype::Array(Box::new(t.substitute(mem, sub))),
            Ttype::Var(_) if self == mem => sub.clone(),
            _ => self.clone(),
        }
    }
    pub fn transform_generic(&self, application_types: Vec<Ttype>) -> Self {
        let mut t = self.clone();
        match self {
            Ttype::Fn(gen_ts) => {
                let mut map: HashMap<&Ttype, &Ttype> = HashMap::new();
                let mut transformed: Vec<Ttype> = vec![];
                for (member, app) in gen_ts.iter().zip(application_types) {
                    if member.is_var() {
                        t = t.substitute(&member, &app);
                    }
                }
                t
            }
            _ => self.clone(),
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

pub fn tvar(n: &str) -> Ttype {
    Ttype::Var(n.into())
}

pub fn tbool() -> Ttype {
    Ttype::Bool
}

impl Ttype {
    pub fn tvar(n: &str) -> Self {
        Ttype::Var(n.into())
    }
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
            Ttype::MaxNumeric(type1, type2) => {
                write!(f, "MaxNumeric({:?}, {:?})", type1, type2)
            }
            Ttype::Str => write!(f, "Str"),
            Ttype::Bool => write!(f, "Bool"),
            Ttype::Tuple(types) => write!(f, "Tuple({:?})", types),
            Ttype::Struct => write!(f, "Struct"),
            Ttype::Void => write!(f, "Void"),
            Ttype::Fn(params) => {
                write!(f, "{:?}", params[0]);
                for p in params.iter().skip(1) {
                    write!(f, "->");
                    write!(f, "{:?}", p);
                }
                Ok(())
            }
            Ttype::Ptr => write!(f, "Ttype::Ptr"),
            Ttype::Array(inner_type) => write!(f, "Ttype::Array({:?})", inner_type),
            Ttype::Uptr => write!(f, "Ttype::Uptr"),
            Ttype::Nth(i, t) => write!(f, "{:?}[{}]", t, i),
            Ttype::Application(n, args, _) => write!(f, "apply({:?} ({:?}))", n, args),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum TypecheckSymbol {
    TypeDecl,
    Function(Ttype),
    Variable(Ttype),
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Symbol {
    TypeDecl,
    Function(Ttype),
    Variable(Ttype),
    FnParam(u32, Ttype),
    RecursiveRef,
}

type StackFrame<T> = HashMap<String, T>;

#[derive(Debug)]
pub struct Env<T> {
    stack: Vec<StackFrame<T>>,
}
impl Env<TypecheckSymbol> {
    pub fn new() -> Self {
        let stack: Vec<StackFrame<TypecheckSymbol>> = vec![];
        Self { stack }
    }

    pub fn push(&mut self) -> Option<&mut StackFrame<TypecheckSymbol>> {
        let frame = HashMap::new();
        self.stack.push(frame);
        self.stack.last_mut()
    }

    pub fn pop(&mut self) -> Option<&mut StackFrame<TypecheckSymbol>> {
        self.stack.pop();
        self.stack.last_mut()
    }
    pub fn current(&mut self) -> Option<&mut StackFrame<TypecheckSymbol>> {
        self.stack.last_mut()
    }

    pub fn bind_symbol(&mut self, name: String, value: TypecheckSymbol) {
        if let Some(frame) = self.current() {
            frame.insert(name, value);
        }
    }
    pub fn lookup(&self, name: String) -> Option<&TypecheckSymbol> {
        for frame in self.stack.iter().rev() {
            let x = frame.get(&name);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

impl Env<Symbol> {
    pub fn new() -> Self {
        let stack: Vec<StackFrame<Symbol>> = vec![];
        Self { stack }
    }

    pub fn push(&mut self) -> Option<&mut StackFrame<Symbol>> {
        let frame = HashMap::new();
        self.stack.push(frame);
        self.stack.last_mut()
    }

    pub fn pop(&mut self) -> Option<&mut StackFrame<Symbol>> {
        self.stack.pop();
        self.stack.last_mut()
    }
    pub fn current(&mut self) -> Option<&mut StackFrame<Symbol>> {
        self.stack.last_mut()
    }

    pub fn bind_symbol(&mut self, name: String, value: Symbol) {
        if let Some(frame) = self.current() {
            frame.insert(name, value);
        }
    }
    pub fn lookup(&self, name: String) -> Option<&Symbol> {
        for frame in self.stack.iter().rev() {
            let x = frame.get(&name);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}

pub fn max_numeric_type(l: Ttype, r: Ttype) -> Option<Ttype> {
    match (&l, &r) {
        (Ttype::Numeric(lnum), rnum) => match rnum {
            Ttype::Numeric(rnum) => Some(Ttype::Numeric(if rnum >= lnum { *rnum } else { *lnum })),
            Ttype::Var(_) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
            _ => None,
        },

        (lnum, Ttype::Numeric(rnum)) => match lnum {
            Ttype::Numeric(lnum) => Some(Ttype::Numeric(if rnum >= lnum { *rnum } else { *lnum })),
            Ttype::Var(_) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
            _ => None,
        },
        (Ttype::Var(_), Ttype::Var(_)) => Some(Ttype::MaxNumeric(Box::new(l), Box::new(r))),
        _ => None,
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn generic_type_transformation() {
        let generic_fn_type = Ttype::Fn(vec![
            tvar("t0"),
            Ttype::MaxNumeric(Box::new(tvar("t0")), Box::new(tint())),
        ]);
        let application_types = vec![tint()];

        assert_eq!(
            generic_fn_type.transform_generic(application_types),
            Ttype::Fn(vec![tint(), tint(),])
        );
    }

    #[test]
    fn generic_type_transformation_more_complex() {
        let generic_fn_type = Ttype::Fn(vec![
            tvar("t0"),
            tvar("t1"),
            Ttype::Tuple(vec![
                tvar("t0"),
                tint(),
                Ttype::Tuple(vec![tint(), tvar("t1")]),
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
