use std::{collections::HashMap, fmt};

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
    Application(String, Vec<Ttype>),
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
            Ttype::Application(n, args) => write!(f, "apply({:?} ({:?}))", n, args),
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
