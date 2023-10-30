use std::{collections::HashMap, fmt};

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
                _ => Ok(()),
            },
            Ttype::MaxNumeric(type1, type2) => {
                write!(f, "MaxNumeric({:?}, {:?})", type1, type2)
            }
            Ttype::Str => write!(f, "Str"),
            Ttype::Bool => write!(f, "Bool"),
            Ttype::Tuple(types) => write!(f, "Ttype::Tuple({:?})", types),
            Ttype::Struct => write!(f, "Ttype::Struct"),
            Ttype::Void => write!(f, "Ttype::Void"),
            Ttype::Fn(params) => {
                let mut r = write!(f, "{:?}", params[0]);
                for pidx in 1..params.len() {
                    r = write!(f, "->");
                    r = write!(f, "{:?}", params[pidx]);
                }
                r
            }
            Ttype::Ptr => write!(f, "Ttype::Ptr"),
            Ttype::Array(inner_type) => write!(f, "Ttype::Array({:?})", inner_type),
            Ttype::Uptr => write!(f, "Ttype::Uptr"),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum SymbolValue {
    TypeDecl,
    Function(Ttype),
    // RecursiveRef(Ttype),
    Variable(Ttype),
}

type StackFrame = HashMap<String, SymbolValue>;

#[derive(Debug)]
pub struct Env {
    stack: Vec<StackFrame>,
}

impl Env {
    pub fn new() -> Self {
        let stack = vec![];
        Self { stack }
    }
    pub fn push(&mut self) -> Option<&mut StackFrame> {
        let frame = HashMap::new();
        self.stack.push(frame);
        self.stack.last_mut()
    }

    pub fn pop(&mut self) -> Option<&mut StackFrame> {
        self.stack.pop();
        self.stack.last_mut()
    }
    pub fn current(&mut self) -> Option<&mut StackFrame> {
        self.stack.last_mut()
    }

    pub fn bind_symbol(&mut self, name: String, value: SymbolValue) {
        if let Some(frame) = self.current() {
            frame.insert(name, value);
        }
    }
    pub fn lookup(&self, name: String) -> Option<&SymbolValue> {
        for frame in self.stack.iter().rev() {
            let x = frame.get(&name);
            if x.is_some() {
                return x;
            }
        }
        None
    }
}
