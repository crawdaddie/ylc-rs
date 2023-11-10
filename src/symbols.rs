use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Numeric {
    Int8 = 0, // int8 - alias char
    Int = 1,  // int
    Num = 2,  // double
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

type StackFrame<T> = HashMap<String, T>;
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
