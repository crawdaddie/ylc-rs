use std::collections::HashMap;

use crate::parser::Ast;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Numeric {
    Int8 = 0, // int8 - alias char
    Int = 1,  // int
    Num = 2,  // double
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
