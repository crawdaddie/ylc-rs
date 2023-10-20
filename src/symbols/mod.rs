use std::collections::HashMap;
#[derive(Hash, Eq, PartialEq, Debug)]
pub enum SymbolValue {
    Type,
    Function,
    Variable,
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
}
