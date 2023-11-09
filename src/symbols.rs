#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Numeric {
    Int8 = 0, // int8 - alias char
    Int = 1,  // int
    Num = 2,  // double
}
#[derive(Debug, Clone, PartialEq)]
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
