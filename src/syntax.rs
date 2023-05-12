use std::fmt::{self, Display};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Byte(pub usize);

impl Byte {
    pub fn locate(&self, code: &str) -> Point {
        let mut acc = 0;
        for (line, code_line) in code.lines().enumerate() {
            if acc + code_line.len() + 1 > self.0 {
                return Point {
                    line,
                    column: self.0 - acc,
                };
            }
            acc += code_line.len() + 1;
        }
        Point::default()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ByteRange(pub Byte, pub Byte);

impl ByteRange {
    pub fn locate(&self, code: &str) -> Range {
        Range(self.0.locate(code), self.1.locate(code))
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct Point {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Range(pub Point, pub Point);

impl Range {
    pub fn new(start: usize, end: usize, code: &str) -> Range {
        Range(Byte(start).locate(code), Byte(end).locate(code))
    }

    pub fn singleton(byte: usize, code: &str) -> Range {
        let point = Byte(byte).locate(code);
        Range(point, point)
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 != self.1 {
            write!(f, "{}~{}", self.0, self.1)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

#[derive(Debug)]
pub enum Item {
    Number(u64),
    Identifier(String),
    Abstraction(String, Box<Syntax>),
    Application(Box<Syntax>, Box<Syntax>),
    Let(String, Box<Syntax>, Box<Syntax>),
}

#[derive(Debug)]
pub struct Syntax {
    pub location: ByteRange,
    pub data: Item,
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Number(n) => write!(f, "{n}"),
            Item::Identifier(id) => write!(f, "{id}"),
            Item::Abstraction(p, e) => write!(f, "(|{p}| {e})"),
            Item::Application(fu, a) => write!(f, "({fu} {a})"),
            Item::Let(n, v, next) => write!(f, "(let {n} = {v}; {next})"),
        }
    }
}

impl fmt::Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}
