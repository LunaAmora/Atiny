use std::fmt::{self, Display};

use itertools::Itertools;

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
    pub fn new(start: usize, end: usize, code: &str) -> Self {
        Self(Byte(start).locate(code), Byte(end).locate(code))
    }

    pub fn singleton(byte: usize, code: &str) -> Self {
        let point = Byte(byte).locate(code);
        Self(point, point)
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
    Unit,
    Number(u64),
    Boolean(bool),
    Tuple(Vec<Syntax>),
    Identifier(String),
    Match(Box<Syntax>, Vec<Clause>),
    Abstraction(String, Box<Syntax>),
    Application(Box<Syntax>, Box<Syntax>),
    Let(String, Box<Syntax>, Box<Syntax>),
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Self::Identifier(id) => write!(f, "{id}"),
            Self::Abstraction(p, e) => write!(f, "(|{p}| {e})"),
            Self::Application(fu, a) => write!(f, "({fu} {a})"),
            Self::Let(n, v, next) => write!(f, "(let {n} = {v}; {next})"),
            Self::Match(e, c) => write!(f, "{e} {{{}}}", c.iter().join(", ")),
        }
    }
}

impl Item {
    pub fn if_let(pattern: Syntax, matcher: Syntax, true_arm: Syntax, else_arm: Syntax) -> Self {
        let else_pat = Syntax {
            location: else_arm.location,
            data: Item::Identifier("_".to_string()),
        };

        Item::Match(
            Box::new(matcher),
            vec![
                Clause::new(Pattern(pattern), true_arm),
                Clause::new(Pattern(else_pat), else_arm),
            ],
        )
    }
}

#[derive(Debug)]
pub struct Syntax {
    pub location: ByteRange,
    pub data: Item,
}

impl Display for Syntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug)]
pub struct Pattern(pub Syntax);

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct Clause {
    pub pat: Pattern,
    pub expr: Syntax,
}

impl Clause {
    pub fn new(pat: Pattern, expr: Syntax) -> Self {
        Self { pat, expr }
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} => {}", self.pat, self.expr)
    }
}
