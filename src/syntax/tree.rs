//! This module describes the first tree that is generated from the parser. It's a tree that contains
//! pure syntatic information due the origin. The main types of this file are [Expr], [Pattern] and
//! [TopLevel]

use std::fmt::{self, Display};

use itertools::Itertools;

use crate::location::{Located};

#[derive(Debug)]
pub enum ExprKind {
    Unit,
    Number(u64),
    Boolean(bool),
    Tuple(Vec<Expr>),
    Identifier(String),
    Match(Box<Expr>, Vec<Clause>),
    Abstraction(String, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Annotation(Box<Expr>, Box<Type>),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Self::Identifier(id) => write!(f, "{id}"),
            Self::Abstraction(p, e) => write!(f, "(|{p}| {e})"),
            Self::Annotation(p, e) => write!(f, "({p} : {e})"),
            Self::Application(fu, a) => write!(f, "({fu} {a})"),
            Self::Let(n, v, next) => write!(f, "(let {n} = {v}; {next})"),
            Self::Match(e, c) => write!(f, "{e} {{{}}}", c.iter().join(", ")),
        }
    }
}

impl ExprKind {
    pub fn if_let(pattern: Expr, matcher: Expr, true_arm: Expr, else_arm: Expr) -> Self {
        let else_pat = Expr {
            location: else_arm.location,
            data: ExprKind::Identifier("_".to_string()),
        };

        ExprKind::Match(
            Box::new(matcher),
            vec![
                Clause::new(Pattern(pattern), true_arm),
                Clause::new(Pattern(else_pat), else_arm),
            ],
        )
    }
}

/// An atiny expression is some syntatic element that intrinsically has a value. It can be a literal
/// value, a variable, a function call, a function definition and etc.
pub type Expr = Located<ExprKind>;

/// A pattern is a syntatic element that goes inside pattern match declarations and are used to
/// match on values and destruct them.
#[derive(Debug)]
pub struct Pattern(pub Expr);

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// This is a clause of a pattern match declaration. It contains a pattern and an expression that is
/// the result of the match.
#[derive(Debug)]
pub struct Clause {
    pub pat: Pattern,
    pub expr: Expr,
}

impl Clause {
    pub fn new(pat: Pattern, expr: Expr) -> Self {
        Self { pat, expr }
    }
}

impl Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} => {}", self.pat, self.expr)
    }
}

#[derive(Debug)]
pub struct ArrowNode {
    pub left: Box<Type>,
    pub right: Box<Type>,
}

impl Display for ArrowNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} -> {})", self.left, self.right)
    }
}

#[derive(Debug)]
pub struct VariableNode {
    pub name: String,
}

impl Display for VariableNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    } 
}

#[derive(Debug)]
pub struct ForallNode {
    pub args: Vec<String>,
    pub body: Box<Type>,
} 

impl Display for ForallNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(forall ({}) . {})", self.args.iter().join(" "), self.body)
    }
}

#[derive(Debug)]
pub struct TypeApplicationNode {
    pub left: Box<Type>,
    pub right: Box<Type>,
}

impl Display for TypeApplicationNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.left, self.right)
    }
}

#[derive(Debug)]
pub struct TypeTupleNode {
    pub types: Vec<Type>,
}

impl Display for TypeTupleNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.types.iter().join(", "))
    }
}

#[derive(Debug)]
pub enum TypeKind {
    Arrow(ArrowNode),
    Variable(VariableNode),
    Forall(ForallNode),
    Application(TypeApplicationNode),
    Tuple(TypeTupleNode),
    Unit
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arrow(n) => write!(f, "{}", n),
            Self::Variable(n) => write!(f, "{}", n),
            Self::Forall(n) => write!(f, "{}", n),
            Self::Application(n) => write!(f, "{}", n),
            Self::Tuple(n) => write!(f, "{}", n),
            Self::Unit => write!(f, "()"),
        }
    }
}

pub type Type = Located<TypeKind>;

#[derive(Debug)]
pub struct Constructor {
    pub name: String,
    pub types: Vec<Type>,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub name: String,
    pub types: Vec<String>,
    pub constructors: Vec<Constructor>
}

#[derive(Debug)]
pub struct LetDecl {
    pub name: String,
    pub expr: Box<Expr>
}

#[derive(Debug)]
pub enum TopLevelKind {
    TypeDecl(TypeDecl),
    LetDecl(LetDecl)
}

/// It's a declaration on the top level of the program. It can be a function definition, a type
/// definition, a trait definition, record definition and etc.
pub type TopLevel = Located<TopLevelKind>;
