//! This module describes the first tree that is generated from the parser. It's a tree that contains
//! pure syntactic information due the origin. The main types of this file are [Expr], [Pattern] and
//! [TopLevel].

use std::fmt::{self, Display};

use atiny_location::Located;
use itertools::Itertools;

/// Primary expressions that are used both for [Pattern] and [Expr].
#[derive(Debug)]
pub enum AtomKind<T> {
    Unit,
    Number(u64),
    Boolean(bool),
    Tuple(Vec<T>),
    Identifier(String),
    Group(Box<T>),
}

impl<T: Display> Display for AtomKind<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{b}"),
            Self::Tuple(t) => write!(f, "({})", t.iter().join(", ")),
            Self::Identifier(id) => write!(f, "{id}"),
            Self::Group(id) => write!(f, "({id})"),
        }
    }
}

/// Expressions are language constructions that intrinsically contains a return value. E.g
///
/// ```atiny
/// let a = 2; b // b is the return value of the hole expression.
/// ```
///
#[derive(Debug)]
pub enum ExprKind {
    Atom(AtomKind<Expr>),
    Match(Box<Expr>, Vec<Clause>),
    Abstraction(String, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Annotation(Box<Expr>, Box<Type>),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(a) => write!(f, "{}", a),
            Self::Abstraction(p, e) => write!(f, "(|{p}| {e})"),
            Self::Annotation(p, e) => write!(f, "({p} : {e})"),
            Self::Application(fu, a) => write!(f, "({fu} {a})"),
            Self::Let(n, v, next) => write!(f, "(let {n} = {v}; {next})"),
            Self::Match(e, c) => write!(f, "{e} {{{}}}", c.iter().join(", ")),
        }
    }
}

impl ExprKind {
    pub fn if_let(pattern: Pattern, matcher: Expr, true_arm: Expr, else_arm: Expr) -> Self {
        let else_pat = Pattern {
            location: else_arm.location,
            data: PatternKind::Atom(AtomKind::Identifier("_".to_string())),
        };

        Self::Match(
            Box::new(matcher),
            vec![
                Clause::new(pattern, true_arm),
                Clause::new(else_pat, else_arm),
            ],
        )
    }
}

/// An atiny expression is some syntactic element that intrinsically has a value. It can be a literal
/// value, a variable, a function call, a function definition and etc.
pub type Expr = Located<ExprKind>;

/// A pattern is a syntactic element that goes inside pattern match declarations and are used to
/// match on values and destruct them.
#[derive(Debug)]
pub enum PatternKind {
    Atom(AtomKind<Pattern>),
}

impl Display for PatternKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(i) => write!(f, "{}", i),
        }
    }
}

pub type Pattern = Located<PatternKind>;

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

/// Universal quantification as a type
#[derive(Debug)]
pub struct ForallNode {
    pub args: Vec<String>,
    pub body: Box<Type>,
}

impl Display for ForallNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = self.args.iter().join(" ");
        write!(f, "(forall ({}) . {})", args, self.body)
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
    Unit,
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

impl Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "| {} {}",
            self.name,
            self.types.iter().map(|x| format!("({})", x)).join(" ")
        )
    }
}

#[derive(Debug)]
pub struct TypeDecl {
    pub name: String,
    pub params: Vec<String>,
    pub constructors: Vec<Constructor>,
}

impl Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.params.iter().map(|x| format!(" {x}")).join("");
        let constructors = self.constructors.iter().join("");
        write!(f, "(type {}{} = {})", self.name, params, constructors)
    }
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub r#return: Box<Type>,
    pub body: Expr,
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(|(n, t)| format!("({} : {})", n, t))
            .join(" ");

        write!(
            f,
            "(fn {} {} : {} {{{}}})",
            self.name, params, self.r#return, self.body
        )
    }
}

#[derive(Debug)]
pub enum TopLevelKind {
    TypeDecl(TypeDecl),
    FnDecl(FnDecl),
}

/// It's a declaration on the top level of the program. It can be a function definition, a type
/// definition, a trait definition, record definition and etc.
pub type TopLevel = Located<TopLevelKind>;

impl Display for TopLevelKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TopLevelKind::TypeDecl(td) => write!(f, "{}", td),
            TopLevelKind::FnDecl(fd) => write!(f, "{}", fd),
        }
    }
}
