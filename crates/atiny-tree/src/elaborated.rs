//! This module defines a tree that contains semantic information. It's widely used for code
//! generation and some expansions.

use atiny_location::Located;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type Labeled<T> = (Symbol, T);

#[derive(Debug, Clone)]
pub struct VariableNode<T> {
    pub name: Symbol,
    pub inst_types: Vec<T>,
}

impl<T> VariableNode<T> {
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            inst_types: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum Stmt<T> {
    Let(CaseTreeNode, Elaborated<T>),
    Expr(Elaborated<T>),
}

pub type Elaborated<T> = Located<Expr<T>>;

#[derive(Debug)]
pub enum Expr<T> {
    Number(u64),
    Tuple(Vec<Elaborated<T>>),

    Variable(VariableNode<T>),
    Function(VariableNode<T>),
    Constructor(VariableNode<T>),

    CaseTree(Box<Elaborated<T>>, CaseTree<T>),

    Abstraction(VecDeque<Labeled<T>>, Box<Elaborated<T>>, T),
    Application(Box<Elaborated<T>>, Vec<Elaborated<T>>, T),

    RecordCreation(Symbol, Vec<Labeled<Elaborated<T>>>),
    RecordUpdate(Box<Elaborated<T>>, Vec<Labeled<Elaborated<T>>>),
    RecordField(Symbol, Symbol, Box<Elaborated<T>>),

    Block(Vec<Stmt<T>>),

    Error,
}

impl<T> Default for Expr<T> {
    fn default() -> Self {
        Self::Error
    }
}

#[derive(Debug, Clone)]
pub enum Accessor<T> {
    Field(T, Symbol, usize),
    Index(T, usize),
}

pub trait AccessorExt<T> {
    fn with_field(self, typ: T, cons: Symbol, field: usize) -> Self;
    fn with_index(self, typ: T, index: usize) -> Self;
}

impl<T> AccessorExt<T> for Vec<Accessor<T>> {
    fn with_field(mut self, typ: T, cons: Symbol, field: usize) -> Self {
        self.push(Accessor::Field(typ, cons, field));
        self
    }

    fn with_index(mut self, typ: T, index: usize) -> Self {
        self.push(Accessor::Index(typ, index));
        self
    }
}

impl<T> Display for Accessor<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field(_, record, field) => write!(f, "({}){}", record, field),
            Self::Index(_, record) => write!(f, "{}", record),
        }
    }
}

#[derive(Debug)]
pub struct CaseTree<T> {
    pub accessors: Vec<HashMap<String, Vec<Accessor<T>>>>,
    pub places: Vec<Elaborated<T>>,
    pub tree: CaseTreeNode,
}

#[derive(Debug)]
pub struct Switch {
    pub var: String,
    pub names: Vec<String>,
    pub cons: String,
    pub tree: CaseTreeNode,
}

#[derive(Debug)]
pub struct Tuple {
    pub var: String,
    pub names: Vec<String>,
    pub tree: CaseTreeNode,
}

#[derive(Debug)]
pub enum CaseTreeNode {
    Node(Vec<Switch>),
    Tuple(Box<Tuple>),
    Leaf(usize),
}

impl Default for CaseTreeNode {
    fn default() -> Self {
        Self::Leaf(0)
    }
}

impl CaseTreeNode {
    pub fn render_indented(&self, f: &mut Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            Self::Node(vec) => {
                for switch in vec {
                    writeln!(
                        f,
                        "{:indent$}{} = {}({}):",
                        "",
                        switch.var,
                        switch.cons,
                        switch.names.join(", ")
                    )?;
                    switch.tree.render_indented(f, indent + 2)?;
                }
            }
            Self::Leaf(index) => writeln!(f, "{:indent$}{index}", "")?,
            Self::Tuple(tuple) => {
                writeln!(
                    f,
                    "{:indent$}({}) = ({})",
                    "",
                    tuple.var,
                    tuple.names.join(", ")
                )?;
                tuple.tree.render_indented(f, indent + 2)?;
            }
        }
        Ok(())
    }
}

impl Display for CaseTreeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render_indented(f, 0)
    }
}
