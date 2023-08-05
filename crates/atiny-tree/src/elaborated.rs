//! This module defines a tree that contains semantic information. It's widely used for code
//! generation and some expansions.

use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
#[derive(Debug, Clone)]
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
    Let(CaseTreeNode, Expr<T>),
    Expr(Expr<T>),
}

#[derive(Debug)]
pub enum Expr<T> {
    Number(u64),
    Tuple(Vec<Self>),

    Variable(VariableNode<T>),
    Function(VariableNode<T>),
    Constructor(VariableNode<T>),

    CaseTree(Box<Self>, CaseTree<T>),

    Abstraction(VecDeque<Labeled<T>>, Box<Self>, T),
    Application(Box<Self>, Vec<Self>, T),

    RecordCreation(Symbol, Vec<Labeled<Self>>),
    RecordUpdate(Box<Self>, Vec<Labeled<Self>>),
    RecordField(Symbol, Symbol, Box<Self>),

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
    pub places: Vec<Expr<T>>,
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
pub enum CaseTreeNode {
    Node(Vec<Switch>),
    Leaf(usize),
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
        }
        Ok(())
    }
}

impl Display for CaseTreeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render_indented(f, 0)
    }
}
