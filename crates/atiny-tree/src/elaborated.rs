//! This module defines a tree that contains semantic information. It's widely used for code
//! generation and some expansions.

use std::collections::VecDeque;
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

#[derive(Debug)]
pub struct CaseTree<T> {
    pub places: Vec<Expr<T>>,
    pub tree: CaseTreeNode,
}

#[derive(Debug)]
pub enum CaseTreeNode {
    Node(Vec<Labeled<CaseTreeNode>>),
    Leaf(usize),
}

impl CaseTreeNode {
    pub fn render_indented(&self, f: &mut Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            Self::Node(vec) => {
                for (name, tree) in vec {
                    writeln!(f, "{:indent$}{name}:", "")?;
                    tree.render_indented(f, indent + 2)?;
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
