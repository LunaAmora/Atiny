//! This module defines a tree that contains semantic information. It's widely used for code
//! generation and some expansions.

use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Symbol(pub String);

#[derive(Debug)]
pub struct VariableNode<T> {
    pub inst_types: Vec<T>,
    pub name: Symbol,
}

#[derive(Debug)]
pub enum Stmt<T> {
    Let(Symbol, Expr<T>),
    Expr(Expr<T>),
}

#[derive(Debug)]
pub enum Expr<T> {
    Number(u64),
    Tuple(Vec<Expr<T>>),
    Variable(VariableNode<T>),

    CaseTree(Box<Expr<T>>, CaseTree<T>),

    Abstraction(Vec<Symbol>, Box<Expr<T>>),
    Application(Box<Expr<T>>, Vec<Expr<T>>, T),

    RecordCreation(Symbol, Vec<(Symbol, Expr<T>)>),
    RecordUpdate(Box<Expr<T>>, Vec<(Symbol, Expr<T>)>),
    RecordField(Symbol, Box<Expr<T>>, Symbol),

    Block(Vec<Stmt<T>>),

    Error,
}

#[derive(Debug)]
pub struct CaseTree<T> {
    pub places: Vec<Expr<T>>,
    pub tree: CaseTreeNode,
}

#[derive(Debug)]
pub enum CaseTreeNode {
    Node(Vec<(Symbol, CaseTreeNode)>),
    Leaf(usize),
}

impl CaseTreeNode {
    pub fn render_indented(&self, f: &mut Formatter<'_>, indent: usize) -> std::fmt::Result {
        match self {
            Self::Node(vec) => {
                for (name, tree) in vec {
                    writeln!(f, "{:indent$}{:?}:", "", name, indent = indent)?;
                    tree.render_indented(f, indent + 2)?;
                }
            }
            Self::Leaf(index) => writeln!(f, "{:indent$}{}", "", index, indent = indent)?,
        }
        Ok(())
    }
}

impl Display for CaseTreeNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.render_indented(f, 0)
    }
}

pub struct FnBody<T>(pub Expr<T>);
