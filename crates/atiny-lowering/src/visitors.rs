use atiny_checker::program::Program;
use atiny_checker::types::{MonoType, Type};
use atiny_error::{Error, ErrorBuilder};
use atiny_location::{ByteRange, WithLoc};
use atiny_tree::elaborated::{CaseTree, CaseTreeNode, Elaborated, Expr, Symbol, VariableNode};
use indexmap::IndexMap;
use std::collections::VecDeque;

use crate::walkable::Walkable;

pub trait Visitor: Sized {
    fn visit_symbol(&mut self, _: &mut Symbol) {}
    fn visit_number(&mut self, _: &mut u64) {}
    fn visit_type(&mut self, _: &mut Type) {}

    fn visit_variable(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}
    fn visit_function(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}
    fn visit_constructor(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}

    fn visit_application(&mut self, _: &mut Elaborated<Type>) {}
    fn visit_abstraction(&mut self, _: &mut Elaborated<Type>) {}

    fn visit_case_tree(&mut self, case_tree: &mut CaseTree<Type>) {
        let CaseTree {
            accessors: _,
            places,
            tree,
        } = case_tree;

        for place in places.iter_mut() {
            place.walk(self);
        }

        tree.walk(self);
    }

    fn visit_case_tree_node(&mut self, _: &mut CaseTreeNode) {}
}

pub struct PartialAppRemover;

impl Visitor for PartialAppRemover {
    fn visit_application(&mut self, expr: &mut Elaborated<Type>) {
        let loc = expr.location;
        let Expr::Application(_, args, typ) = &mut expr.data else {
            unreachable!()
        };

        let abs_type = typ.clone();
        let mut symbols = VecDeque::new();

        while let MonoType::Arrow(current, next) = &*typ.clone().flatten() {
            *typ = next.clone();

            let symbol = Symbol(format!("_gen{}", symbols.len() + 1));
            symbols.push_front((symbol.clone(), current.clone()));

            args.push(Expr::Variable(VariableNode::new(symbol)).with_loc(loc));
        }

        if symbols.is_empty() {
            return;
        }

        let application = std::mem::take(&mut expr.data);
        expr.data = Expr::Abstraction(symbols, Box::new(application.with_loc(loc)), abs_type);
    }
}

pub struct ClosureMoveChecker(Program, ByteRange);

impl ClosureMoveChecker {
    pub fn new(prog: Program) -> Self {
        Self(prog, ByteRange::default())
    }
}

impl ErrorBuilder for ClosureMoveChecker {
    fn push_error(&self, error: Error) {
        self.0.borrow_mut().errors.push(error);
    }

    fn location(&self) -> ByteRange {
        self.1
    }
}

impl Visitor for ClosureMoveChecker {
    fn visit_abstraction(&mut self, abs: &mut Elaborated<Type>) {
        let Expr::Abstraction(args, expr, _) = &mut abs.data else {
            unreachable!()
        };

        let mut visitor = CollectLocalVars::default();
        expr.walk(&mut visitor);

        for symbol in args.iter().map(|(s, _)| s) {
            visitor.0.remove(symbol);
        }

        if !visitor.0.is_empty() {
            abs.data = Expr::Error;

            for (free, loc) in visitor.0 {
                self.1 = loc;
                self.error(format!(
                    "free variables can not move into closures yet: '{}'",
                    free
                ));
            }
        }
    }
}

#[derive(Default)]
pub struct CollectLocalVars(IndexMap<Symbol, ByteRange>);

impl Visitor for CollectLocalVars {
    fn visit_variable(&mut self, var: &mut VariableNode<Type>, loc: ByteRange) {
        self.0.insert(var.name.clone(), loc);
    }

    fn visit_case_tree(&mut self, case_tree: &mut CaseTree<Type>) {
        let CaseTree {
            accessors,
            places,
            tree,
        } = case_tree;

        for (accessor, place) in accessors.iter_mut().zip(places.iter_mut()) {
            let mut visitor = Self::default();
            place.walk(&mut visitor);

            for symbol in accessor.keys().map(|s| Symbol(s.to_string())) {
                visitor.0.remove(&symbol);
            }

            self.0.extend(visitor.0);
        }

        tree.walk(self);
    }
}
