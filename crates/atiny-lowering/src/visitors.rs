use atiny_checker::types::{MonoType, Type};
use atiny_location::{ByteRange, WithLoc};
use atiny_tree::elaborated::{CaseTree, CaseTreeNode, Elaborated, Expr, Symbol, VariableNode};
use std::collections::VecDeque;

pub trait Visitor {
    fn visit_symbol(&mut self, _: &mut Symbol) {}
    fn visit_number(&mut self, _: &mut u64) {}
    fn visit_type(&mut self, _: &mut Type) {}

    fn visit_variable(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}
    fn visit_function(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}
    fn visit_constructor(&mut self, _: &mut VariableNode<Type>, _: ByteRange) {}

    fn visit_application(&mut self, _: &mut Elaborated<Type>) {}
    fn visit_abstraction(&mut self, _: &mut Elaborated<Type>) {}

    fn visit_case_tree(&mut self, _: &mut CaseTree<Type>) {}
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

            args.push(Expr::Variable(VariableNode::new(symbol)).loc(loc));
        }

        if symbols.is_empty() {
            return;
        }

        let application = std::mem::take(&mut expr.data);
        expr.data = Expr::Abstraction(symbols, Box::new(application.loc(loc)), abs_type);
    }
}
