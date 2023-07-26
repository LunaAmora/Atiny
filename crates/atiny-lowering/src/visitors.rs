use atiny_checker::types::{MonoType, Type};
use atiny_tree::elaborated::{CaseTree, CaseTreeNode, Expr, Symbol, VariableNode};
use std::collections::VecDeque;

pub trait Visitor<T = Type> {
    fn visit_symbol(&mut self, _: &mut Symbol) {}
    fn visit_number(&mut self, _: &mut u64) {}
    fn visit_type(&mut self, _: &mut T) {}

    fn visit_variable(&mut self, _: &mut VariableNode<T>) {}

    fn visit_application(&mut self, _: &mut Expr<T>) {}
    fn visit_abstraction(&mut self, _: &mut Expr<T>) {}

    fn visit_case_tree(&mut self, _: &mut CaseTree<T>) {}
    fn visit_case_tree_node(&mut self, _: &mut CaseTreeNode) {}
}

pub struct PartialAppRemover;

impl Visitor for PartialAppRemover {
    fn visit_application(&mut self, expr: &mut Expr<Type>) {
        let Expr::Application(_, args, typ) = expr else {
            unreachable!()
        };

        let abstraction_type = typ.clone();
        let mut symbols = VecDeque::new();

        while let MonoType::Arrow(current, next) = &*typ.clone().flatten() {
            *typ = next.clone();

            let symbol = Symbol(format!("_gen{}", symbols.len() + 1));
            symbols.push_front((symbol.clone(), current.clone()));

            args.push(Expr::Variable(VariableNode::new(symbol)));
        }
        
        if symbols.is_empty() {
            return;
        }

        let application = std::mem::take(expr);
        *expr = Expr::Abstraction(symbols, Box::new(application), abstraction_type);
    }
}
