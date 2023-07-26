use atiny_checker::types::Type;
use atiny_tree::elaborated::{CaseTree, CaseTreeNode, Expr, Symbol, VariableNode};

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
