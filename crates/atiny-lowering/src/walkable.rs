use atiny_checker::{infer::top_level::FnBody, program::Program, types::Type};
use atiny_tree::elaborated::{CaseTreeNode, Elaborated, Expr, Stmt};

use crate::visitors::Visitor;

pub trait Walkable {
    fn walk<V: Visitor>(&mut self, visitor: &mut V);
}

impl Walkable for Elaborated<Type> {
    fn walk<V: Visitor>(&mut self, visitor: &mut V) {
        let loc = self.location;

        match &mut self.data {
            Expr::Number(num) => visitor.visit_number(num),

            Expr::Tuple(tuple) => tuple.iter_mut().for_each(|expr| expr.walk(visitor)),

            Expr::Variable(var) => {
                visitor.visit_variable(var, loc);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Expr::Function(var) => {
                visitor.visit_function(var, loc);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Expr::Constructor(var) => {
                visitor.visit_constructor(var, loc);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Expr::CaseTree(expr, case_tree) => {
                expr.walk(visitor);

                visitor.visit_case_tree(case_tree);

                for place in case_tree.places.iter_mut() {
                    place.walk(visitor);
                }

                case_tree.tree.walk(visitor);
            }

            Expr::Abstraction(..) => {
                visitor.visit_abstraction(self);

                if let Expr::Abstraction(args, expr, typ) = &mut self.data {
                    for (sym, typ) in args.iter_mut().rev() {
                        visitor.visit_symbol(sym);
                        visitor.visit_type(typ);
                    }

                    expr.walk(visitor);
                    visitor.visit_type(typ);
                } else {
                    self.walk(visitor);
                }
            }

            Expr::Application(..) => {
                visitor.visit_application(self);

                if let Expr::Application(fun, args, typ) = &mut self.data {
                    fun.walk(visitor);

                    for arg in args.iter_mut() {
                        arg.walk(visitor);
                    }

                    visitor.visit_type(typ);
                } else {
                    self.walk(visitor);
                }
            }

            Expr::RecordCreation(sym, fields) => {
                visitor.visit_symbol(sym);
                for (field, expr) in fields.iter_mut() {
                    visitor.visit_symbol(field);
                    expr.walk(visitor);
                }
            }

            Expr::RecordUpdate(expr, fields) => {
                expr.walk(visitor);
                for (field, expr) in fields.iter_mut() {
                    visitor.visit_symbol(field);
                    expr.walk(visitor);
                }
            }

            Expr::RecordField(record, field, expr) => {
                visitor.visit_symbol(record);
                visitor.visit_symbol(field);
                expr.walk(visitor);
            }

            Expr::Block(block) => {
                for stmt in block.iter_mut() {
                    match stmt {
                        Stmt::Let(tree, expr) => {
                            tree.walk(visitor);
                            expr.walk(visitor);
                        }
                        Stmt::Expr(expr) => expr.walk(visitor),
                    }
                }
            }

            Expr::Error => {}
        }
    }
}

impl Walkable for CaseTreeNode {
    fn walk<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.visit_case_tree_node(self);

        if let Self::Node(v) = self {
            for s in v.iter_mut() {
                s.tree.walk(visitor);
            }
        }
    }
}

impl Walkable for Program {
    fn walk<V: Visitor>(&mut self, visitor: &mut V) {
        let mut elab = { std::mem::take(&mut self.borrow_mut().elaborated) };

        for (_, bodies) in elab.iter_mut() {
            for body in bodies {
                body.walk(visitor);
            }
        }

        self.borrow_mut().elaborated = elab;
    }
}

impl Walkable for FnBody {
    fn walk<V: Visitor>(&mut self, visitor: &mut V) {
        let Self(_, body) = self;
        body.walk(visitor);
    }
}
