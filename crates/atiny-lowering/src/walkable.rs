use atiny_tree::elaborated::{CaseTreeNode, Expr, Stmt};

use crate::visitors::Visitor;

pub trait Walkable<T> {
    fn walk<V: Visitor<T>>(&mut self, visitor: &mut V);
}

impl<T> Walkable<T> for Expr<T> {
    fn walk<V: Visitor<T>>(&mut self, visitor: &mut V) {
        match self {
            Self::Number(num) => visitor.visit_number(num),

            Self::Tuple(tuple) => tuple.iter_mut().for_each(|expr| expr.walk(visitor)),

            Self::Variable(var) => {
                visitor.visit_variable(var);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Self::Function(var) => {
                visitor.visit_function(var);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Self::Constructor(var) => {
                visitor.visit_constructor(var);
                visitor.visit_symbol(&mut var.name);

                for typ in var.inst_types.iter_mut() {
                    visitor.visit_type(typ);
                }
            }

            Self::CaseTree(expr, case_tree) => {
                expr.walk(visitor);

                visitor.visit_case_tree(case_tree);

                for place in case_tree.places.iter_mut() {
                    place.walk(visitor);
                }

                case_tree.tree.walk(visitor);
            }

            Self::Abstraction(..) => {
                visitor.visit_abstraction(self);

                if let Self::Abstraction(args, expr, typ) = self {
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

            Self::Application(..) => {
                visitor.visit_application(self);

                if let Self::Application(fun, args, typ) = self {
                    fun.walk(visitor);

                    for arg in args.iter_mut() {
                        arg.walk(visitor);
                    }

                    visitor.visit_type(typ);
                } else {
                    self.walk(visitor);
                }
            }

            Self::RecordCreation(sym, fields) => {
                visitor.visit_symbol(sym);
                for (field, expr) in fields.iter_mut() {
                    visitor.visit_symbol(field);
                    expr.walk(visitor);
                }
            }

            Self::RecordUpdate(expr, fields) => {
                expr.walk(visitor);
                for (field, expr) in fields.iter_mut() {
                    visitor.visit_symbol(field);
                    expr.walk(visitor);
                }
            }

            Self::RecordField(record, field, expr) => {
                visitor.visit_symbol(record);
                visitor.visit_symbol(field);
                expr.walk(visitor);
            }

            Self::Block(block) => {
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

            Self::Error => todo!(),
        }
    }
}

impl<T> Walkable<T> for CaseTreeNode {
    fn walk<V: Visitor<T>>(&mut self, visitor: &mut V) {
        visitor.visit_case_tree_node(self);

        if let Self::Node(v) = self {
            for (sym, case_tree) in v.iter_mut() {
                visitor.visit_symbol(sym);
                case_tree.walk(visitor);
            }
        }
    }
}
