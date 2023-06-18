//! Type inference for for expressions.

use super::Infer;
use crate::check::Check;
use crate::exhaustive::Problem;
use crate::{context::Ctx, types::*, unify::unify};

use atiny_tree::elaborated::{self, CaseTree, Symbol, VariableNode};
use atiny_tree::r#abstract::*;
use std::collections::HashSet;

type Elaborated = elaborated::Expr<Type>;

impl Infer<'_> for &Expr {
    type Context = Ctx;
    type Return = (Type, Elaborated);

    fn infer(self, mut ctx: Self::Context) -> Self::Return {
        use AtomKind::*;
        use ExprKind::*;
        ctx = ctx.set_position(self.location);

        match &self.data {
            Atom(a) => match a {
                Number(n) => (MonoType::typ("Int".to_string()), Elaborated::Number(*n)),

                Tuple(vec) => {
                    let (typ, el) = vec.iter().map(|expr| expr.infer(ctx.clone())).unzip();
                    (MonoType::tuple(typ), Elaborated::Tuple(el))
                }

                Identifier(x) => match ctx.lookup(x) {
                    Some(sigma) => {
                        let (inst, inst_types) = sigma.instantiate(ctx);

                        let variable_node = VariableNode {
                            inst_types,
                            name: Symbol(x.to_owned()),
                        };

                        (inst, Elaborated::Variable(variable_node))
                    }

                    None => (
                        ctx.new_error(format!("unbound variable '{}'", x)),
                        Elaborated::Error,
                    ),
                },

                Group(expr) => expr.infer(ctx),
            },

            Application(fun, arg) => {
                let (t0, elab_fun) = fun.infer(ctx.clone());
                let (t1, elab_arg) = arg.infer(ctx.clone());

                let t_return = ctx.new_hole();
                let function_type = MonoType::arrow(t1, t_return.clone());

                unify(ctx, t0, function_type);

                let appl = match elab_fun {
                    Elaborated::Application(fun, mut args, typ) => {
                        args.push(elab_arg);
                        Elaborated::Application(fun, args, typ)
                    }
                    _ => Elaborated::Application(
                        Box::new(elab_fun),
                        vec![elab_arg],
                        t_return.clone(),
                    ),
                };

                (t_return, appl)
            }

            Abstraction(param, body) => {
                let t = ctx.new_hole();
                let new_ctx = ctx.extend(param.to_owned(), t.to_poly());
                let (t_line, elab_body) = body.infer(new_ctx);

                let mut symbols = vec![Symbol(param.to_owned())];

                let abs = match elab_body {
                    Elaborated::Abstraction(param1, body1) => {
                        symbols.extend(param1);
                        Elaborated::Abstraction(symbols, body1)
                    }
                    _ => Elaborated::Abstraction(symbols, Box::new(elab_body)),
                };

                (MonoType::arrow(t, t_line), abs)
            }

            Let(x, e0, e1) => {
                let lvl_ctx = ctx.level_up();
                let (t, el0) = e0.infer(lvl_ctx);
                let t_generalized = t.generalize(ctx.clone());

                let new_ctx = ctx.extend(x.to_owned(), t_generalized);

                let (infered, el1) = e1.infer(new_ctx);
                let expr = Elaborated::Let(Symbol(x.to_owned()), Box::new(el0), Box::new(el1));
                (infered, expr)
            }

            Match(e, clauses) => {
                let err_count = ctx.err_count();

                let (pat_ty, scrutinee) = e.infer(ctx.clone());
                let ret_ty = ctx.new_hole();

                let mut places = Vec::new();

                for c in clauses {
                    let mut set = HashSet::new();
                    let pat_loc = c.pat.location;

                    let clause_pat = c.pat.clone().infer((&mut ctx, &mut set));
                    ctx = ctx.set_position(pat_loc);

                    unify(ctx.clone(), pat_ty.clone(), clause_pat);

                    let (right, elaborated) = c.expr.infer(ctx.clone());

                    unify(ctx.clone(), ret_ty.clone(), right);
                    places.push(elaborated);
                }

                // We can't do coverage checking / exhaustiveness checking without a well typed
                // pattern match and with linear variables.
                let elaborated = if err_count == ctx.err_count() {
                    let columns = if !clauses.is_empty() {
                        let mut collumns = vec![clauses[0].pat.clone()];

                        for c in &clauses[1..] {
                            let column_pat = c.pat.clone();
                            let column = vec![column_pat.clone()];
                            let problem = Problem::new(pat_ty.clone(), column, collumns.clone());

                            ctx = ctx.set_position(column_pat.location);
                            let witness = problem.exhaustiveness(&ctx);

                            if !witness.is_non_exhaustive() {
                                ctx.error(format!("the clause is useless: {}", column_pat));
                            }

                            collumns.push(column_pat.clone());
                        }

                        collumns
                    } else {
                        Vec::new()
                    };

                    let problem = Problem::new(pat_ty, vec![wildcard()], columns);
                    let witness = problem.exhaustiveness(&ctx);

                    witness.result().map_or_else(
                        |err| {
                            ctx.error(format!("non-exhaustive pattern match: {}", err));
                            Elaborated::Error
                        },
                        |tree| Elaborated::CaseTree(Box::new(scrutinee), CaseTree { tree, places }),
                    )
                } else {
                    Elaborated::Error
                };

                (ret_ty, elaborated)
            }

            Annotation(expr, typ) => {
                let typ_res = typ.infer(ctx.clone());
                let elab = expr.check(ctx, typ_res.clone());
                (typ_res, elab)
            }
        }
    }
}
