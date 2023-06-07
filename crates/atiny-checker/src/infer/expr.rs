//! Type inference for for expressions.

use super::Infer;
use crate::check::Check;
use crate::exhaustive::{CaseTree, Problem, Witness};
use crate::{context::Ctx, types::*, unify::unify};

use atiny_tree::r#abstract::*;
use itertools::Itertools;
use std::{collections::HashSet, rc::Rc};

impl Infer<'_> for Expr {
    type Context = Ctx;

    fn infer(self, mut ctx: Self::Context) -> Type {
        use AtomKind::*;
        use ExprKind::*;
        ctx = ctx.set_position(self.location);

        match self.data {
            Atom(a) => match a {
                Number(_) => MonoType::typ("Int".to_string()),

                Tuple(vec) => Rc::new(MonoType::Tuple(
                    vec.into_iter()
                        .map(|expr| expr.infer(ctx.clone()))
                        .collect(),
                )),

                Identifier(x) => match ctx.lookup(&x) {
                    Some(sigma) => sigma.instantiate(ctx),
                    None => ctx.new_error(format!("unbound variable '{}'", x)),
                },

                Group(expr) => expr.infer(ctx),
            },

            Application(e0, e1) => {
                let t0 = e0.infer(ctx.clone());
                let t1 = e1.infer(ctx.clone());

                let t_return = ctx.new_hole();
                let function_type = MonoType::arrow(t1, t_return.clone());

                unify(ctx, t0, function_type);

                t_return
            }

            Abstraction(x, e) => {
                let t = ctx.new_hole();
                let new_ctx = ctx.extend(x, t.to_poly());
                let t_line = e.infer(new_ctx);
                MonoType::arrow(t, t_line)
            }

            Let(x, e0, e1) => {
                let lvl_ctx = ctx.level_up();
                let t = e0.infer(lvl_ctx);
                let t_generalized = t.generalize(ctx.clone());

                let new_ctx = ctx.extend(x, t_generalized);

                e1.infer(new_ctx)
            }

            Match(e, clauses) => {
                let err_count = ctx.err_count();

                let pat_ty = e.infer(ctx.clone());
                let ret_ty = ctx.new_hole();

                let columns = clauses.iter().map(|x| x.pat.clone()).collect_vec();

                for c in clauses {
                    let mut set = HashSet::new();
                    let pat_loc = c.pat.location;
                    let clause_pat = c.pat.infer((&mut ctx, &mut set));
                    ctx = ctx.set_position(pat_loc);
                    unify(ctx.clone(), pat_ty.clone(), clause_pat);
                    unify(ctx.clone(), ret_ty.clone(), c.expr.infer(ctx.clone()));
                }

                // We can't do coverage checking / exhaustiveness checking without a well typed
                // pattern match and with linear variables.
                if ctx.err_count() == err_count {
                    let problem = Problem::new(pat_ty, vec![wildcard()], columns);
                    let witness = problem.exhaustiveness(&mut ctx);

                    let _ = report_exhaustiveness(witness).map_err(|err| {
                        ctx.new_error(format!("non-exhaustive pattern match: {}", err))
                    });
                }

                ret_ty
            }

            Annotation(expr, typ) => {
                let typ_res = typ.infer(ctx.clone());
                expr.check(ctx, typ_res.clone());
                typ_res
            }
        }
    }
}

fn report_exhaustiveness(witness: Witness) -> Result<CaseTree, Pattern> {
    match witness {
        Witness::NonExhaustive(result) => Err(result.into_pattern()),
        Witness::Ok(res) => Ok(res),
    }
}
