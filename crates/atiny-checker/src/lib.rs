use std::{collections::HashSet, rc::Rc};

use atiny_error::Error;
use atiny_tree::r#abstract::*;

use self::{
    context::Ctx,
    types::{Hole, MonoType, Ref, TypeScheme},
    util::OccursCheck,
};

pub mod context;
pub mod types;
pub mod util;

pub fn unify(ctx: Ctx, left: Rc<MonoType>, right: Rc<MonoType>) -> Result<(), Error> {
    if Rc::ptr_eq(&left, &right) {
        return Ok(());
    }

    match (&*left, &*right) {
        (MonoType::Var(x), MonoType::Var(y)) if x == y => Ok(()),

        (MonoType::Arrow(l, r), MonoType::Arrow(l1, r1)) => {
            unify(ctx.clone(), l.clone(), l1.clone())?;
            unify(ctx, r.clone(), r1.clone())
        }

        (MonoType::Hole(l), MonoType::Hole(r)) if l == r => Ok(()),

        (MonoType::Hole(hole), _) => unify_hole(ctx, hole, right, false),

        (_, MonoType::Hole(hole)) => unify_hole(ctx, hole, left, true),

        (MonoType::Tuple(vec_l), MonoType::Tuple(vec_r)) if vec_l.len() == vec_r.len() => {
            for (l, r) in vec_l.iter().zip(vec_r.iter()) {
                unify(ctx.clone(), l.clone(), r.clone())?;
            }
            Ok(())
        }

        (l, r) => ctx.error(format!("type mismatch between '{}' and '{}'", l, r)),
    }
}

fn unify_hole(ctx: Ctx, hole: &Ref, other: Rc<MonoType>, swap: bool) -> Result<(), Error> {
    match hole.get() {
        Hole::Empty(lvl) => match occur_check(hole, lvl, other.clone()) {
            Err(occurs_check) => ctx.error(occurs_check.to_string()),
            Ok(_) => {
                hole.fill(other);
                Ok(())
            }
        },
        Hole::Filled(filled) if swap => unify(ctx, other, filled),
        Hole::Filled(filled) => unify(ctx, filled, other),
    }
}

fn occur_check(hole: &Ref, lvl: usize, other: Rc<MonoType>) -> Result<(), OccursCheck> {
    match &*other {
        MonoType::Hole(other_hole) if hole == other_hole => return Err(OccursCheck),

        MonoType::Hole(other_hole) => match other_hole.get() {
            Hole::Empty(lvl2) => {
                let min_level = usize::min(lvl, lvl2);
                other_hole.get_item_mut().data = Hole::Empty(min_level);
            }

            Hole::Filled(filled) => {
                occur_check(hole, lvl, filled)?;
            }
        },

        MonoType::Tuple(vec) => {
            for mono in vec.clone() {
                occur_check(hole, lvl, mono)?;
            }
        }

        MonoType::Arrow(l, r) => {
            occur_check(hole, lvl, l.clone())?;
            occur_check(hole, lvl, r.clone())?;
        }

        MonoType::Var(_) => {}
    }

    Ok(())
}

pub trait Infer<'a> {
    type Context;
    fn infer(self, ctx: Self::Context) -> Result<Rc<MonoType>, Error>;
}

impl Infer<'_> for Expr {
    type Context = Ctx;

    fn infer(self, mut ctx: Self::Context) -> Result<Rc<MonoType>, Error> {
        use AtomKind::*;
        use ExprKind::*;
        ctx = ctx.set_position(self.location);

        match self.data {
            Atom(a) => match a {
                Unit => Ok(MonoType::var("()".to_string())),

                Number(_) => Ok(MonoType::var("Int".to_string())),

                Boolean(_) => Ok(MonoType::var("Bool".to_string())),

                Tuple(vec) => vec
                    .into_iter()
                    .map(|expr| expr.infer(ctx.clone()))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|vec| Rc::new(MonoType::Tuple(vec))),

                Identifier(x) => match ctx.lookup(&x) {
                    Some(sigma) => Ok(sigma.instantiate(ctx)),
                    None => ctx.error(format!("unbound variable '{}'", x)),
                },
            },

            Application(e0, e1) => {
                let t0 = e0.infer(ctx.clone())?;
                let t1 = e1.infer(ctx.clone())?;

                let t_return = ctx.new_hole();
                let function_type = MonoType::arrow(t1, t_return.clone());

                unify(ctx, t0, function_type)?;

                Ok(t_return)
            }

            Abstraction(x, e) => {
                let t = ctx.new_hole();
                let new_ctx = ctx.extend(x, t.to_poly());
                let t_line = e.infer(new_ctx)?;
                Ok(MonoType::arrow(t, t_line))
            }

            Let(x, e0, e1) => {
                let lvl_ctx = ctx.level_up();
                let t = e0.infer(lvl_ctx)?;
                let t_generalized = t.generalize(ctx.clone());

                let new_ctx = ctx.extend(x, t_generalized);

                e1.infer(new_ctx)
            }

            Match(e, clauses) => {
                let pat_ty = e.infer(ctx.clone())?;
                let ret_ty = ctx.new_hole();

                for c in clauses {
                    let mut set = HashSet::new();
                    let clause_pat = c.pat.infer((&mut ctx, &mut set))?;
                    unify(ctx.clone(), pat_ty.clone(), clause_pat)?;
                    unify(ctx.clone(), ret_ty.clone(), c.expr.infer(ctx.clone())?)?;
                }

                Ok(ret_ty)
            }

            Annotation(expr, typ) => {
                let typ_res = typ.infer(ctx.clone())?;
                let expr_res = expr.infer(ctx.clone())?;

                unify(ctx, expr_res, typ_res.clone())?;

                Ok(typ_res)
            }
        }
    }
}

impl<'a> Infer<'a> for Pattern {
    type Context = (&'a mut Ctx, &'a mut HashSet<String>);

    fn infer(self, (mut ctx, set): Self::Context) -> Result<Rc<MonoType>, Error> {
        use AtomKind::*;
        *ctx = ctx.set_position(self.location);

        match self.data {
            PatternKind::Atom(a) => match a {
                Unit => Ok(MonoType::var("()".to_string())),

                Number(_) => Ok(MonoType::var("Int".to_string())),

                Boolean(_) => Ok(MonoType::var("Bool".to_string())),

                Identifier(x) => {
                    if !set.insert(x.to_owned()) {
                        return ctx.error(format!("identifier '{}' bound more than once", x));
                    }

                    let hole = ctx.new_hole();
                    *ctx = ctx.extend(x, hole.to_poly());
                    Ok(hole)
                }

                Tuple(vec) => vec
                    .into_iter()
                    .map(|pat| pat.infer((&mut ctx, set)))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|vec| Rc::new(MonoType::Tuple(vec))),
            },
        }
    }
}

impl Infer<'_> for Type {
    type Context = Ctx;

    fn infer(self, ctx: Self::Context) -> Result<Rc<MonoType>, Error> {
        let ctx = ctx.set_position(self.location);

        match self.data {
            TypeKind::Arrow(arrow) => {
                let left = arrow.left.infer(ctx.clone())?;
                let right = arrow.right.infer(ctx)?;
                Ok(MonoType::arrow(left, right))
            }

            TypeKind::Variable(v) => {
                if ctx.typ_map.contains(&v.name) {
                    Ok(MonoType::var(v.name))
                } else {
                    ctx.error(format!("unbound type variable '{}'", v.name))
                }
            }

            TypeKind::Tuple(tuple) => tuple
                .types
                .into_iter()
                .map(|typ| typ.infer(ctx.clone()))
                .collect::<Result<Vec<_>, _>>()
                .map(|vec| Rc::new(MonoType::Tuple(vec))),

            TypeKind::Forall(forall) => forall
                .body
                .infer(ctx.extend_types(&forall.args))
                .map(|mono| TypeScheme {
                    names: forall.args,
                    mono,
                })
                .map(|forall| forall.instantiate(ctx)),

            TypeKind::Application(_) => todo!(),

            TypeKind::Unit => todo!(),
        }
    }
}
