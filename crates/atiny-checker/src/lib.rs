#![feature(try_trait_v2)]

use std::{collections::HashSet, rc::Rc};

use atiny_error::Error;
use atiny_tree::r#abstract::*;

use self::{
    context::Ctx,
    types::{Hole, MonoType, Ref, TypeScheme},
    util::UnifyResult,
};

pub mod context;
pub mod types;
pub mod util;

pub fn unify(ctx: Ctx<'_>, left: Rc<MonoType>, right: Rc<MonoType>) -> Result<(), Error<'_>> {
    match (&*left, &*right) {
        (MonoType::Var(x), MonoType::Var(y)) if x == y => Ok(()),

        (MonoType::Arrow(l, r), MonoType::Arrow(l1, r1)) => {
            unify(ctx.clone(), l.clone(), l1.clone())?;
            unify(ctx, r.clone(), r1.clone())
        }

        (MonoType::Hole(l), MonoType::Hole(r)) if l == r => Ok(()),

        (MonoType::Hole(_), _) => unify_hole(ctx, left.clone(), right, false),

        (_, MonoType::Hole(_)) => unify_hole(ctx, right.clone(), left, true),

        (MonoType::Tuple(vec_l), MonoType::Tuple(vec_r)) if vec_l.len() == vec_r.len() => {
            for (l, r) in vec_l.iter().zip(vec_r.iter()) {
                unify(ctx.clone(), l.clone(), r.clone())?;
            }
            Ok(())
        }

        (l, r) => ctx.error(format!("type mismatch between '{}' and '{}'", l, r)),
    }
}

fn unify_hole(
    ctx: Ctx<'_>,
    hole: Rc<MonoType>,
    other: Rc<MonoType>,
    swap: bool,
) -> Result<(), Error<'_>> {
    let MonoType::Hole(ref_) = &*hole else {
        panic!("Hole should be a `MonoType::Hole`");
    };

    match ref_.get() {
        Hole::Empty(lvl) => {
            match occur_check(ref_, other.clone()) {
                UnifyResult::Ok => {}
                UnifyResult::CantUnify => return Ok(()),
                UnifyResult::Cyclic => {
                    return ctx.error("found cyclic type of infinite size".to_owned())
                }
            }

            if let MonoType::Hole(oth_ref) = &*other {
                match oth_ref.get() {
                    Hole::Empty(lvl2) if lvl2 > lvl => {
                        ref_.get_item().data = Hole::Empty(lvl);
                        oth_ref.fill(hole);
                    }
                    _ => ref_.fill(other),
                }
            } else {
                ref_.fill(other);
            }
        }
        Hole::Filled(filled) if swap => return unify(ctx, other, filled),
        Hole::Filled(filled) => return unify(ctx, filled, other),
    }
    Ok(())
}

fn occur_check(hole: &Ref, other: Rc<MonoType>) -> UnifyResult {
    match &*other {
        MonoType::Hole(other) => {
            if hole == other {
                return UnifyResult::CantUnify;
            }

            if let Hole::Filled(filled) = other.get() {
                occur_check(hole, filled)?;
            }
        }

        MonoType::Tuple(vec) => {
            for mono in vec.clone() {
                occur_check(hole, mono)?;
            }
        }

        MonoType::Arrow(l, r) => {
            occur_check(hole, l.clone())?;
            occur_check(hole, r.clone())?;
        }

        MonoType::Var(_) => {}
    }

    UnifyResult::Ok
}

pub trait Infer<'a, 'b> {
    type Return;
    type Context;
    fn infer(self, ctx: Self::Context) -> Self::Return;
}

impl<'a> Infer<'a, '_> for Expr {
    type Return = Result<Rc<MonoType>, Error<'a>>;
    type Context = Ctx<'a>;

    fn infer(self, ctx: Self::Context) -> Self::Return {
        use AtomKind::*;
        use ExprKind::*;
        let ctx = ctx.set_position(self.location);

        match self.data {
            Atom(a) => match a {
                Unit => Ok(MonoType::var("()".to_string())),

                Number(_) => Ok(MonoType::var("Int".to_string())),

                Boolean(_) => Ok(MonoType::var("Bool".to_string())),

                Tuple(vec) => Ok(MonoType::Tuple(
                    vec.into_iter()
                        .map(|e| e.infer(ctx.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                )
                .into()),

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
                let t = e0.infer(lvl_ctx.clone())?;
                let t_generalized = t.generalize(ctx.clone());

                let new_ctx = ctx.extend(x, t_generalized);

                e1.infer(new_ctx)
            }

            Match(e, clauses) => {
                let pat_ty = e.infer(ctx.clone())?;
                let ret_ty = ctx.new_hole();

                for c in clauses {
                    let mut set = HashSet::new();
                    let (clause_pat, new_ctx) = c.pat.infer((ctx.clone(), &mut set))?;
                    unify(new_ctx.clone(), pat_ty.clone(), clause_pat)?;
                    unify(new_ctx.clone(), ret_ty.clone(), c.expr.infer(new_ctx)?)?;
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

impl<'a, 'b> Infer<'a, 'b> for Pattern {
    type Return = Result<(Rc<MonoType>, Ctx<'b>), Error<'b>>;
    type Context = (Ctx<'b>, &'a mut HashSet<String>);

    fn infer(self, (ctx, set): Self::Context) -> Self::Return {
        use AtomKind::*;
        let ctx = ctx.set_position(self.location);

        match self.data {
            PatternKind::Atom(a) => match a {
                Unit => Ok((MonoType::var("()".to_string()), ctx)),

                Number(_) => Ok((MonoType::var("Int".to_string()), ctx)),

                Boolean(_) => Ok((MonoType::var("Bool".to_string()), ctx)),

                Identifier(x) => {
                    if !set.insert(x.to_owned()) {
                        return ctx.error(format!("identifier '{}' bound more than once", x));
                    }

                    match ctx.lookup(&x) {
                        Some(sigma) => Ok((sigma.instantiate(ctx.clone()), ctx)),
                        None => {
                            let t = ctx.new_hole();
                            let new_ctx = ctx.extend(x, t.to_poly());
                            Ok((t, new_ctx))
                        }
                    }
                }

                Tuple(vec) => {
                    let mut res = Vec::new();
                    let mut last_ctx = ctx.clone();

                    for e in vec {
                        let (i, c) = e.infer((last_ctx, set))?;
                        last_ctx = c;
                        res.push(i);
                    }

                    Ok((MonoType::Tuple(res).into(), last_ctx))
                }
            },
        }
    }
}

impl<'a> Infer<'a, '_> for Type {
    type Return = Result<Rc<MonoType>, Error<'a>>;
    type Context = Ctx<'a>;

    fn infer(self, ctx: Self::Context) -> Self::Return {
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

            TypeKind::Tuple(tuple) => {
                let mut typ = Vec::new();
                for el in tuple.types {
                    typ.push(el.infer(ctx.clone())?);
                }
                Ok(MonoType::Tuple(typ).into())
            }

            TypeKind::Forall(forall) => {
                let new_ctx = ctx.extend_types(&forall.args);
                let mono = forall.body.infer(new_ctx)?;

                let forall = TypeScheme {
                    names: forall.args,
                    mono,
                };

                Ok(forall.instantiate(ctx))
            }

            TypeKind::Application(_) => todo!(),

            TypeKind::Unit => todo!(),
        }
    }
}
