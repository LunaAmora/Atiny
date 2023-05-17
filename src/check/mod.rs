use std::{collections::HashSet, rc::Rc};

use self::{
    context::Ctx,
    types::{Hole, MonoType, Ref, TypeScheme},
};

pub mod context;
pub mod types;
pub mod util;

use crate::{
    error::Error,
    syntax::tree::{AtomKind, Expr, ExprKind, Pattern, PatternKind, Type, TypeKind},
};

pub fn unify(ctx: Ctx<'_>, left: Rc<MonoType>, right: Rc<MonoType>) -> Result<(), Error<'_>> {
    match (&*left, &*right) {
        (MonoType::Var(x), MonoType::Var(y)) if x == y => Ok(()),

        (MonoType::Arrow(l, r), MonoType::Arrow(l1, r1)) => {
            unify(ctx.clone(), l.clone(), l1.clone())?;
            unify(ctx, r.clone(), r1.clone())
        }

        (MonoType::Hole(ref_), _) => unify_hole(ctx, left.clone(), ref_, right, false),

        (_, MonoType::Hole(ref_)) => unify_hole(ctx, right.clone(), ref_, left, true),

        (MonoType::Tuple(vec_l), MonoType::Tuple(vec_r)) if vec_l.len() == vec_r.len() => {
            for (l, r) in vec_l.iter().zip(vec_r.iter()) {
                unify(ctx.clone(), l.clone(), r.clone())?;
            }
            Ok(())
        }

        (l, r) => ctx.error(format!("type mismatch between '{}' and '{}'", l, r)),
    }
}

fn unify_hole<'a>(
    ctx: Ctx<'a>,
    typ: Rc<MonoType>,
    hole: &Ref,
    other: Rc<MonoType>,
    swap: bool,
) -> Result<(), Error<'a>> {
    match hole.get() {
        Hole::Empty(u) => {
            match occur_check(hole, u, other.clone()) {
                Ok(()) => {}
                Err(UnifyError::CantUnify) => return Ok(()),
                Err(UnifyError::Cyclic) => {
                    return ctx.error("found cyclic type of infinite size".to_owned())
                }
            }

            if let MonoType::Hole(hole_) = &&*other {
                match hole_.get() {
                    Hole::Empty(l) if u < l => hole_.fill(typ),
                    _ => hole.fill(other),
                }
            } else {
                hole.fill(other);
            }
        }
        Hole::Filled(filled) if swap => return unify(ctx, other, filled),
        Hole::Filled(filled) => return unify(ctx, filled, other),
    }
    Ok(())
}

enum UnifyError {
    CantUnify,
    Cyclic,
}

fn occur_check(hole: &Ref, lvl: usize, other: Rc<MonoType>) -> Result<(), UnifyError> {
    match &*other {
        MonoType::Hole(other) => {
            if hole == other {
                return Err(UnifyError::CantUnify);
            }

            match other.get() {
                Hole::Empty(lvl2) if lvl2 > lvl => {
                    hole.get_item().data = Hole::Empty(lvl);
                }
                Hole::Filled(t) => occur_check(hole, lvl, t)?,
                _ => (),
            }
        }

        MonoType::Tuple(vec) => {
            for mono in vec {
                occur_check(hole, lvl, mono.clone()).map_err(|e| match e {
                    UnifyError::CantUnify => UnifyError::Cyclic,
                    UnifyError::Cyclic => UnifyError::Cyclic,
                })?;
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
                let t = e0.infer(ctx.level_up())?;
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
                    unify(ctx.clone(), pat_ty.clone(), clause_pat)?;
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
