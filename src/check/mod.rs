use std::rc::Rc;

use self::{
    context::Ctx,
    types::{Hole, MonoType, Ref},
};

pub mod context;
pub mod types;
pub mod util;

use crate::{error::Error, syntax::Syntax};

pub fn unify(ctx: Ctx<'_>, left: Rc<MonoType>, right: Rc<MonoType>) -> Result<(), Error<'_>> {
    match (&*left, &*right) {
        (MonoType::Var(x), MonoType::Var(y)) if x == y => Ok(()),

        (MonoType::Arrow(l, r), MonoType::Arrow(l1, r1)) => {
            unify(ctx.clone(), l.clone(), l1.clone())?;
            unify(ctx, r.clone(), r1.clone())
        }

        (MonoType::Hole(ref_), _) => unify_hole(ctx, ref_, right, false),

        (_, MonoType::Hole(ref_)) => unify_hole(ctx, ref_, left, true),

        (l, r) => ctx.error(format!("type mismatch between '{}' and '{}'", l, r)),
    }
}

fn unify_hole<'a>(
    ctx: Ctx<'a>,
    hole: &Ref,
    other: Rc<MonoType>,
    swap: bool,
) -> Result<(), Error<'a>> {
    match hole.get() {
        Hole::Empty => hole.fill(other),
        Hole::Filled(filled) if swap => return unify(ctx, other, filled),
        Hole::Filled(filled) => return unify(ctx, filled, other),
    }
    Ok(())
}

pub fn infer<'a>(ctx: &Ctx<'a>, expr: Syntax) -> Result<Rc<MonoType>, Error<'a>> {
    use crate::syntax::Item::*;

    let ctx = ctx.set_position(expr.location);

    match expr.data {
        Number(_) => Ok(MonoType::var("Int".to_string())),

        Boolean(_) => Ok(MonoType::var("Bool".to_string())),

        Identifier(x) => match ctx.lookup(&x) {
            Some(sigma) => Ok(sigma.instantiate(ctx)),
            None => ctx.error(format!("unbound variable '{}'", x)),
        },

        Application(e0, e1) => {
            let t0 = infer(&ctx, *e0)?;
            let t1 = infer(&ctx, *e1)?;

            let t_return = ctx.new_hole();
            let function_type = MonoType::arrow(t1, t_return.clone());

            unify(ctx, t0, function_type)?;

            Ok(t_return)
        }

        Abstraction(x, e) => {
            let t = ctx.new_hole();
            let new_ctx = ctx.extend(x, t.to_poly());
            let t_line = infer(&new_ctx, *e)?;
            Ok(MonoType::arrow(t, t_line))
        }

        Let(x, e0, e1) => {
            let t = infer(&ctx, *e0)?;
            let t_generalized = t.generalize(ctx.clone());
            let new_ctx = ctx.extend(x, t_generalized);

            infer(&new_ctx, *e1)
        }
    }
}
