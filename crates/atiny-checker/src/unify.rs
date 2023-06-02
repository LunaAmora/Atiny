//! This module exposes functions like [unify] and [occur_check] for unification and occur checking
//! that are useful for the checker to check if two types are equal. These functions produce side
//! effects.
use std::{fmt::Display, rc::Rc};

use atiny_error::Error;

use crate::{
    context::Ctx,
    types::{Hole, MonoType, Ref},
};

#[derive(Debug)]
pub struct OccursCheck;

impl Display for OccursCheck {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "found cyclic type of infinite size")
    }
}

/// Tries to find a general unifier for two types, it fails if these two types are not "equal".
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

/// This function unifies a hole with a type.
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

/// Checks if a hole occur inside a type. It can lead to infinite types so we must not continue
/// after that. It also changes the level of an empty hole to the minimum between the two holes in
/// order to fix the level to the current region.
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
