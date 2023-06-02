//! This module is useful for type checking [Expr] and [TopLevel] definitions using the
//! hindley-milner type system extended with type classes and linear types.

use std::{collections::HashSet, rc::Rc};

use atiny_error::Error;
use atiny_tree::r#abstract::*;

use self::{
    context::Ctx,
    types::{MonoType, TypeScheme},
};

pub mod context;
pub mod types;
pub mod unify;

type Result<T, U = Error> = std::result::Result<T, U>;

/// This trait exposes a function called [Infer::infer] that tries to discover a type for an
/// expression. A type rule that express this is:
///
/// ```md
///    'a = new_hole     G, x: 'a |- e => b'
/// ----------------------------------------
///         G |- |x| e => a' -> b'
/// ```
///
pub trait Infer<'a> {
    type Context;

    /// Infers the type of an expression.
    fn infer(self, ctx: Self::Context) -> Result<Rc<MonoType>, Error>;
}

/// This trait exposes a function called [Check::check] that tries to check an expression against a
/// known type only producing some side effects. An example of this type rule is
///
/// ```md
///       G, x: 'a |- e <== b'
/// -------------------------------
///     G |- |x| e <== a' -> b'
/// ```
///
pub trait Check<'a> {
    type Context;

    /// Checks an expression against a known type.
    fn check(self, ctx: Self::Context, typ: Rc<MonoType>) -> Result<(), Error>;
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
                Group(expr) => expr.infer(ctx),
            },

            Application(e0, e1) => {
                let t0 = e0.infer(ctx.clone())?;
                let t1 = e1.infer(ctx.clone())?;

                let t_return = ctx.new_hole();
                let function_type = MonoType::arrow(t1, t_return.clone());

                unify::unify(ctx, t0, function_type)?;

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
                    unify::unify(ctx.clone(), pat_ty.clone(), clause_pat)?;
                    unify::unify(ctx.clone(), ret_ty.clone(), c.expr.infer(ctx.clone())?)?;
                }

                Ok(ret_ty)
            }

            Annotation(expr, typ) => {
                let typ_res = typ.infer(ctx.clone())?;
                let expr_res = expr.infer(ctx.clone())?;

                unify::unify(ctx, expr_res, typ_res.clone())?;

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
                Group(expr) => expr.infer((ctx, set)),
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
