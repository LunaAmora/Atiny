//! Type checking for expressions.

use super::Check;
use crate::infer::Infer;
use crate::{context::Ctx, types::*, unify::unify};

use atiny_tree::r#abstract::{Expr, ExprKind};

impl Check<'_> for Expr {
    type Context = Ctx;

    fn check(self, ctx: Self::Context, expected: Type) {
        let ctx = ctx.set_position(self.location);

        match (self.data, &*expected) {
            (ExprKind::Abstraction(param, body), MonoType::Arrow(left, right)) => {
                let new_ctx = ctx.extend(param, left.to_poly());
                body.check(new_ctx, right.clone());
            }

            (data, _) => {
                let located = Self::new(self.location, data);
                let infer = located.infer(ctx.clone());
                unify(ctx, infer, expected);
            }
        }
    }
}
