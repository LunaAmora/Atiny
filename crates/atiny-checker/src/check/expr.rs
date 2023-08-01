//! Type checking for expressions.

use super::Check;
use crate::infer::Infer;
use crate::{context::Ctx, types::*, unify::unify};

use atiny_tree::elaborated::Elaborated;
use atiny_tree::r#abstract::Expr;

impl Check<'_> for &Expr {
    type Context = Ctx;
    type Result = Elaborated<Type>;

    fn check(self, ctx: Self::Context, expected: Type) -> Self::Result {
        ctx.set_position(self.location);

        let (infer, elaborated) = self.infer(ctx.clone());
        unify(ctx, infer, expected);
        elaborated
    }
}
