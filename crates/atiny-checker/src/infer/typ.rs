//! Type inference for types on type annotations.

use super::Infer;
use crate::{context::Ctx, types::*};

use atiny_tree::r#abstract::{TypeKind, TypeNode};
use std::rc::Rc;

impl Infer<'_> for TypeNode {
    type Context = Ctx;

    fn infer(self, ctx: Self::Context) -> Type {
        let ctx = ctx.set_position(self.location);

        match self.data {
            TypeKind::Arrow(arrow) => {
                let left = arrow.left.infer(ctx.clone());
                let right = arrow.right.infer(ctx);
                MonoType::arrow(left, right)
            }

            TypeKind::Variable(v) if ctx.typ_map.contains(&v.name) => MonoType::var(v.name),

            TypeKind::Variable(v) => ctx.new_error(format!("unbound type variable '{}'", v.name)),

            TypeKind::Tuple(tuple) => Rc::new(MonoType::Tuple(
                tuple
                    .types
                    .into_iter()
                    .map(|typ| typ.infer(ctx.clone()))
                    .collect(),
            )),

            // TODO: Error when try to infer a forall inside other types.
            TypeKind::Forall(forall) => TypeScheme {
                names: forall.args.clone(),
                mono: forall.body.infer(ctx.extend_types(&forall.args)),
            }
            .instantiate(ctx),

            TypeKind::Application(app) => match ctx.signatures.types.get(&app.fun) {
                Some(sig) if sig.params.len() == app.args.len() => Rc::new(MonoType::Application(
                    sig.name.clone(),
                    app.args
                        .into_iter()
                        .map(|typ| typ.infer(ctx.clone()))
                        .collect(),
                )),

                Some(sig) => ctx.new_error(format!(
                    "expected {} arguments but got {} in type",
                    sig.params.len(),
                    app.args.len()
                )),

                None => ctx.new_error(format!("unbound variable '{}'", app.fun)),
            },

            TypeKind::Unit => MonoType::var("()".to_string()),
        }
    }
}
