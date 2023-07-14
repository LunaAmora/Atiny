//! Type inference for types on type annotations.

use super::Infer;
use crate::{context::*, types::*};

use atiny_location::WithLoc;
use atiny_tree::r#abstract::{Path, TypeKind, TypeNode, VariableNode};

impl Infer for &TypeNode {
    type Context<'a> = Ctx;
    type Return = Type;

    fn infer(self, mut ctx: Self::Context<'_>) -> Self::Return {
        ctx.set_position(self.location);
        let id = ctx.id;

        match &self.data {
            TypeKind::Arrow(arrow) => {
                let left = arrow.left.infer(ctx.clone());
                let right = arrow.right.infer(ctx);
                left.arrow(right, id)
            }

            TypeKind::Variable(v) => {
                if ctx.lookup_type(&v.name).is_some() {
                    Type::typ(v.name.to_owned(), id)
                } else if ctx.typ_map.contains(&v.name) {
                    Type::var(v.name.to_owned(), id)
                } else {
                    ctx.new_error(format!("unbound type variable '{}'", v.name))
                }
            }

            TypeKind::Tuple(tuple) => Type::new(
                MonoType::Tuple(
                    tuple
                        .types
                        .iter()
                        .map(|typ| typ.infer(ctx.clone()))
                        .collect(),
                ),
                id,
            ),

            // TODO: Error when try to infer a forall inside other types.
            TypeKind::Forall(forall) => {
                TypeScheme {
                    names: forall.args.clone(),
                    mono: forall.body.infer(ctx.extend_types(&forall.args)),
                }
                .instantiate(ctx)
                .0
            }

            TypeKind::Application(app) => match ctx.lookup_type(&app.fun) {
                Some(sig) if sig.params.len() == app.args.len() => Type::new(
                    MonoType::Application(
                        sig.name,
                        app.args.iter().map(|typ| typ.infer(ctx.clone())).collect(),
                    ),
                    ctx.id,
                ),

                Some(sig) => ctx.new_error(format!(
                    "expected {} arguments but got {} in type",
                    sig.params.len(),
                    app.args.len()
                )),

                None => ctx.new_error(format!("unbound variable '{}'", app.fun)),
            },

            TypeKind::Path(Path(q, item)) => {
                let file = ctx.get_file_from_qualifier(q.clone());
                ctx.program.return_ctx(ctx.clone());
                ctx.program
                    .clone()
                    .get_infered_module::<Vec<_>, _>(file, |ctx| {
                        TypeKind::Variable(VariableNode {
                            name: item.data.clone(),
                        })
                        .with_loc(self)
                        .infer(ctx.clone())
                    })
            }
        }
    }
}
