//! Type inference for types on type annotations.

use super::Infer;
use crate::{context::*, types::*};

use atiny_location::{Located, WithLoc};
use atiny_tree::r#abstract::{Path, TypeApplicationNode, TypeKind, TypeNode, VariableNode};

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

            TypeKind::Application(TypeApplicationNode { fun, args }) => {
                let Path(_, item) = fun;
                match ctx.path_map(fun, |ctx| ctx.lookup_type(&item.data)) {
                    Some(sig) if sig.params.len() == args.len() => Type::new(
                        MonoType::Application(
                            sig.name,
                            args.iter().map(|typ| typ.infer(ctx.clone())).collect(),
                        ),
                        ctx.id,
                    ),

                    Some(sig) => ctx.new_error(format!(
                        "expected {} arguments but got {} in type",
                        sig.params.len(),
                        args.len()
                    )),

                    None => ctx.new_error(format!("unbound variable '{}'", fun)),
                }
            }

            TypeKind::Path(path @ Path(_, Located { location, data })) => ctx
                .ctx_from_path(path, |ctx| {
                    TypeKind::Variable(VariableNode { name: data.clone() })
                        .loc(*location)
                        .infer(ctx.clone())
                })
                .unwrap_or_else(|| ctx.infer_error()),
        }
    }
}
