use super::Infer;
use crate::{
    context::Ctx,
    types::{MonoType, Type},
    unify::unify,
};

use atiny_tree::r#abstract::{AtomKind, Pattern, PatternKind};
use std::{collections::HashSet, rc::Rc};

impl<'a> Infer<'a> for Pattern {
    type Context = (&'a mut Ctx, &'a mut HashSet<String>);

    fn infer(self, (ctx, set): Self::Context) -> Type {
        use AtomKind::*;
        *ctx = ctx.set_position(self.location);

        match self.data {
            PatternKind::Atom(a) => match a {
                Unit => MonoType::var("()".to_string()),

                Number(_) => MonoType::var("Int".to_string()),

                Boolean(_) => MonoType::var("Bool".to_string()),

                Identifier(x) if set.insert(x.to_owned()) => {
                    let hole = ctx.new_hole();
                    *ctx = ctx.extend(x, hole.to_poly());
                    hole
                }

                Identifier(x) => ctx.new_error(format!("identifier '{}' bound more than once", x)),

                Tuple(vec) => Rc::new(MonoType::Tuple(
                    vec.into_iter().map(|pat| pat.infer((ctx, set))).collect(),
                )),

                Group(expr) => expr.infer((ctx, set)),
            },

            PatternKind::Constructor(name, args) => {
                let Some(constructor) = ctx.lookup_constructor(&name) else {
                    return ctx.new_error(format!("unbound constructor '{}'", name));
                };

                if constructor.args.len() != args.len() {
                    return ctx.new_error(format!(
                        "constructor '{}' expects {} arguments, but got {}",
                        name,
                        constructor.args.len(),
                        args.len()
                    ));
                }

                let mut typ = constructor.typ.instantiate(ctx.clone());

                for pat in args {
                    let pat_ty = pat.infer((ctx, set));

                    let result = if let MonoType::Arrow(left, right) = &*typ {
                        unify(ctx.clone(), pat_ty.clone(), left.clone());
                        right.clone()
                    } else {
                        unreachable!("impossible branch when matching constructor arguments")
                    };

                    typ = result;
                }

                typ
            }
        }
    }
}
