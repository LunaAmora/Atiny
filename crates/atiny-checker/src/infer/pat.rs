use super::Infer;
use crate::context::{Ctx, InferError};
use crate::exhaustive::{Problem, Witness};
use crate::types::{MonoType, Type};
use crate::unify::unify;

use atiny_location::WithLoc;
use atiny_tree::r#abstract::{wildcard, AtomKind, Path, Pattern, PatternKind};
use std::collections::HashSet;

impl Infer for Pattern {
    type Context<'a> = (&'a mut Ctx, &'a mut HashSet<String>);
    type Return = Type;

    fn infer(self, (ctx, set): Self::Context<'_>) -> Self::Return {
        use AtomKind::*;
        ctx.set_position(self.location);
        let id = ctx.id;

        match self.data {
            PatternKind::Atom(a) => match a {
                Wildcard => ctx.new_hole(),

                Number(_) => Type::typ("Int".to_string(), id),

                Identifier(x) if x.starts_with(|c: char| c.is_ascii_uppercase()) => {
                    if ctx.lookup_cons(&x).is_some() {
                        Self::new(self.location, PatternKind::Constructor(x, vec![]))
                            .infer((ctx, set))
                    } else {
                        ctx.new_error(format!("unbound constructor: {}", x))
                    }
                }

                Identifier(x) if ctx.lookup_cons(&x).is_some() => {
                    Self::new(self.location, PatternKind::Constructor(x, vec![])).infer((ctx, set))
                }

                Identifier(x) if set.insert(x.to_owned()) => {
                    let hole = ctx.new_hole();
                    ctx.map.insert(x, hole.to_poly());
                    hole
                }

                Identifier(x) => ctx.new_error(format!("identifier '{}' bound more than once", x)),

                Tuple(vec) => Type::new(
                    MonoType::Tuple(vec.into_iter().map(|pat| pat.infer((ctx, set))).collect()),
                    id,
                ),

                PathItem(Path(q, item)) => {
                    let file = ctx.get_file_from_qualifier(q);
                    ctx.program.return_ctx(ctx.clone());
                    ctx.program
                        .clone()
                        .get_infered_module::<Vec<_>, _>(file, |ctx| {
                            PatternKind::Atom(Identifier(item.data.clone()))
                                .with_loc(&item)
                                .infer((ctx, set))
                        })
                }
            },

            PatternKind::Constructor(name, args) => {
                let Some(constructor) = ctx.lookup_cons(&name) else {
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

                let (mut typ, _) = constructor.typ.instantiate(ctx.clone());

                for pat in args {
                    let pat_ty = pat.infer((ctx, set));

                    let MonoType::Arrow(left, right) = &*typ else {
                        unreachable!("impossible branch when matching constructor arguments")
                    };

                    unify(ctx.clone(), pat_ty.clone(), left.clone());
                    typ = right.clone();
                }

                typ
            }
        }
    }
}

impl Ctx {
    pub fn single_exhaustiveness(&mut self, pattern: &Pattern, pattern_type: Type) -> Witness {
        let mut set = HashSet::new();
        let cons_pat = pattern.clone().infer((self, &mut set));

        unify(self.clone(), cons_pat.clone(), pattern_type.clone());
        self.extend_with_pattern(pattern, pattern_type);

        let problem = Problem::new(cons_pat, vec![wildcard()], vec![pattern.clone()]);
        problem.exhaustiveness(self)
    }

    pub fn extend_with_pattern(&mut self, pattern: &Pattern, pattern_type: Type) {
        match (&pattern.data, &*pattern_type) {
            (PatternKind::Atom(AtomKind::Wildcard), _) => {}

            (PatternKind::Atom(atom), _) => {
                self.map.insert(atom.to_string(), pattern_type.to_poly());
            }

            (PatternKind::Constructor(_, patterns), MonoType::Application(_, apps)) => {
                for (arg, app) in patterns.iter().zip(apps.iter()) {
                    self.extend_with_pattern(arg, app.clone());
                }
            }

            (_, MonoType::Error) => {}

            (_, _) => unreachable!(),
        }
    }
}
