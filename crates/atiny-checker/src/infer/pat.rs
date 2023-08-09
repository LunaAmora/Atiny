use std::collections::HashMap;

use super::Infer;
use crate::context::{Ctx, InferError, VariableKind};
use crate::exhaustive::{Problem, Witness};
use crate::types::{MonoType, Type};
use crate::unify::unify;

use atiny_location::{Located, WithLoc};
use atiny_tree::elaborated::{Accessor, AccessorExt, Symbol};
use atiny_tree::r#abstract::{wildcard, AtomKind, Path, Pattern, PatternKind};

impl Infer for Pattern {
    type Context<'a> = (
        &'a mut Ctx,
        Vec<Accessor<Type>>,
        &'a mut HashMap<String, Vec<Accessor<Type>>>,
    );
    type Return = Type;

    fn infer(self, (ctx, vec, set): Self::Context<'_>) -> Self::Return {
        use AtomKind::*;
        ctx.set_position(self.location);
        let id = ctx.id;

        match self.data {
            PatternKind::Atom(a) => match a {
                Wildcard => ctx.new_hole(),

                Number(_) => Type::typ("Int".to_string(), id),

                Identifier(x) if x.starts_with(|c: char| c.is_ascii_uppercase()) => {
                    if ctx.lookup_cons(&x).is_some() {
                        PatternKind::Constructor(x, vec![])
                            .with_loc(self.location)
                            .infer((ctx, vec, set))
                    } else {
                        ctx.new_error(format!("unbound constructor: {}", x))
                    }
                }

                Identifier(x) if ctx.lookup_cons(&x).is_some() => {
                    PatternKind::Constructor(x, vec![])
                        .with_loc(self.location)
                        .infer((ctx, vec, set))
                }

                Identifier(x) => {
                    if set.insert(x.to_owned(), vec).is_none() {
                        let hole = ctx.new_hole();
                        ctx.map.insert(x, (VariableKind::Local, hole.to_poly()));
                        hole
                    } else {
                        ctx.new_error(format!("identifier '{}' bound more than once", x))
                    }
                }

                Tuple(parts) => {
                    let hole = ctx.new_hole();

                    let res = Type::new(
                        MonoType::Tuple(
                            parts
                                .into_iter()
                                .enumerate()
                                .map(|(index, pat)| {
                                    let with_index = vec.clone().with_index(hole.clone(), index);
                                    pat.infer((ctx, with_index, set))
                                })
                                .collect(),
                        ),
                        id,
                    );

                    unify(ctx.clone(), res.clone(), hole);
                    res
                }

                PathItem(ref path @ Path(_, Located { location, ref data })) => ctx
                    .ctx_from_path(path, |ctx| {
                        PatternKind::Atom(Identifier(data.clone()))
                            .with_loc(location)
                            .infer((ctx, vec.clone(), set))
                    })
                    .unwrap_or_else(|| ctx.infer_error()),
            },

            PatternKind::Constructor(name, args) => {
                let hole = ctx.new_hole();

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

                for (index, pat) in args.into_iter().enumerate() {
                    let vec = vec
                        .clone()
                        .with_field(hole.clone(), Symbol(name.clone()), index);

                    let pat_ty = pat.infer((ctx, vec, set));

                    let MonoType::Arrow(left, right) = &*typ else {
                        unreachable!("impossible branch when matching constructor arguments")
                    };

                    unify(ctx.clone(), pat_ty.clone(), left.clone());
                    typ = right.clone();
                }

                unify(ctx.clone(), typ.clone(), hole);
                typ
            }
        }
    }
}

impl Ctx {
    pub fn single_exhaustiveness(&mut self, pattern: &Pattern, pattern_type: Type) -> Witness {
        let mut set = HashMap::new();
        let cons_pat = pattern.clone().infer((self, vec![], &mut set));

        unify(self.clone(), cons_pat.clone(), pattern_type.clone());
        self.extend_with_pattern(pattern, pattern_type);

        let problem = Problem::new(cons_pat, vec![wildcard()], vec![pattern.clone()]);
        problem.exhaustiveness(self)
    }

    pub fn extend_with_pattern(&mut self, pattern: &Pattern, pattern_type: Type) {
        match (&pattern.data, &*pattern_type) {
            (PatternKind::Atom(AtomKind::Wildcard), _) => {}

            (PatternKind::Atom(atom), _) => {
                self.map.insert(
                    atom.to_string(),
                    (VariableKind::Local, pattern_type.to_poly()),
                );
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
