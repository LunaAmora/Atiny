use crate::{check::Check, context::Ctx, exhaustive::Problem, infer::Infer, types::*};
use atiny_tree::{elaborated::FnBody, r#abstract::*};
use itertools::Itertools;
use std::{collections::HashSet, iter, rc::Rc};

impl Ctx {
    pub fn extend_type_sigs(&mut self, iter: impl IntoIterator<Item = TypeDecl>) {
        iter.into_iter()
            .map(|type_decl| type_decl.infer(self))
            .collect_vec()
            .into_iter()
            .for_each(|constr| constr.infer(self));
    }

    pub fn extend_fun_sigs(&mut self, iter: impl IntoIterator<Item = FnDecl>) -> Vec<FnBody<Type>> {
        iter.into_iter()
            .map(|fun_decl| fun_decl.infer(self))
            .collect_vec()
            .into_iter()
            .map(|body| body.infer(self))
            .collect()
    }
}

impl Infer for Vec<TopLevel> {
    type Context<'a> = &'a mut Ctx;
    type Return = Vec<FnBody<Type>>;

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let mut fn_vec = Vec::new();
        let top_iter = TopIter::new(self, &mut fn_vec);

        ctx.extend_type_sigs(top_iter);
        ctx.extend_fun_sigs(fn_vec)
    }
}

impl Infer for TypeDecl {
    type Context<'a> = &'a mut Ctx;
    type Return = (String, TypeDeclKind);

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let value = match self.constructors {
            TypeDeclKind::Sum(_) => TypeValue::Sum(Vec::new()),
            TypeDeclKind::Product(_) => TypeValue::Product(Vec::new()),
        };

        let type_sig = TypeSignature {
            name: self.name.clone(),
            params: self.params.clone(),
            value,
        };

        ctx.signatures.types.insert(self.name.clone(), type_sig);

        (self.name, self.constructors)
    }
}

impl Infer for (String, TypeDeclKind) {
    type Context<'a> = &'a mut Ctx;
    type Return = ();

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let (decl_name, kind) = self;
        let name = iter::repeat(decl_name.as_str());

        match kind {
            TypeDeclKind::Sum(constrs) => name.zip(constrs).for_each(|constr| constr.infer(ctx)),
            TypeDeclKind::Product(fields) => name.zip(fields).for_each(|field| field.infer(ctx)),
        }
    }
}

impl Infer for (&str, Field) {
    type Context<'a> = &'a mut Ctx;
    type Return = ();

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let (decl_name, field) = self;

        let Some(TypeSignature { params, .. }) = &ctx.signatures.types.get(decl_name) else {
            panic!("The String should be a valid type signature name on the Ctx");
        };

        let new_ctx = ctx.extend_types(params);

        let mono = field.ty.infer(new_ctx);

        let sig_value = &mut ctx.signatures.types.get_mut(decl_name).unwrap().value;

        if let TypeValue::Product(ref mut rec) = sig_value {
            rec.push((field.name.clone(), mono));
        }

        ctx.signatures
            .fields
            .entry(field.name)
            .or_default()
            .insert(decl_name.to_string());
    }
}

impl Infer for (&str, Constructor) {
    type Context<'a> = &'a mut Ctx;
    type Return = ();

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let (decl_name, constr) = self;

        let Some(TypeSignature { params, .. }) = &ctx.signatures.types.get(decl_name) else {
            panic!("The String should be a valid type signature name on the Ctx");
        };

        let application = Rc::new(MonoType::Application(
            decl_name.to_string(),
            params.iter().cloned().map(MonoType::var).collect(),
        ));

        let new_ctx = ctx.extend_types(params);

        let args: Vec<_> = constr
            .types
            .iter()
            .map(|t| t.infer(new_ctx.clone()))
            .collect();

        let value = Rc::new(ConstructorSignature::new(
            constr.name.clone(),
            params.clone(),
            MonoType::rfold_arrow(args.iter().cloned(), application),
            args,
        ));

        let sig_value = &mut ctx.signatures.types.get_mut(decl_name).unwrap().value;

        if let TypeValue::Sum(ref mut sum) = sig_value {
            sum.push(value.clone());
        }

        ctx.signatures
            .values
            .insert(constr.name, DeclSignature::Constructor(value));
    }
}

impl Infer for FnDecl {
    type Context<'a> = &'a mut Ctx;
    type Return = (String, Expr);

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let mut set = HashSet::new();

        for (_, ty) in &self.params {
            ctx.free_variables(ty, &mut set);
        }

        let new_ctx = ctx.extend_types(set.iter());

        let args: Vec<_> = self
            .params
            .iter()
            .map(|(a, p)| (a.to_owned(), p.infer(new_ctx.clone())))
            .collect();

        let ret = self.ret.infer(new_ctx);

        let entire_type = TypeScheme::new(
            set.into_iter().collect(),
            MonoType::rfold_arrow(args.iter().map(|(_, ty)| ty.clone()), ret),
        );

        let sig = DeclSignature::Function(FunctionSignature {
            name: self.name.clone(),
            args,
            entire_type,
        });

        ctx.signatures.values.insert(self.name.clone(), sig);

        (self.name, self.body)
    }
}

impl Infer for (String, Expr) {
    type Context<'a> = &'a Ctx;
    type Return = FnBody<Type>;

    fn infer(self, ctx: Self::Context<'_>) -> Self::Return {
        let (fn_name, body) = self;

        let Some(DeclSignature::Function(sig)) = ctx.signatures.values.get(&fn_name) else {
            panic!("The String should be a valid function signature name on the Ctx");
        };

        let mut new_ctx = ctx.extend_types(sig.type_variables());

        for (arg_pat, arg_type) in &sig.args {
            let mut set = HashSet::new();
            let cons_pat = arg_pat.clone().infer((&mut new_ctx, &mut set));

            let problem = Problem::new(cons_pat, vec![wildcard()], vec![arg_pat.clone()]);
            let witness = problem.exhaustiveness(ctx);

            let _ = witness.result().map_err(|err| {
                new_ctx.set_position(arg_pat.location);
                new_ctx.error(format!(
                    "refutable pattern in function argument. pattern `{}` not covered",
                    err
                ));
            });

            new_ctx.insert_pattern_bind(arg_pat, arg_type.clone());
        }

        FnBody(body.check(new_ctx, sig.return_type()))
    }
}

impl Ctx {
    fn free_variables(&self, ty: &TypeNode, set: &mut HashSet<String>) {
        match &ty.data {
            TypeKind::Arrow(arrow) => {
                self.free_variables(&arrow.left, set);
                self.free_variables(&arrow.right, set);
            }
            TypeKind::Variable(v) => {
                if !self.typ_map.contains(&v.name) {
                    set.insert(v.name.clone());
                }
            }
            TypeKind::Forall(forall) => {
                self.free_variables(&forall.body, set);
                for arg in &forall.args {
                    set.remove(arg);
                }
            }
            TypeKind::Application(app) => {
                for arg in &app.args {
                    self.free_variables(arg, set);
                }
            }
            TypeKind::Tuple(tuple) => {
                for arg in &tuple.types {
                    self.free_variables(arg, set);
                }
            }
        }
    }

    fn insert_pattern_bind(&mut self, pattern: &Pattern, pattern_type: Type) {
        match (&pattern.data, &*pattern_type) {
            (PatternKind::Atom(atom), _) => {
                self.map.insert(atom.to_string(), pattern_type.to_poly());
            }

            (PatternKind::Constructor(_, patterns), MonoType::Application(_, apps)) => {
                for (arg, app) in patterns.iter().zip(apps.iter()) {
                    self.insert_pattern_bind(arg, app.clone());
                }
            }

            (_, MonoType::Error) => {}

            (_, _) => unreachable!(),
        }
    }
}

struct TopIter<'a> {
    vec: Vec<TopLevel>,
    fn_vec: &'a mut Vec<FnDecl>,
}

impl<'a> TopIter<'a> {
    fn new(vec: Vec<TopLevel>, fn_vec: &'a mut Vec<FnDecl>) -> Self {
        Self { vec, fn_vec }
    }
}

impl<'a> Iterator for TopIter<'a> {
    type Item = TypeDecl;

    fn next(&mut self) -> Option<Self::Item> {
        match self.vec.pop() {
            Some(top) => match top.data {
                TopLevelKind::TypeDecl(typ) => Some(typ),
                TopLevelKind::FnDecl(fnd) => {
                    self.fn_vec.push(fnd);
                    self.next()
                }
            },
            None => None,
        }
    }
}
