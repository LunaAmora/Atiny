use crate::{check::Check, context::Ctx, infer::Infer, types::*};
use atiny_tree::{elaborated::FnBody, r#abstract::*};
use std::{collections::HashSet, rc::Rc};

impl<'a> Infer<'a> for Vec<TopLevel> {
    type Context = &'a mut Ctx;
    type Return = Vec<FnBody<Type>>;

    fn infer(self, ctx: Self::Context) -> Self::Return {
        let mut fn_vec = Vec::new();
        let top_iter = TopIter::new(self, &mut fn_vec);

        let constr_list: Vec<_> = top_iter.map(|type_decl| type_decl.infer(ctx)).collect();

        for (decl_name, constructors) in constr_list {
            for constructor in constructors {
                (decl_name.as_str(), constructor).infer(ctx);
            }
        }

        let mut bodies = Vec::new();

        fn_vec
            .into_iter()
            .filter_map(|fun_decl| fun_decl.infer(ctx))
            .for_each(|body| bodies.push(body));

        bodies.into_iter().map(|body| body.infer(ctx)).collect()
    }
}

impl<'a> Infer<'a> for TypeDecl {
    type Context = &'a mut Ctx;
    type Return = (String, Vec<Constructor>);

    fn infer(self, ctx: Self::Context) -> Self::Return {
        let type_sig = TypeSignature {
            name: self.name.clone(),
            params: self.params.clone(),
            value: TypeValue::Sum(Vec::new()),
        };

        ctx.signatures.types.insert(self.name.clone(), type_sig);

        (self.name, self.constructors)
    }
}

impl<'a> Infer<'a> for (&str, Constructor) {
    type Context = &'a mut Ctx;
    type Return = ();

    fn infer(self, ctx: Self::Context) -> Self::Return {
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

impl<'a> Infer<'a> for FnDecl {
    type Context = &'a mut Ctx;
    type Return = Option<(String, Expr)>;

    fn infer(self, ctx: Self::Context) -> Self::Return {
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

        self.body.map(|body| (self.name, body))
    }
}

impl<'a> Infer<'a> for (String, Expr) {
    type Context = &'a Ctx;
    type Return = FnBody<Type>;

    fn infer(self, ctx: Self::Context) -> Self::Return {
        let (fn_name, body) = self;

        let Some(DeclSignature::Function(sig)) = ctx.signatures.values.get(&fn_name) else {
            panic!("The String should be a valid function signature name on the Ctx");
        };

        let mut new_ctx = ctx.extend_types(sig.type_variables());

        for (arg, arg_type) in &sig.args {
            new_ctx.map.insert(arg.to_string(), arg_type.to_poly());
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
