use std::{collections::HashSet, iter, rc::Rc};

use atiny_tree::r#abstract::{
    Constructor, Expr, FnDecl, TopLevel, TopLevelKind, Type, TypeDecl, TypeKind,
};

use crate::{
    context::Ctx,
    types::{ConstructorSignature, DeclSignature, FunctionSignature, MonoType, TypeSignature},
    Check, Infer,
};

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

impl Ctx {
    pub fn add_top_level_types(&mut self, top_levels: Vec<TopLevel>) {
        let mut fn_vec = Vec::new();
        let top_iter = TopIter::new(top_levels, &mut fn_vec);

        let constructors: Vec<_> = top_iter
            .map(|type_decl| self.add_type_decl(type_decl))
            .collect();

        for (decl_name, contructors) in constructors {
            for contructor in contructors {
                self.add_type_constr(&decl_name, contructor);
            }
        }

        let bodies: Vec<_> = fn_vec
            .into_iter()
            .filter_map(|fun_decl| self.add_fn_decl(fun_decl))
            .collect();

        for (fun, body) in bodies {
            self.check_fn_body(fun, body);
        }
    }

    fn add_type_decl(&mut self, type_decl: TypeDecl) -> (String, Vec<Constructor>) {
        let type_sig = TypeSignature {
            name: type_decl.name.clone(),
            params: type_decl.params.clone(),
            constructors: Vec::new(),
        };

        self.signatures
            .types
            .insert(type_decl.name.clone(), type_sig);

        (type_decl.name, type_decl.constructors)
    }

    fn add_type_constr(&mut self, decl_name: &str, constr: Constructor) {
        let mut ctx = self.clone();
        let sig = self.signatures.types.get_mut(decl_name).unwrap();
        ctx.typ_map.extend(&sig.params);

        let application = Rc::new(MonoType::Application(
            decl_name.to_string(),
            sig.params.iter().cloned().map(MonoType::var).collect(),
        ));

        let constructor_type = constr
            .types
            .into_iter()
            .map(|t| t.infer(ctx.clone()))
            .chain(iter::once(application))
            .reduce(MonoType::arrow)
            .unwrap();

        let value = Rc::new(ConstructorSignature::new(
            constr.name.clone(),
            sig.params.clone(),
            constructor_type,
        ));

        sig.constructors.push(value.clone());

        self.signatures
            .values
            .insert(constr.name, DeclSignature::Constructor(value));
    }

    fn add_fn_decl(&mut self, fn_decl: FnDecl) -> Option<(String, Expr)> {
        let mut set = HashSet::new();

        for (_, ty) in &fn_decl.params {
            self.free_variables(ty, &mut set);
        }

        let mut ctx = self.clone();
        ctx.typ_map.extend(set.iter());

        let args = fn_decl
            .params
            .into_iter()
            .map(|(a, p)| (a, p.infer(ctx.clone())))
            .collect();

        let ret = fn_decl.ret.infer(ctx);

        let sig = DeclSignature::Function(FunctionSignature {
            name: fn_decl.name.clone(),
            args,
            ret,
            type_variables: set.into_iter().collect(),
        });

        self.signatures.values.insert(fn_decl.name.clone(), sig);

        fn_decl.body.map(|body| (fn_decl.name, body))
    }

    fn free_variables(&self, ty: &Type, set: &mut HashSet<String>) {
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
            TypeKind::Unit => {}
        }
    }

    fn check_fn_body(&mut self, fn_name: String, body: Expr) {
        let mut ctx = self.clone();
        let DeclSignature::Function(sig) = self.signatures.values.get_mut(&fn_name).unwrap() else {
            unreachable!()
        };

        ctx.typ_map.extend(&sig.type_variables);

        for (arg, arg_type) in &sig.args {
            ctx.map.insert(arg.to_string(), arg_type.to_poly());
        }

        body.check(ctx, sig.ret.clone());
    }
}
